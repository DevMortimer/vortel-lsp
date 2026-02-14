;;; vortel-lsp.el --- Helix-inspired LSP mode for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors
;; Keywords: languages, tools
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; `vortel-lsp-mode' provides asynchronous LSP support with a custom
;; Helix-style transport/request layer.

;;; Code:

(require 'cl-lib)
(require 'thingatpt)
(require 'flymake)
(require 'subr-x)
(require 'xref)

(require 'vortel-lsp-client)
(require 'vortel-lsp-registry)
(require 'vortel-lsp-util)

(defcustom vortel-lsp-change-debounce 0.05
  "Seconds to debounce `textDocument/didChange` notifications."
  :type 'number
  :group 'vortel-lsp)

(defcustom vortel-lsp-enable-flymake t
  "When non-nil, integrate diagnostics with Flymake."
  :type 'boolean
  :group 'vortel-lsp)

(defcustom vortel-lsp-enable-capf t
  "When non-nil, integrate LSP completion via CAPF."
  :type 'boolean
  :group 'vortel-lsp)

(defcustom vortel-lsp-sync-request-timeout 2.0
  "Timeout in seconds used by synchronous bridge requests.
Used for xref and completion requests that must return immediately."
  :type 'number
  :group 'vortel-lsp)

(defcustom vortel-lsp-completion-timeout 0.8
  "Timeout in seconds for each completion request."
  :type 'number
  :group 'vortel-lsp)

(defcustom vortel-lsp-goto-reference-include-declaration t
  "When non-nil, include declarations in reference queries."
  :type 'boolean
  :group 'vortel-lsp)

(defvar vortel-lsp-mode nil)

(defvar-local vortel-lsp--attachments nil)
(defvar-local vortel-lsp--opened-clients nil)
(defvar-local vortel-lsp--document-version 0)
(defvar-local vortel-lsp--pending-changes nil)
(defvar-local vortel-lsp--before-change-ranges nil)
(defvar-local vortel-lsp--change-timer nil)
(defvar-local vortel-lsp--language nil)
(defvar-local vortel-lsp--flymake-report-fn nil)
(defvar-local vortel-lsp--diagnostics (make-hash-table :test #'equal))
(defvar-local vortel-lsp--completion-candidates nil)
(defvar-local vortel-lsp--completion-resolve-cache nil)

(defun vortel-lsp--buffer-uri ()
  "Return URI for current buffer file, or nil."
  (when-let* ((file buffer-file-name))
    (vortel-lsp-path-to-uri file)))

(defun vortel-lsp--with-live-buffer (buffer fn)
  "Call FN in BUFFER when it is still live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (funcall fn))))

(defun vortel-lsp--count-encoding-units (text encoding)
  "Count TEXT length in ENCODING units used by LSP positions."
  (pcase encoding
    ('utf-8 (string-bytes text))
    ('utf-16 (/ (string-bytes (encode-coding-string text 'utf-16-le)) 2))
    (_ (length text))))

(defun vortel-lsp--point-to-lsp-position (point encoding)
  "Convert POINT to LSP position hash-table using ENCODING."
  (save-excursion
    (goto-char point)
    (let* ((line (1- (line-number-at-pos point t)))
           (line-start (line-beginning-position))
           (segment (buffer-substring-no-properties line-start point))
           (character (vortel-lsp--count-encoding-units segment encoding)))
      (vortel-lsp-make-hash "line" line "character" character))))

(defun vortel-lsp--lsp-position-to-point (position encoding)
  "Convert LSP POSITION hash-table to point using ENCODING."
  (let ((line (or (vortel-lsp-hash-get position "line") 0))
        (character (or (vortel-lsp-hash-get position "character") 0)))
    (save-excursion
      (goto-char (point-min))
      (forward-line line)
      (let* ((line-start (point))
             (line-end (line-end-position))
             (point line-start)
             (units 0))
        (while (and (< point line-end)
                    (< units character))
          (let* ((next (1+ point))
                 (chunk (buffer-substring-no-properties point next))
                 (chunk-units (vortel-lsp--count-encoding-units chunk encoding)))
            (setq units (+ units chunk-units)
                  point next)))
        (min point line-end)))))

(defun vortel-lsp--server-entry-allows-feature-p (server-entry feature)
  "Return non-nil when SERVER-ENTRY allows FEATURE.
FEATURE is a Helix-style string such as `hover'.
SERVER-ENTRY is a catalog hash-table with `only' and `except' arrays."
  (let* ((only (or (vortel-lsp-hash-get server-entry "only") '()))
         (except (or (vortel-lsp-hash-get server-entry "except") '())))
    (and (or (null only) (member feature only))
         (not (member feature except)))))

(defun vortel-lsp--attachments-for-feature (feature)
  "Return active attachments that can serve FEATURE."
  (cl-remove-if-not
   (lambda (attachment)
     (let ((client (plist-get attachment :client))
           (entry (plist-get attachment :server-entry)))
       (and (vortel-lsp-client-ready-p client)
            (vortel-lsp--server-entry-allows-feature-p entry feature)
             (vortel-lsp-client-supports client feature))))
   vortel-lsp--attachments))

(defun vortel-lsp--request-sync (client method params &optional timeout)
  "Send METHOD request with PARAMS to CLIENT and wait for completion.
Return plist:
- `:ok' non-nil on success
- `:result' on success
- `:error' on failure"
  (let ((done nil)
        (result nil)
        (error nil)
        (timeout-seconds (or timeout vortel-lsp-sync-request-timeout))
        (process (and (vortel-lsp-client-live-p client)
                      (vortel-lsp-transport-process
                       (vortel-lsp-client-transport client)))))
    (if (not process)
        (list :ok nil :error (vortel-lsp-make-hash
                              "code" -32000
                              "message" "language server is not running"))
      (vortel-lsp-client-request
       client
       method
       params
       :timeout timeout-seconds
       :on-success (lambda (value)
                     (setq result value)
                     (setq done t))
       :on-error (lambda (err)
                   (setq error err)
                   (setq done t)))
      (let ((deadline (+ (float-time) timeout-seconds)))
        (while (and (not done)
                    (< (float-time) deadline)
                    (process-live-p process))
          (accept-process-output process 0.01)))
      (cond
       (done
        (if error
            (list :ok nil :error error)
          (list :ok t :result result)))
       ((not (process-live-p process))
        (list :ok nil :error (vortel-lsp-make-hash
                              "code" -32000
                              "message" "language server exited")))
       (t
        (list :ok nil :error (vortel-lsp-make-hash
                              "code" -32001
                              "message" (format "request timed out: %s" method))))))))

(defun vortel-lsp--normalize-location (item)
  "Normalize ITEM into a location plist with `:uri' and `:range'."
  (when (hash-table-p item)
    (let ((uri (vortel-lsp-hash-get item "uri"))
          (range (vortel-lsp-hash-get item "range"))
          (target-uri (vortel-lsp-hash-get item "targetUri"))
          (target-range (vortel-lsp-hash-get item "targetRange")))
      (cond
       ((and uri range)
        (list :uri uri :range range))
       ((and target-uri target-range)
        (list :uri target-uri :range target-range))
       (t nil)))))

(defun vortel-lsp--locations-from-response (response)
  "Extract a normalized location list from RESPONSE."
  (cond
   ((null response) nil)
   ((hash-table-p response)
    (let ((location (vortel-lsp--normalize-location response)))
      (if location (list location) nil)))
   ((listp response)
    (delq nil (mapcar #'vortel-lsp--normalize-location response)))
   (t nil)))

(defun vortel-lsp--location-start (location)
  "Return start position hash-table for LOCATION plist."
  (let* ((range (plist-get location :range)))
    (vortel-lsp-hash-get range "start")))

(defun vortel-lsp--location-end (location)
  "Return end position hash-table for LOCATION plist."
  (let* ((range (plist-get location :range)))
    (vortel-lsp-hash-get range "end")))

(defun vortel-lsp--location-file-position (path position encoding)
  "Convert LSP POSITION to file (line . column) in PATH using ENCODING."
  (with-current-buffer (find-file-noselect path t)
    (save-excursion
      (goto-char (vortel-lsp--lsp-position-to-point position encoding))
      (cons (line-number-at-pos) (current-column)))))

(defun vortel-lsp--location-summary (path position encoding)
  "Build one-line summary text at POSITION in PATH for xref display."
  (with-current-buffer (find-file-noselect path t)
    (save-excursion
      (goto-char (vortel-lsp--lsp-position-to-point position encoding))
      (string-trim (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))))

(defun vortel-lsp--location-to-xref (location encoding)
  "Convert LOCATION plist into an xref using ENCODING.
Return nil when the location does not map to a local file."
  (let* ((uri (plist-get location :uri))
         (path (vortel-lsp-uri-to-path uri))
         (start (vortel-lsp--location-start location)))
    (when (and path start)
      (let* ((line-column (vortel-lsp--location-file-position path start encoding))
             (line (car line-column))
             (column (cdr line-column))
             (summary (vortel-lsp--location-summary path start encoding)))
        (xref-make
         (if (string-empty-p summary)
             (file-name-nondirectory path)
           summary)
         (xref-make-file-location path line column))))))

(defun vortel-lsp--dedupe-xrefs (xrefs)
  "Return XREFS without duplicate locations."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (xref xrefs)
      (let* ((location (xref-item-location xref))
             (file (xref-file-location-file location))
             (line (xref-file-location-line location))
             (column (xref-file-location-column location))
             (key (format "%s:%s:%s" file line column)))
        (unless (gethash key seen)
          (puthash key t seen)
          (push xref result))))
    (nreverse result)))

(defun vortel-lsp--xref-request (feature method params-fn)
  "Collect xrefs by querying FEATURE with METHOD and PARAMS-FN.
PARAMS-FN is called with client and returns request params."
  (let ((attachments (vortel-lsp--attachments-for-feature feature))
        (xrefs nil))
    (dolist (attachment attachments)
      (let* ((client (plist-get attachment :client))
             (encoding (vortel-lsp-client-position-encoding client))
             (response
              (vortel-lsp--request-sync
               client
               method
               (funcall params-fn client)
               vortel-lsp-sync-request-timeout)))
        (when (plist-get response :ok)
          (dolist (location (vortel-lsp--locations-from-response
                             (plist-get response :result)))
            (when-let* ((xref (vortel-lsp--location-to-xref location encoding)))
              (push xref xrefs))))))
    (vortel-lsp--dedupe-xrefs (nreverse xrefs))))

(defun vortel-lsp--location-sort-key (location)
  "Return descending sort key for LOCATION plist text edits."
  (let* ((start (vortel-lsp--location-start location))
         (line (or (vortel-lsp-hash-get start "line") 0))
         (character (or (vortel-lsp-hash-get start "character") 0)))
    (cons line character)))

(defun vortel-lsp--text-edit-sort-desc (edit-a edit-b)
  "Sort predicate for text edits EDIT-A and EDIT-B in reverse order."
  (let* ((loc-a (list :range (vortel-lsp-hash-get edit-a "range")))
         (loc-b (list :range (vortel-lsp-hash-get edit-b "range")))
         (key-a (vortel-lsp--location-sort-key loc-a))
         (key-b (vortel-lsp--location-sort-key loc-b)))
    (or (> (car key-a) (car key-b))
        (and (= (car key-a) (car key-b))
             (> (cdr key-a) (cdr key-b))))))

(defun vortel-lsp--apply-text-edits-in-buffer (buffer edits encoding)
  "Apply LSP text EDITS in BUFFER using position ENCODING.
EDITS must be a list of TextEdit-compatible hash-tables."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (let ((inhibit-read-only t)
              (ordered (sort (copy-sequence edits) #'vortel-lsp--text-edit-sort-desc)))
          (dolist (edit ordered)
            (let* ((range (vortel-lsp-hash-get edit "range"))
                   (start (vortel-lsp-hash-get range "start"))
                   (end (vortel-lsp-hash-get range "end"))
                   (new-text (or (vortel-lsp-hash-get edit "newText") "")))
              (unless (and start end)
                (error "invalid text edit range"))
              (let ((beg (vortel-lsp--lsp-position-to-point start encoding))
                    (fin (vortel-lsp--lsp-position-to-point end encoding)))
                (delete-region beg fin)
                (goto-char beg)
                (insert new-text)))))))))

(defun vortel-lsp--apply-text-edits-to-path (path edits encoding)
  "Apply LSP text EDITS to PATH using ENCODING.
Return non-nil on success."
  (let* ((existing-buffer (get-file-buffer path))
         (buffer (or existing-buffer (find-file-noselect path)))
         (ephemeral (not existing-buffer)))
    (unwind-protect
        (progn
          (vortel-lsp--apply-text-edits-in-buffer buffer edits encoding)
          (when ephemeral
            (with-current-buffer buffer
              (save-buffer)))
          t)
      (when (and ephemeral
                 (buffer-live-p buffer)
                 (not (get-buffer-window buffer t)))
        (kill-buffer buffer)))))

(defun vortel-lsp--workspace-edit-dangerous-path-p (path)
  "Return non-nil when PATH points to a dangerous location."
  (let* ((expanded (expand-file-name path))
         (normalized (directory-file-name expanded)))
    (or (not (file-name-absolute-p expanded))
        (string-empty-p normalized)
        (equal normalized "/")
        (and (eq system-type 'windows-nt)
             (string-match-p "^[A-Za-z]:$" normalized)))))

(defun vortel-lsp--workspace-edit-uri-to-safe-path (uri context)
  "Convert URI to a safe local path for CONTEXT.
Return plist with keys:
- `:ok' non-nil on success
- `:path' normalized absolute path on success
- `:reason' human-readable failure reason on failure"
  (let ((path (vortel-lsp-uri-to-path uri)))
    (cond
     ((not path)
      (list :ok nil :reason (format "unsupported URI in %s: %s" context uri)))
     ((vortel-lsp--workspace-edit-dangerous-path-p path)
      (list :ok nil :reason (format "refusing unsafe path in %s: %s" context path)))
     (t
      (list :ok t :path (expand-file-name path))))))

(defun vortel-lsp--collect-workspace-edit-operations (workspace-edit)
  "Collect ordered operations from WORKSPACE-EDIT.
Return plist with keys:
- `:ok' non-nil when fully supported
- `:operations' ordered list of operation plists
- `:reason' failure reason when not supported"
  (let ((operations nil)
        (failure nil))
    (when-let* ((changes (vortel-lsp-hash-get workspace-edit "changes")))
      (maphash
       (lambda (uri edits)
         (unless failure
           (let ((resolved (vortel-lsp--workspace-edit-uri-to-safe-path
                            uri
                            "workspace edit changes")))
             (if (not (plist-get resolved :ok))
                 (setq failure (plist-get resolved :reason))
               (push (list :kind 'text-edits
                           :path (plist-get resolved :path)
                           :edits (if (listp edits) edits '()))
                     operations)))))
       changes))
    (when-let* ((document-changes (vortel-lsp-hash-get workspace-edit "documentChanges")))
      (dolist (entry document-changes)
        (unless failure
          (let ((kind (vortel-lsp-hash-get entry "kind")))
            (cond
             ((or (null kind) (string-empty-p (format "%s" kind)))
              (let* ((text-document (vortel-lsp-hash-get entry "textDocument"))
                     (uri (and text-document (vortel-lsp-hash-get text-document "uri")))
                     (edits (vortel-lsp-hash-get entry "edits"))
                     (resolved (vortel-lsp--workspace-edit-uri-to-safe-path
                                uri
                                "workspace edit documentChanges")))
                (if (not (plist-get resolved :ok))
                    (setq failure (plist-get resolved :reason))
                  (push (list :kind 'text-edits
                              :path (plist-get resolved :path)
                              :edits (if (listp edits) edits '()))
                        operations))))
             ((string= kind "create")
              (let* ((uri (vortel-lsp-hash-get entry "uri"))
                     (options (vortel-lsp-hash-get entry "options"))
                     (resolved (vortel-lsp--workspace-edit-uri-to-safe-path
                                uri
                                "workspace edit create")))
                (if (not (plist-get resolved :ok))
                    (setq failure (plist-get resolved :reason))
                  (push (list :kind 'create-file
                              :path (plist-get resolved :path)
                              :options options)
                        operations))))
             ((string= kind "rename")
              (let* ((old-uri (vortel-lsp-hash-get entry "oldUri"))
                     (new-uri (vortel-lsp-hash-get entry "newUri"))
                     (options (vortel-lsp-hash-get entry "options"))
                     (old-resolved (vortel-lsp--workspace-edit-uri-to-safe-path
                                    old-uri
                                    "workspace edit rename oldUri"))
                     (new-resolved (vortel-lsp--workspace-edit-uri-to-safe-path
                                    new-uri
                                    "workspace edit rename newUri")))
                (cond
                 ((not (plist-get old-resolved :ok))
                  (setq failure (plist-get old-resolved :reason)))
                 ((not (plist-get new-resolved :ok))
                  (setq failure (plist-get new-resolved :reason)))
                 (t
                  (push (list :kind 'rename-file
                              :old-path (plist-get old-resolved :path)
                              :new-path (plist-get new-resolved :path)
                              :options options)
                        operations)))))
             ((string= kind "delete")
              (let* ((uri (vortel-lsp-hash-get entry "uri"))
                     (options (vortel-lsp-hash-get entry "options"))
                     (resolved (vortel-lsp--workspace-edit-uri-to-safe-path
                                uri
                                "workspace edit delete")))
                (if (not (plist-get resolved :ok))
                    (setq failure (plist-get resolved :reason))
                  (push (list :kind 'delete-file
                              :path (plist-get resolved :path)
                              :options options)
                        operations))))
             (t
              (setq failure (format "unsupported workspace edit operation kind: %s" kind))))))))
    (if failure
        (list :ok nil :reason failure)
      (list :ok t :operations (nreverse operations)))))

(defun vortel-lsp--workspace-edit-track-renamed-buffer (old-path new-path)
  "Update visited file path from OLD-PATH to NEW-PATH when buffer is live."
  (when-let* ((buffer (get-file-buffer old-path)))
    (with-current-buffer buffer
      (let ((modified (buffer-modified-p)))
        (set-visited-file-name new-path t nil)
        (set-buffer-modified-p modified)))))

(defun vortel-lsp--workspace-edit-track-deleted-buffer (path)
  "Detach PATH from a live visiting buffer after file deletion."
  (when-let* ((buffer (get-file-buffer path)))
    (with-current-buffer buffer
      (let ((modified (buffer-modified-p)))
        (set-visited-file-name nil t nil)
        (set-buffer-modified-p modified)))))

(defun vortel-lsp--workspace-edit-create-file (path options)
  "Apply a workspace create operation for PATH using OPTIONS.
Return plist with `:ok' and optional `:reason'."
  (let ((overwrite (vortel-lsp-truthy-p (vortel-lsp-hash-get options "overwrite")))
        (ignore-if-exists (vortel-lsp-truthy-p
                           (vortel-lsp-hash-get options "ignoreIfExists"))))
    (condition-case err
        (cond
         ((file-directory-p path)
          (list :ok nil :reason (format "cannot create file because directory exists: %s" path)))
         ((file-exists-p path)
          (cond
           (overwrite
            (with-temp-file path)
            (list :ok t))
           (ignore-if-exists
            (list :ok t))
           (t
            (list :ok nil :reason (format "file already exists: %s" path)))))
         ((not (file-directory-p (file-name-directory path)))
          (list :ok nil :reason (format "parent directory does not exist: %s" path)))
         (t
          (with-temp-file path)
          (list :ok t)))
      (file-error
       (list :ok nil :reason (error-message-string err))))))

(defun vortel-lsp--workspace-edit-rename-file (old-path new-path options)
  "Apply a workspace rename operation from OLD-PATH to NEW-PATH.
OPTIONS controls overwrite behavior.
Return plist with `:ok' and optional `:reason'."
  (let ((overwrite (vortel-lsp-truthy-p (vortel-lsp-hash-get options "overwrite")))
        (ignore-if-exists (vortel-lsp-truthy-p
                           (vortel-lsp-hash-get options "ignoreIfExists"))))
    (condition-case err
        (cond
         ((string= (expand-file-name old-path) (expand-file-name new-path))
          (list :ok t))
         ((not (file-exists-p old-path))
          (list :ok nil :reason (format "rename source does not exist: %s" old-path)))
         ((not (file-directory-p (file-name-directory new-path)))
          (list :ok nil :reason (format "rename target parent does not exist: %s" new-path)))
         ((file-exists-p new-path)
          (cond
           (overwrite
            (rename-file old-path new-path t)
            (vortel-lsp--workspace-edit-track-renamed-buffer old-path new-path)
            (list :ok t))
           (ignore-if-exists
            (list :ok t))
           (t
            (list :ok nil :reason (format "rename target already exists: %s" new-path)))))
         (t
          (rename-file old-path new-path nil)
          (vortel-lsp--workspace-edit-track-renamed-buffer old-path new-path)
          (list :ok t)))
      (file-error
       (list :ok nil :reason (error-message-string err))))))

(defun vortel-lsp--workspace-edit-delete-file (path options)
  "Apply a workspace delete operation for PATH using OPTIONS.
Return plist with `:ok' and optional `:reason'."
  (let ((recursive (vortel-lsp-truthy-p (vortel-lsp-hash-get options "recursive")))
        (ignore-if-not-exists (vortel-lsp-truthy-p
                               (vortel-lsp-hash-get options "ignoreIfNotExists"))))
    (condition-case err
        (cond
         ((not (file-exists-p path))
          (if ignore-if-not-exists
              (list :ok t)
            (list :ok nil :reason (format "delete target does not exist: %s" path))))
         ((file-directory-p path)
          (if recursive
              (progn
                (delete-directory path t)
                (list :ok t))
            (list :ok nil :reason (format "refusing non-recursive directory delete: %s" path))))
         (t
          (delete-file path)
          (vortel-lsp--workspace-edit-track-deleted-buffer path)
          (list :ok t)))
      (file-error
       (list :ok nil :reason (error-message-string err))))))

(defun vortel-lsp--apply-workspace-edit-operation (operation encoding)
  "Apply one WORKSPACE-EDIT OPERATION using position ENCODING.
Return plist with `:ok' and optional `:reason'."
  (pcase (plist-get operation :kind)
    ('text-edits
     (let ((path (plist-get operation :path))
           (edits (plist-get operation :edits)))
       (cond
        ((null edits)
         (list :ok t))
        ((not (or (file-exists-p path)
                  (get-file-buffer path)))
         (list :ok nil :reason (format "text edit target does not exist: %s" path)))
        (t
         (condition-case err
             (if (vortel-lsp--apply-text-edits-to-path path edits encoding)
                 (list :ok t)
               (list :ok nil :reason (format "failed to apply edits to %s" path)))
           (error
            (list :ok nil :reason (format "%s" err))))))))
    ('create-file
     (vortel-lsp--workspace-edit-create-file
      (plist-get operation :path)
      (plist-get operation :options)))
    ('rename-file
     (vortel-lsp--workspace-edit-rename-file
      (plist-get operation :old-path)
      (plist-get operation :new-path)
      (plist-get operation :options)))
    ('delete-file
     (vortel-lsp--workspace-edit-delete-file
      (plist-get operation :path)
      (plist-get operation :options)))
    (_
     (list :ok nil
           :reason (format "unknown workspace edit operation: %s"
                           (plist-get operation :kind))))))

(defun vortel-lsp--apply-workspace-edit (client workspace-edit)
  "Apply WORKSPACE-EDIT received from CLIENT.
Return ApplyWorkspaceEditResponse payload hash-table."
  (condition-case err
      (let* ((collected (vortel-lsp--collect-workspace-edit-operations workspace-edit))
             (encoding (vortel-lsp-client-position-encoding client)))
        (if (not (plist-get collected :ok))
            (vortel-lsp-make-hash
             "applied" vortel-lsp--json-false
             "failureReason" (or (plist-get collected :reason)
                                  "workspace edit contains unsupported operations"))
          (let ((ok t)
                (failure nil))
            (dolist (operation (plist-get collected :operations))
              (when ok
                (let ((result (vortel-lsp--apply-workspace-edit-operation operation encoding)))
                  (unless (plist-get result :ok)
                    (setq ok nil
                          failure (or (plist-get result :reason)
                                      "workspace edit operation failed"))))))
            (if ok
                (vortel-lsp-make-hash "applied" t)
              (vortel-lsp-make-hash
               "applied" vortel-lsp--json-false
               "failureReason" (or failure "workspace edit apply failed"))))))
    (error
     (vortel-lsp-make-hash
      "applied" vortel-lsp--json-false
      "failureReason" (format "%s" err)))))

(defun vortel-lsp--send-did-open (attachment)
  "Send didOpen for ATTACHMENT when not already opened."
  (let* ((client (plist-get attachment :client))
         (uri (vortel-lsp--buffer-uri))
         (language-id (or (and vortel-lsp--language
                               (vortel-lsp-hash-get vortel-lsp--language "language_id"))
                          (and vortel-lsp--language
                               (vortel-lsp-hash-get vortel-lsp--language "name"))
                          (symbol-name major-mode))))
    (when (and uri
               (vortel-lsp-client-ready-p client)
               (not (gethash client vortel-lsp--opened-clients)))
      (vortel-lsp-client-did-open
       client
       uri
       vortel-lsp--document-version
       (buffer-substring-no-properties (point-min) (point-max))
       language-id)
      (puthash client t vortel-lsp--opened-clients))))

(defun vortel-lsp--send-did-close (attachment)
  "Send didClose for ATTACHMENT when it was opened."
  (let ((client (plist-get attachment :client))
        (uri (vortel-lsp--buffer-uri)))
    (when (and uri (gethash client vortel-lsp--opened-clients))
      (vortel-lsp-client-did-close client uri)
      (remhash client vortel-lsp--opened-clients))))

(defun vortel-lsp--request-handler (client _id method params reply)
  "Handle server->client requests for CLIENT.
ID, METHOD, PARAMS, and REPLY are defined by `vortel-lsp-client'."
  (pcase method
    ("workspace/workspaceFolders"
     (let* ((attachment (cl-find client vortel-lsp--attachments
                                 :key (lambda (item)
                                        (plist-get item :client))))
            (root (or (plist-get attachment :root) default-directory)))
       (funcall reply
                (list (vortel-lsp-make-hash
                       "uri" (vortel-lsp-path-to-uri root)
                       "name" (file-name-nondirectory (directory-file-name root)))))))
    ("workspace/configuration"
     (let* ((items (or (vortel-lsp-hash-get params "items") '()))
            (result (mapcar (lambda (_item) nil) items)))
       (funcall reply result)))
    ("window/workDoneProgress/create"
     (funcall reply nil))
    ("client/registerCapability"
     (vortel-lsp-client-register-capabilities
      client
      (or (vortel-lsp-hash-get params "registrations") '()))
     (funcall reply nil))
    ("client/unregisterCapability"
     (vortel-lsp-client-unregister-capabilities
      client
      (or (vortel-lsp-hash-get params "unregisterations")
          (vortel-lsp-hash-get params "unregistrations")
          '()))
     (funcall reply nil))
    ("workspace/applyEdit"
     (let ((workspace-edit (vortel-lsp-hash-get params "edit")))
       (funcall reply (vortel-lsp--apply-workspace-edit client workspace-edit))))
    ("window/showDocument"
     (let* ((uri (vortel-lsp-hash-get params "uri"))
            (path (vortel-lsp-uri-to-path uri)))
       (if (and path (file-readable-p path))
           (progn
             (find-file-other-window path)
             (funcall reply (vortel-lsp-make-hash "success" t)))
         (funcall reply (vortel-lsp-make-hash "success" vortel-lsp--json-false)))))
    (_
     (funcall reply nil
              (vortel-lsp-make-hash
               "code" -32601
               "message" (format "Unhandled method: %s" method))))))

(defun vortel-lsp--severity-to-flymake (severity)
  "Convert LSP diagnostic SEVERITY into Flymake type symbol."
  (pcase severity
    (1 :error)
    (2 :warning)
    (_ :note)))

(defun vortel-lsp--diagnostics-for-current-buffer ()
  "Build Flymake diagnostics list from buffered LSP diagnostics."
  (let (result)
    (maphash
     (lambda (_client-id diagnostics)
       (dolist (diag diagnostics)
         (let* ((range (vortel-lsp-hash-get diag "range"))
                (start (vortel-lsp-hash-get range "start"))
                (end (vortel-lsp-hash-get range "end"))
                (source-attachment
                 (cl-find-if
                  (lambda (attachment)
                    (equal (vortel-lsp-client-id (plist-get attachment :client))
                           (vortel-lsp-hash-get diag "_vortel_client_id")))
                  vortel-lsp--attachments))
                (encoding
                 (if source-attachment
                 (vortel-lsp-client-position-encoding
                      (plist-get source-attachment :client))
                   'utf-16))
                (beg (vortel-lsp--lsp-position-to-point start encoding))
                (fin (max beg (vortel-lsp--lsp-position-to-point end encoding)))
                (message (or (vortel-lsp-hash-get diag "message") "LSP diagnostic"))
                (severity (vortel-lsp--severity-to-flymake
                           (vortel-lsp-hash-get diag "severity"))))
           (push (flymake-make-diagnostic (current-buffer) beg fin severity message)
                 result))))
     vortel-lsp--diagnostics)
    (nreverse result)))

(defun vortel-lsp--report-flymake ()
  "Push updated diagnostics to Flymake when backend is active."
  (when vortel-lsp--flymake-report-fn
    (funcall vortel-lsp--flymake-report-fn
             (vortel-lsp--diagnostics-for-current-buffer))))

(defun vortel-lsp--handle-publish-diagnostics (client params)
  "Handle textDocument/publishDiagnostics notification from CLIENT."
  (let* ((uri (vortel-lsp-hash-get params "uri"))
         (current-uri (vortel-lsp--buffer-uri)))
    (when (and uri current-uri (string= uri current-uri))
      (let* ((client-id (vortel-lsp-client-id client))
             (client-key (format "%s" client-id))
             (diagnostics (or (vortel-lsp-hash-get params "diagnostics") '())))
        (setq diagnostics
              (mapcar (lambda (diag)
                        (puthash "_vortel_client_id" client-id diag)
                        diag)
                      diagnostics))
        (puthash client-key diagnostics vortel-lsp--diagnostics)
        (vortel-lsp--report-flymake)))))

(defun vortel-lsp--notification-handler (client method params)
  "Handle notifications from CLIENT."
  (pcase method
    ("textDocument/publishDiagnostics"
     (vortel-lsp--handle-publish-diagnostics client params))
    ("window/showMessage"
     (message "[%s] %s"
              (vortel-lsp-client-name client)
              (or (vortel-lsp-hash-get params "message") "")))
    ("window/logMessage"
     (vortel-lsp-log "[%s] %s"
                     (vortel-lsp-client-name client)
                     (or (vortel-lsp-hash-get params "message") "")))
    ("exit"
     (message "LSP server exited: %s" (vortel-lsp-client-name client)))
    (_ nil)))

(defun vortel-lsp--state-handler (_client new-state _old-state)
  "Handle state changes for the current buffer attachment."
  (when (eq new-state 'ready)
    (dolist (attachment vortel-lsp--attachments)
      (vortel-lsp--send-did-open attachment))))

(defun vortel-lsp--before-change (beg end)
  "Track pre-change ranges from BEG to END for incremental sync."
  (when (and vortel-lsp-mode vortel-lsp--attachments)
    (setq vortel-lsp--before-change-ranges nil)
    (let ((encodings (delete-dups
                      (mapcar (lambda (attachment)
                                (vortel-lsp-client-position-encoding
                                 (plist-get attachment :client)))
                              vortel-lsp--attachments))))
      (dolist (encoding encodings)
        (let ((start (vortel-lsp--point-to-lsp-position beg encoding))
              (finish (vortel-lsp--point-to-lsp-position end encoding)))
          (push (cons encoding (cons start finish))
                vortel-lsp--before-change-ranges))))))

(defun vortel-lsp--flush-changes (buffer)
  "Flush pending didChange notifications for BUFFER."
  (vortel-lsp--with-live-buffer
   buffer
   (lambda ()
     (setq vortel-lsp--change-timer nil)
     (when (and vortel-lsp-mode
                vortel-lsp--attachments
                vortel-lsp--pending-changes)
       (let* ((uri (vortel-lsp--buffer-uri))
              (changes (nreverse vortel-lsp--pending-changes))
              (full-text (buffer-substring-no-properties (point-min) (point-max))))
         (setq vortel-lsp--pending-changes nil)
         (cl-incf vortel-lsp--document-version)
         (dolist (attachment vortel-lsp--attachments)
           (let* ((client (plist-get attachment :client))
                  (encoding (vortel-lsp-client-position-encoding client))
                  (encoded-changes
                   (mapcar
                    (lambda (change)
                      (let* ((range-map (plist-get change :ranges))
                             (range-pair (assoc encoding range-map))
                             (range (cdr range-pair)))
                        (if (and range (car range) (cdr range))
                            (vortel-lsp-make-hash
                             "range" (vortel-lsp-make-hash
                                      "start" (car range)
                                      "end" (cdr range))
                             "text" (plist-get change :text))
                          (vortel-lsp-make-hash "text" full-text))))
                    changes)))
             (when (gethash client vortel-lsp--opened-clients)
               (vortel-lsp-client-did-change
                client
                uri
                vortel-lsp--document-version
                full-text
                encoded-changes)))))))))

(defun vortel-lsp--after-change (beg end _pre-change-length)
  "Collect incremental changes after edit from BEG to END."
  (when (and vortel-lsp-mode vortel-lsp--attachments)
    (let ((text (buffer-substring-no-properties beg end)))
      (push (list :ranges vortel-lsp--before-change-ranges
                  :text text)
            vortel-lsp--pending-changes)
      (unless vortel-lsp--change-timer
        (setq vortel-lsp--change-timer
              (run-at-time
               vortel-lsp-change-debounce
               nil
               #'vortel-lsp--flush-changes
               (current-buffer)))))))

(defun vortel-lsp--after-save ()
  "Send didSave for all opened attachments."
  (let ((uri (vortel-lsp--buffer-uri))
        (text (buffer-substring-no-properties (point-min) (point-max))))
    (dolist (attachment vortel-lsp--attachments)
      (let ((client (plist-get attachment :client)))
        (when (gethash client vortel-lsp--opened-clients)
          (vortel-lsp-client-did-save client uri text))))))

(defun vortel-lsp--flymake-backend (report-fn &rest _args)
  "Flymake backend entrypoint.
REPORT-FN is retained and reused on incoming diagnostics."
  (setq vortel-lsp--flymake-report-fn report-fn)
  (funcall report-fn (vortel-lsp--diagnostics-for-current-buffer)))

(defun vortel-lsp--cleanup-attachments ()
  "Unregister handlers and send didClose for all attachments."
  (when vortel-lsp--change-timer
    (cancel-timer vortel-lsp--change-timer)
    (setq vortel-lsp--change-timer nil))
  (when vortel-lsp--pending-changes
    (setq vortel-lsp--pending-changes nil))
  (dolist (attachment vortel-lsp--attachments)
    (let ((client (plist-get attachment :client))
          (notification-handler (plist-get attachment :notification-handler))
          (request-handler (plist-get attachment :request-handler))
          (state-handler (plist-get attachment :state-handler)))
      (when notification-handler
        (vortel-lsp-client-remove-notification-handler client notification-handler))
      (when request-handler
        (vortel-lsp-client-remove-request-handler client request-handler))
      (when state-handler
        (vortel-lsp-client-remove-state-handler client state-handler))
      (vortel-lsp--send-did-close attachment)))
  (setq vortel-lsp--attachments nil)
  (setq vortel-lsp--opened-clients nil)
  (setq vortel-lsp--flymake-report-fn nil)
  (setq vortel-lsp--completion-resolve-cache nil)
  (clrhash vortel-lsp--diagnostics))

(defun vortel-lsp--enable ()
  "Enable vortel-lsp in current buffer."
  (unless buffer-file-name
    (user-error "vortel-lsp requires a file-backed buffer"))
  (let ((attachments (vortel-lsp-registry-attachments-for-path buffer-file-name)))
    (unless attachments
      (user-error "no language servers configured for this file"))
    (setq vortel-lsp--attachments attachments)
    (setq vortel-lsp--language (plist-get (car attachments) :language))
    (setq vortel-lsp--opened-clients (make-hash-table :test #'eq))
    (setq vortel-lsp--document-version 0)
    (setq vortel-lsp--pending-changes nil)
    (setq vortel-lsp--before-change-ranges nil)
    (setq vortel-lsp--flymake-report-fn nil)
    (setq vortel-lsp--diagnostics (make-hash-table :test #'equal))
    (setq vortel-lsp--attachments
          (mapcar
           (lambda (attachment)
             (let* ((client (plist-get attachment :client))
                    (buffer (current-buffer))
                    (notification-handler
                     (lambda (incoming-client method params)
                       (vortel-lsp--with-live-buffer
                        buffer
                        (lambda ()
                          (when vortel-lsp-mode
                            (vortel-lsp--notification-handler
                             incoming-client method params))))))
                    (request-handler
                     (lambda (incoming-client id method params reply)
                       (vortel-lsp--with-live-buffer
                        buffer
                        (lambda ()
                          (when vortel-lsp-mode
                            (vortel-lsp--request-handler
                             incoming-client id method params reply))))))
                    (state-handler
                     (lambda (incoming-client new-state old-state)
                       (vortel-lsp--with-live-buffer
                        buffer
                        (lambda ()
                          (when vortel-lsp-mode
                            (vortel-lsp--state-handler
                             incoming-client new-state old-state)))))))
               (vortel-lsp-client-add-notification-handler client notification-handler)
               (vortel-lsp-client-add-request-handler client request-handler)
               (vortel-lsp-client-add-state-handler client state-handler)
               (plist-put
                (plist-put
                 (plist-put attachment :notification-handler notification-handler)
                 :request-handler request-handler)
                :state-handler state-handler)))
           attachments))
    (dolist (attachment vortel-lsp--attachments)
      (vortel-lsp--send-did-open attachment))
    (add-hook 'before-change-functions #'vortel-lsp--before-change nil t)
    (add-hook 'after-change-functions #'vortel-lsp--after-change nil t)
    (add-hook 'after-save-hook #'vortel-lsp--after-save nil t)
    (add-hook 'xref-backend-functions #'vortel-lsp--xref-backend nil t)
    (when vortel-lsp-enable-capf
      (add-hook 'completion-at-point-functions #'vortel-lsp--completion-at-point nil t))
    (when vortel-lsp-enable-flymake
      (add-hook 'flymake-diagnostic-functions #'vortel-lsp--flymake-backend nil t)
      (flymake-mode 1))))

(defun vortel-lsp--disable ()
  "Disable vortel-lsp in current buffer."
  (remove-hook 'before-change-functions #'vortel-lsp--before-change t)
  (remove-hook 'after-change-functions #'vortel-lsp--after-change t)
  (remove-hook 'after-save-hook #'vortel-lsp--after-save t)
  (remove-hook 'xref-backend-functions #'vortel-lsp--xref-backend t)
  (remove-hook 'completion-at-point-functions #'vortel-lsp--completion-at-point t)
  (remove-hook 'flymake-diagnostic-functions #'vortel-lsp--flymake-backend t)
  (vortel-lsp--cleanup-attachments))

(define-minor-mode vortel-lsp-mode
  "Minor mode for vortel-lsp."
  :group 'vortel-lsp
  :lighter " Vortel"
  (if vortel-lsp-mode
      (condition-case err
          (vortel-lsp--enable)
        (error
         (setq vortel-lsp-mode nil)
         (signal (car err) (cdr err))))
    (vortel-lsp--disable)))

(defun vortel-lsp--text-document-position-params (client)
  "Build textDocument/position params for CLIENT at point."
  (vortel-lsp-make-hash
   "textDocument" (vortel-lsp-make-hash "uri" (vortel-lsp--buffer-uri))
   "position"
   (vortel-lsp--point-to-lsp-position (point)
                                      (vortel-lsp-client-position-encoding client))))

(defun vortel-lsp--text-document-references-params (client)
  "Build textDocument/references params for CLIENT at point."
  (vortel-lsp-make-hash
   "textDocument" (vortel-lsp-make-hash "uri" (vortel-lsp--buffer-uri))
   "position"
   (vortel-lsp--point-to-lsp-position (point)
                                      (vortel-lsp-client-position-encoding client))
   "context"
   (vortel-lsp-make-hash
    "includeDeclaration" (if vortel-lsp-goto-reference-include-declaration
                              t
                            vortel-lsp--json-false))))

(defun vortel-lsp--xref-backend ()
  "Return xref backend symbol when `vortel-lsp-mode' is active."
  (when vortel-lsp-mode
    'vortel-lsp))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql vortel-lsp)))
  "Return identifier at point for the vortel-lsp xref backend."
  (thing-at-point 'symbol t))

(cl-defmethod xref-backend-definitions ((_backend (eql vortel-lsp)) _identifier)
  "Return xref definitions from attached LSP servers."
  (vortel-lsp--xref-request
   "goto-definition"
   "textDocument/definition"
   #'vortel-lsp--text-document-position-params))

(cl-defmethod xref-backend-references ((_backend (eql vortel-lsp)) _identifier)
  "Return xref references from attached LSP servers."
  (vortel-lsp--xref-request
   "goto-reference"
   "textDocument/references"
   #'vortel-lsp--text-document-references-params))

(defun vortel-lsp-hover ()
  "Request and display hover info at point."
  (interactive)
  (let ((attachments (vortel-lsp--attachments-for-feature "hover")))
    (unless attachments
      (user-error "no attached language server supports hover"))
    (let ((resolved nil))
      (dolist (attachment attachments)
        (let ((client (plist-get attachment :client)))
          (vortel-lsp-client-request
           client
           "textDocument/hover"
           (vortel-lsp--text-document-position-params client)
           :on-success
           (lambda (result)
             (unless resolved
               (let ((contents (vortel-lsp-hash-get result "contents")))
                 (when contents
                   (setq resolved t)
                   (message "%s" (vortel-lsp--hover-contents-to-string contents))))))))))))

(defun vortel-lsp--hover-contents-to-string (contents)
  "Render hover CONTENTS into a message-friendly string."
  (cond
   ((stringp contents) contents)
   ((hash-table-p contents)
    (or (vortel-lsp-hash-get contents "value")
        (format "%s" contents)))
   ((listp contents)
    (string-join
     (mapcar #'vortel-lsp--hover-contents-to-string contents)
     "\n\n"))
   (t
    (format "%s" contents))))

(defun vortel-lsp-find-definition ()
  "Jump to definition using the xref backend integration."
  (interactive)
  (call-interactively #'xref-find-definitions))

(defun vortel-lsp-find-references ()
  "Find references using the xref backend integration."
  (interactive)
  (call-interactively #'xref-find-references))

(defconst vortel-lsp--completion-kind-names
  [nil
   "Text" "Method" "Function" "Constructor" "Field" "Variable" "Class"
   "Interface" "Module" "Property" "Unit" "Value" "Enum" "Keyword"
   "Snippet" "Color" "File" "Reference" "Folder" "EnumMember"
   "Constant" "Struct" "Event" "Operator" "TypeParameter"]
  "LSP CompletionItemKind names indexed by kind integer.")

(defun vortel-lsp--completion-items-from-response (response)
  "Extract completion item list from RESPONSE."
  (cond
   ((null response) nil)
   ((listp response) response)
   ((hash-table-p response)
    (or (vortel-lsp-hash-get response "items") nil))
   (t nil)))

(defun vortel-lsp--completion-item-text (item)
  "Return insertion text for completion ITEM."
  (let* ((label (or (vortel-lsp-hash-get item "label") ""))
         (text-edit (vortel-lsp-hash-get item "textEdit"))
         (new-text (and (hash-table-p text-edit)
                        (vortel-lsp-hash-get text-edit "newText")))
         (insert-text (or new-text (vortel-lsp-hash-get item "insertText") label))
         (insert-format (or (vortel-lsp-hash-get item "insertTextFormat") 1)))
    (if (= insert-format 2)
        label
      insert-text)))

(defun vortel-lsp--completion-item-candidate (item client)
  "Build completion candidate string from ITEM provided by CLIENT."
  (propertize
   (vortel-lsp--completion-item-text item)
   'vortel-lsp-item item
   'vortel-lsp-client client
   'vortel-lsp-label (or (vortel-lsp-hash-get item "label") "")))

(defun vortel-lsp--completion-resolve-item (item client)
  "Return resolved completion ITEM from CLIENT when supported."
  (if (or (not (hash-table-p item))
          (not client))
      item
    (unless (hash-table-p vortel-lsp--completion-resolve-cache)
      (setq vortel-lsp--completion-resolve-cache (make-hash-table :test #'eq)))
    (let ((cached (gethash item vortel-lsp--completion-resolve-cache :vortel-none)))
      (if (not (eq cached :vortel-none))
          cached
        (let ((resolved item))
          (when (vortel-lsp-client-completion-resolve-supported-p client)
            (let ((response (vortel-lsp--request-sync
                             client
                             "completionItem/resolve"
                             item
                             vortel-lsp-completion-timeout)))
              (when (and (plist-get response :ok)
                         (hash-table-p (plist-get response :result)))
                (setq resolved (plist-get response :result)))))
          (puthash item resolved vortel-lsp--completion-resolve-cache)
          resolved)))))

(defun vortel-lsp--completion-markup-to-string (value)
  "Convert completion documentation VALUE into plain text."
  (cond
   ((stringp value) value)
   ((hash-table-p value)
    (or (vortel-lsp-hash-get value "value")
        (format "%s" value)))
   ((listp value)
    (string-join
     (delq nil (mapcar #'vortel-lsp--completion-markup-to-string value))
     "\n\n"))
   (t nil)))

(defun vortel-lsp--completion-item-documentation (item)
  "Return displayable documentation string for completion ITEM."
  (when (hash-table-p item)
    (let* ((detail (vortel-lsp-hash-get item "detail"))
           (doc (vortel-lsp--completion-markup-to-string
                 (vortel-lsp-hash-get item "documentation")))
           (detail-text (and (stringp detail) (not (string-empty-p detail)) detail))
           (doc-text (and (stringp doc) (not (string-empty-p doc)) doc)))
      (cond
       ((and detail-text doc-text)
        (format "%s\n\n%s" detail-text doc-text))
       (detail-text detail-text)
       (doc-text doc-text)
       (t nil)))))

(defconst vortel-lsp--completion-doc-buffer-name "*vortel-lsp-completion-doc*"
  "Buffer name used to render completion documentation.")

(defun vortel-lsp--completion-docsig (candidate)
  "Return short documentation signature for completion CANDIDATE."
  (let* ((item (get-text-property 0 'vortel-lsp-item candidate))
         (client (get-text-property 0 'vortel-lsp-client candidate))
         (resolved (vortel-lsp--completion-resolve-item item client))
         (documentation (vortel-lsp--completion-item-documentation resolved)))
    (when (and (stringp documentation)
               (not (string-empty-p documentation)))
      (car (split-string documentation "\n" t)))))

(defun vortel-lsp--completion-doc-buffer (candidate)
  "Return a documentation buffer for completion CANDIDATE."
  (let* ((item (get-text-property 0 'vortel-lsp-item candidate))
         (client (get-text-property 0 'vortel-lsp-client candidate))
         (resolved (vortel-lsp--completion-resolve-item item client))
         (documentation (vortel-lsp--completion-item-documentation resolved)))
    (when (and (stringp documentation)
               (not (string-empty-p documentation)))
      (let ((buffer (get-buffer-create vortel-lsp--completion-doc-buffer-name)))
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert documentation)
          (goto-char (point-min))
          (setq buffer-read-only t))
        buffer))))

(defun vortel-lsp--completion-annotation (candidate)
  "Return annotation string for completion CANDIDATE."
  (let* ((item (get-text-property 0 'vortel-lsp-item candidate))
         (detail (and item (vortel-lsp-hash-get item "detail")))
         (kind-id (and item (vortel-lsp-hash-get item "kind")))
         (kind-name (and (integerp kind-id)
                         (> kind-id 0)
                         (< kind-id (length vortel-lsp--completion-kind-names))
                         (aref vortel-lsp--completion-kind-names kind-id))))
    (cond
     ((and (stringp detail) (not (string-empty-p detail)))
      (concat " " detail))
     ((and (stringp kind-name) (not (string-empty-p kind-name)))
      (concat " [" kind-name "]"))
     (t ""))))

(defun vortel-lsp--completion-exit (candidate status)
  "Completion exit hook for CANDIDATE and STATUS.
Applies additional text edits when provided by the server."
  (when (and (eq status 'finished)
             (stringp candidate))
    (let* ((item (get-text-property 0 'vortel-lsp-item candidate))
            (client (get-text-property 0 'vortel-lsp-client candidate))
            (resolved (vortel-lsp--completion-resolve-item item client))
            (additional (and (hash-table-p resolved)
                             (vortel-lsp-hash-get resolved "additionalTextEdits"))))
      (when (and (listp additional) client)
        (condition-case err
            (vortel-lsp--apply-text-edits-in-buffer
             (current-buffer)
             additional
             (vortel-lsp-client-position-encoding client))
          (error
            (vortel-lsp-log "completion additionalTextEdits failed: %s" err))))
      (when-let* ((documentation (vortel-lsp--completion-item-documentation resolved))
                  (preview (car (split-string documentation "\n" t))))
        (message "%s" preview)))))

(defun vortel-lsp--completion-at-point ()
  "CAPF backend for `vortel-lsp-mode'."
  (let ((attachments (vortel-lsp--attachments-for-feature "completion")))
    (when attachments
      (let* ((bounds (or (bounds-of-thing-at-point 'symbol)
                         (cons (point) (point))))
             (start (car bounds))
             (end (cdr bounds))
              (candidates nil))
        (setq vortel-lsp--completion-resolve-cache (make-hash-table :test #'eq))
        (dolist (attachment attachments)
          (let* ((client (plist-get attachment :client))
                 (params
                  (vortel-lsp-make-hash
                   "textDocument" (vortel-lsp-make-hash "uri" (vortel-lsp--buffer-uri))
                   "position"
                   (vortel-lsp--point-to-lsp-position
                    (point)
                    (vortel-lsp-client-position-encoding client))
                   "context" (vortel-lsp-make-hash "triggerKind" 1)))
                 (response
                  (vortel-lsp--request-sync
                   client
                   "textDocument/completion"
                   params
                   vortel-lsp-completion-timeout)))
            (when (plist-get response :ok)
              (dolist (item (vortel-lsp--completion-items-from-response
                             (plist-get response :result)))
                (when (hash-table-p item)
                  (push (vortel-lsp--completion-item-candidate item client)
                        candidates))))))
        (when candidates
          (setq vortel-lsp--completion-candidates (nreverse candidates))
          (list start
                end
                (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata (category . vortel-lsp))
                    (complete-with-action action vortel-lsp--completion-candidates string pred)))
                :annotation-function #'vortel-lsp--completion-annotation
                :company-docsig #'vortel-lsp--completion-docsig
                :company-doc-buffer #'vortel-lsp--completion-doc-buffer
                :exit-function #'vortel-lsp--completion-exit
                :exclusive 'no))))))


(provide 'vortel-lsp)

;;; vortel-lsp.el ends here
