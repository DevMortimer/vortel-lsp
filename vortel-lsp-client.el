;;; vortel-lsp-client.el --- LSP client core for vortel-lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors
;; Keywords: languages, tools

;;; Commentary:

;; Client state machine inspired by Helix LSP behavior:
;; - async process transport
;; - initialization gating and pending queue
;; - request correlation with per-request timeout

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(require 'vortel-lsp-transport)
(require 'vortel-lsp-util)

(defcustom vortel-lsp-default-request-timeout 20
  "Default request timeout in seconds."
  :type 'integer
  :group 'vortel-lsp)

(cl-defstruct (vortel-lsp-client (:constructor vortel-lsp-client--create))
  "LSP client state."
  id
  name
  command
  args
  root-path
  root-uri
  initialization-options
  timeout
  environment
  transport
  (state 'starting)
  (next-request-id 0)
  pending
  send-queue
  capabilities
  dynamic-capabilities
  watched-file-registrations
  settings
  server-info
  notification-handlers
  request-handlers
  state-handlers)

(defconst vortel-lsp-client--file-change-created 1)
(defconst vortel-lsp-client--file-change-changed 2)
(defconst vortel-lsp-client--file-change-deleted 3)

(defconst vortel-lsp-client--watch-kind-create 1)
(defconst vortel-lsp-client--watch-kind-change 2)
(defconst vortel-lsp-client--watch-kind-delete 4)
(defconst vortel-lsp-client--watch-kind-all 7)

(defun vortel-lsp-client--watch-capability-p (method)
  "Return non-nil when METHOD is watched-files dynamic registration."
  (string= method "workspace/didChangeWatchedFiles"))

(defun vortel-lsp-client--watcher-base-path (client watcher)
  "Resolve WATCHER base path for CLIENT.
Return nil when base URI is unsupported."
  (let ((glob-pattern (vortel-lsp-hash-get watcher "globPattern")))
    (cond
     ((stringp glob-pattern)
      (vortel-lsp-client-root-path client))
     ((hash-table-p glob-pattern)
      (let* ((base-uri-value (vortel-lsp-hash-get glob-pattern "baseUri"))
             (base-uri
              (cond
               ((stringp base-uri-value) base-uri-value)
               ((hash-table-p base-uri-value)
                (or (vortel-lsp-hash-get base-uri-value "uri")
                    (vortel-lsp-hash-get base-uri-value "value")))
               (t nil)))
             (base-path (and (stringp base-uri) (vortel-lsp-uri-to-path base-uri))))
        (when (and base-path (file-directory-p base-path))
          base-path)))
     (t nil))))

(defun vortel-lsp-client--watcher-kind-mask (watcher)
  "Return watcher kind bitmask for WATCHER."
  (let ((kind (vortel-lsp-hash-get watcher "kind" vortel-lsp-client--watch-kind-all)))
    (if (and (integerp kind) (> kind 0))
        kind
      vortel-lsp-client--watch-kind-all)))

(defun vortel-lsp-client--watch-kind-enabled-p (mask bit)
  "Return non-nil when MASK includes BIT."
  (not (zerop (logand mask bit))))

(defun vortel-lsp-client--watch-event-path (value)
  "Normalize file-notify event VALUE into a path string."
  (cond
   ((stringp value) value)
   ((and (consp value) (stringp (car value))) (car value))
   (t nil)))

(defun vortel-lsp-client--watch-change (path type)
  "Build one watched-files PATH change payload with TYPE."
  (when (and (stringp path) (file-name-absolute-p path))
    (vortel-lsp-make-hash
     "uri" (vortel-lsp-path-to-uri path)
     "type" type)))

(defun vortel-lsp-client--watch-event-changes (event kind-mask)
  "Convert file EVENT to LSP changes, filtered by KIND-MASK."
  (let* ((action (nth 1 event))
         (path-a (vortel-lsp-client--watch-event-path (nth 2 event)))
         (path-b (vortel-lsp-client--watch-event-path (nth 3 event)))
         changes)
    (pcase action
      ('created
       (when (vortel-lsp-client--watch-kind-enabled-p kind-mask vortel-lsp-client--watch-kind-create)
         (when-let* ((change (vortel-lsp-client--watch-change
                              path-a
                              vortel-lsp-client--file-change-created)))
           (push change changes))))
      ((or 'changed 'attribute-changed)
       (when (vortel-lsp-client--watch-kind-enabled-p kind-mask vortel-lsp-client--watch-kind-change)
         (when-let* ((change (vortel-lsp-client--watch-change
                              path-a
                              vortel-lsp-client--file-change-changed)))
           (push change changes))))
      ('deleted
       (when (vortel-lsp-client--watch-kind-enabled-p kind-mask vortel-lsp-client--watch-kind-delete)
         (when-let* ((change (vortel-lsp-client--watch-change
                              path-a
                              vortel-lsp-client--file-change-deleted)))
           (push change changes))))
      ('renamed
       (when (vortel-lsp-client--watch-kind-enabled-p kind-mask vortel-lsp-client--watch-kind-delete)
         (when-let* ((change (vortel-lsp-client--watch-change
                              path-a
                              vortel-lsp-client--file-change-deleted)))
           (push change changes)))
       (when (vortel-lsp-client--watch-kind-enabled-p kind-mask vortel-lsp-client--watch-kind-create)
         (when-let* ((change (vortel-lsp-client--watch-change
                              path-b
                              vortel-lsp-client--file-change-created)))
           (push change changes))))
      (_ nil))
    (nreverse changes)))

(defun vortel-lsp-client--unregister-file-watch (client registration-id)
  "Remove and stop watched-files registration REGISTRATION-ID on CLIENT."
  (let ((watch-table (vortel-lsp-client-watched-file-registrations client)))
    (when (and (hash-table-p watch-table)
               (stringp registration-id)
               (not (string-empty-p registration-id)))
      (dolist (descriptor (gethash registration-id watch-table))
        (when (fboundp 'file-notify-rm-watch)
          (ignore-errors (file-notify-rm-watch descriptor))))
      (remhash registration-id watch-table)
      (setf (vortel-lsp-client-watched-file-registrations client) watch-table))))

(defun vortel-lsp-client--clear-file-watch-registrations (client)
  "Stop all watched-files registrations on CLIENT."
  (let ((watch-table (vortel-lsp-client-watched-file-registrations client)))
    (when (hash-table-p watch-table)
      (let (ids)
        (maphash (lambda (registration-id _value)
                   (push registration-id ids))
                 watch-table)
        (dolist (registration-id ids)
          (vortel-lsp-client--unregister-file-watch client registration-id))))))

(defun vortel-lsp-client--register-file-watch (client registration)
  "Apply one watched-files dynamic REGISTRATION for CLIENT."
  (let* ((registration-id (vortel-lsp-hash-get registration "id"))
         (options (vortel-lsp-hash-get registration "registerOptions"))
         (watchers (if (hash-table-p options)
                       (or (vortel-lsp-hash-get options "watchers") '())
                     '()))
         (watch-table (or (vortel-lsp-client-watched-file-registrations client)
                          (make-hash-table :test #'equal)))
         descriptors)
    (when (and (stringp registration-id)
               (not (string-empty-p registration-id)))
      (vortel-lsp-client--unregister-file-watch client registration-id)
      (when (fboundp 'file-notify-add-watch)
        (dolist (watcher watchers)
          (let ((base-path (and (hash-table-p watcher)
                                (vortel-lsp-client--watcher-base-path client watcher))))
            (when (and base-path (file-directory-p base-path))
              (let ((kind-mask (vortel-lsp-client--watcher-kind-mask watcher)))
                (condition-case err
                    (push
                     (file-notify-add-watch
                      base-path
                      '(change attribute-change)
                      (lambda (event)
                        (let ((changes (vortel-lsp-client--watch-event-changes event kind-mask)))
                          (when changes
                            (vortel-lsp-client-notify
                             client
                             "workspace/didChangeWatchedFiles"
                             (vortel-lsp-make-hash "changes" changes))))))
                     descriptors)
                  (error
                   (vortel-lsp-log "failed to register file watch (%s): %s"
                                   (vortel-lsp-client-name client)
                                   (error-message-string err)))))))))
      (when descriptors
        (puthash registration-id descriptors watch-table)
        (setf (vortel-lsp-client-watched-file-registrations client) watch-table)))))

(defconst vortel-lsp--feature-method-map
  '(("hover" . ("textDocument/hover"))
    ("goto-definition" . ("textDocument/definition"))
    ("goto-declaration" . ("textDocument/declaration"))
    ("goto-type-definition" . ("textDocument/typeDefinition"))
    ("goto-reference" . ("textDocument/references"))
    ("goto-implementation" . ("textDocument/implementation"))
    ("signature-help" . ("textDocument/signatureHelp"))
    ("completion" . ("textDocument/completion"))
    ("code-action" . ("textDocument/codeAction"))
    ("document-symbols" . ("textDocument/documentSymbol"))
    ("workspace-symbols" . ("workspace/symbol"))
    ("rename-symbol" . ("textDocument/rename"))
    ("format" . ("textDocument/formatting"))
    ("inlay-hints" . ("textDocument/inlayHint"))
    ("document-colors" . ("textDocument/documentColor"))
    ("diagnostics" . ("textDocument/publishDiagnostics")))
  "Mapping between vortel feature names and LSP method strings.")

(defun vortel-lsp-client-live-p (client)
  "Return non-nil when CLIENT transport is live."
  (and (vortel-lsp-client-transport client)
       (vortel-lsp-transport-live-p (vortel-lsp-client-transport client))))

(defun vortel-lsp-client-ready-p (client)
  "Return non-nil when CLIENT has completed initialize/initialized."
  (eq (vortel-lsp-client-state client) 'ready))

(defun vortel-lsp-client-position-encoding (client)
  "Return CLIENT negotiated position encoding symbol.
Possible values are `utf-8', `utf-16', and `utf-32'."
  (let* ((caps (vortel-lsp-client-capabilities client))
         (position-encoding
          (and (hash-table-p caps)
               (or (vortel-lsp-hash-get caps "positionEncoding")
                   (let ((offset-encoding (vortel-lsp-hash-get caps "offsetEncoding")))
                     (if (listp offset-encoding)
                         (car offset-encoding)
                       offset-encoding))))))
    (pcase position-encoding
      ("utf-8" 'utf-8)
      ("utf-32" 'utf-32)
      (_ 'utf-16))))

(defun vortel-lsp-client-add-notification-handler (client fn)
  "Register FN for CLIENT notifications.
FN is called with (client method params)."
  (setf (vortel-lsp-client-notification-handlers client)
        (cons fn (vortel-lsp-client-notification-handlers client))))

(defun vortel-lsp-client-remove-notification-handler (client fn)
  "Unregister FN from CLIENT notification handlers."
  (setf (vortel-lsp-client-notification-handlers client)
        (delq fn (vortel-lsp-client-notification-handlers client))))

(defun vortel-lsp-client-add-request-handler (client fn)
  "Register FN for CLIENT server->client requests.
FN is called with (client id method params reply-fn)."
  (setf (vortel-lsp-client-request-handlers client)
        (cons fn (vortel-lsp-client-request-handlers client))))

(defun vortel-lsp-client-remove-request-handler (client fn)
  "Unregister FN from CLIENT request handlers."
  (setf (vortel-lsp-client-request-handlers client)
        (delq fn (vortel-lsp-client-request-handlers client))))

(defun vortel-lsp-client-add-state-handler (client fn)
  "Register FN for CLIENT state changes.
FN is called with (client new-state old-state)."
  (setf (vortel-lsp-client-state-handlers client)
        (cons fn (vortel-lsp-client-state-handlers client))))

(defun vortel-lsp-client-remove-state-handler (client fn)
  "Unregister FN from CLIENT state handlers."
  (setf (vortel-lsp-client-state-handlers client)
        (delq fn (vortel-lsp-client-state-handlers client))))

(defun vortel-lsp-client-register-capabilities (client registrations)
  "Apply dynamic capability REGISTRATIONS on CLIENT."
  (let ((dynamic (or (vortel-lsp-client-dynamic-capabilities client)
                     (make-hash-table :test #'equal))))
    (dolist (registration registrations)
      (let ((method (vortel-lsp-hash-get registration "method")))
        (when (and (stringp method) (not (string-empty-p method)))
          (puthash method (1+ (or (gethash method dynamic) 0)) dynamic)
          (when (vortel-lsp-client--watch-capability-p method)
            (vortel-lsp-client--register-file-watch client registration)))))
    (setf (vortel-lsp-client-dynamic-capabilities client) dynamic)))

(defun vortel-lsp-client-unregister-capabilities (client unregistrations)
  "Remove dynamic capability UNREGISTRATIONS from CLIENT."
  (let ((dynamic (vortel-lsp-client-dynamic-capabilities client)))
    (when (hash-table-p dynamic)
      (dolist (unregistration unregistrations)
        (let ((method (vortel-lsp-hash-get unregistration "method")))
          (when (and (stringp method) (not (string-empty-p method)))
            (let ((remaining (1- (or (gethash method dynamic) 0))))
              (if (> remaining 0)
                  (puthash method remaining dynamic)
                (remhash method dynamic)))
            (when (vortel-lsp-client--watch-capability-p method)
              (vortel-lsp-client--unregister-file-watch
               client
               (vortel-lsp-hash-get unregistration "id"))))))
      (setf (vortel-lsp-client-dynamic-capabilities client) dynamic))))

(defun vortel-lsp-client--set-state (client new-state)
  "Transition CLIENT to NEW-STATE and notify state handlers."
  (let ((old-state (vortel-lsp-client-state client)))
    (unless (eq old-state new-state)
      (setf (vortel-lsp-client-state client) new-state)
      (dolist (fn (vortel-lsp-client-state-handlers client))
        (condition-case err
            (funcall fn client new-state old-state)
          (error
           (vortel-lsp-log "state handler failed (%s): %s"
                           (vortel-lsp-client-name client)
                           err)))))))

(defun vortel-lsp-client--id-key (id)
  "Normalize JSON-RPC ID into a hash-table key string."
  (cond
   ((numberp id) (number-to-string id))
   ((stringp id) id)
   ((null id) "null")
   (t (format "%s" id))))

(defun vortel-lsp-client--next-id (client)
  "Return and increment the next request ID for CLIENT."
  (prog1 (vortel-lsp-client-next-request-id client)
    (setf (vortel-lsp-client-next-request-id client)
          (1+ (vortel-lsp-client-next-request-id client)))))

(defun vortel-lsp-client--initialization-message-p (payload)
  "Return non-nil when PAYLOAD can be sent before init is complete."
  (let ((method (vortel-lsp-hash-get payload "method")))
    (member method '("initialize" "initialized" "shutdown"))))

(defun vortel-lsp-client--queue-payload (client payload)
  "Queue PAYLOAD for CLIENT preserving order."
  (setf (vortel-lsp-client-send-queue client)
        (nconc (vortel-lsp-client-send-queue client) (list payload))))

(defun vortel-lsp-client--flush-queue (client)
  "Flush queued payloads for CLIENT."
  (let ((queue (vortel-lsp-client-send-queue client)))
    (setf (vortel-lsp-client-send-queue client) nil)
    (dolist (payload queue)
      (vortel-lsp-transport-send (vortel-lsp-client-transport client) payload))))

(defun vortel-lsp-client--send-payload (client payload)
  "Send or gate PAYLOAD according to CLIENT state."
  (let ((state (vortel-lsp-client-state client)))
    (cond
     ((eq state 'starting)
      (if (vortel-lsp-client--initialization-message-p payload)
          (vortel-lsp-transport-send (vortel-lsp-client-transport client) payload)
        (let ((id (vortel-lsp-hash-get payload "id" :vortel-none)))
          (if (eq id :vortel-none)
              ;; Helix behavior: ignore notifications before init.
              nil
            (vortel-lsp-client--queue-payload client payload)))))
     ((eq state 'ready)
      (vortel-lsp-transport-send (vortel-lsp-client-transport client) payload))
     (t
      nil))))

(defun vortel-lsp-client--call-notification-handlers (client method params)
  "Dispatch a server notification METHOD with PARAMS for CLIENT."
  (dolist (fn (vortel-lsp-client-notification-handlers client))
    (condition-case err
        (funcall fn client method params)
      (error
       (vortel-lsp-log "notification handler failed (%s/%s): %s"
                       (vortel-lsp-client-name client)
                       method
                       err)))))

(defun vortel-lsp-client--dispatch-reply (client id method replied-cell &optional result error)
  "Reply to one server request exactly once.
CLIENT, ID, and METHOD identify the request. REPLIED-CELL tracks whether a
reply has already been sent. RESULT and ERROR follow JSON-RPC response shape."
  (if (car replied-cell)
      (vortel-lsp-log "dropping duplicate reply (%s/%s id=%s)"
                      (vortel-lsp-client-name client)
                      method
                      (vortel-lsp-client--id-key id))
    (setcar replied-cell t)
    (if error
        (vortel-lsp-client-reply-error client id error)
      (vortel-lsp-client-reply client id result))))

(defun vortel-lsp-client--dispatch-request (client id method params)
  "Dispatch server request METHOD/PARAMS with ID for CLIENT."
  (let* ((handled nil)
         (replied-cell (list nil))
         (reply (apply-partially #'vortel-lsp-client--dispatch-reply
                                 client
                                 id
                                 method
                                 replied-cell)))
    (dolist (fn (vortel-lsp-client-request-handlers client))
      (unless handled
        (condition-case err
            (let ((handler-result (funcall fn client id method params reply)))
              (when (or (car replied-cell) handler-result)
                (setq handled t)))
          (error
           (vortel-lsp-log "request handler failed (%s/%s): %s"
                           (vortel-lsp-client-name client)
                           method
                           err)))))
    (unless handled
      (vortel-lsp-client-reply-error
       client
       id
       (vortel-lsp-make-hash
        "code" -32601
        "message" (format "Method not handled: %s" method))))))

(defun vortel-lsp-client--complete-request (client message)
  "Complete a pending request on CLIENT from MESSAGE."
  (let* ((id (vortel-lsp-hash-get message "id"))
         (id-key (vortel-lsp-client--id-key id))
         (entry (gethash id-key (vortel-lsp-client-pending client))))
    (if (not entry)
        (vortel-lsp-log "dropping response without pending request (%s id=%s)"
                        (vortel-lsp-client-name client)
                        id-key)
      (remhash id-key (vortel-lsp-client-pending client))
      (when-let* ((timer (plist-get entry :timer)))
        (cancel-timer timer))
      (if (vortel-lsp-hash-get message "error")
          (when-let* ((on-error (plist-get entry :on-error)))
            (funcall on-error (vortel-lsp-hash-get message "error")))
        (when-let* ((on-success (plist-get entry :on-success)))
          (funcall on-success (vortel-lsp-hash-get message "result")))))))

(defun vortel-lsp-client--request-timeout (client id-key)
  "Handle timeout for CLIENT request ID-KEY."
  (let ((entry (gethash id-key (vortel-lsp-client-pending client))))
    (when entry
      (remhash id-key (vortel-lsp-client-pending client))
      (when-let* ((on-error (plist-get entry :on-error)))
        (funcall on-error
                 (vortel-lsp-make-hash
                  "code" -32001
                  "message"
                  (format "request timed out: %s" (plist-get entry :method))))))))

(defun vortel-lsp-client--handle-message (client message)
  "Handle inbound MESSAGE for CLIENT."
  (let ((method (vortel-lsp-hash-get message "method" :none))
        (id (vortel-lsp-hash-get message "id" :none)))
    (cond
     ((not (eq method :none))
      (if (eq id :none)
          (vortel-lsp-client--call-notification-handlers
           client
           method
           (vortel-lsp-hash-get message "params"))
        (vortel-lsp-client--dispatch-request
         client
         id
         method
         (vortel-lsp-hash-get message "params"))))
     (t
      (vortel-lsp-client--complete-request client message)))))

(defun vortel-lsp-client--transport-exited (client event)
  "Handle CLIENT transport exit EVENT."
  (vortel-lsp-log "server exited (%s): %s" (vortel-lsp-client-name client) event)
  (vortel-lsp-client--clear-file-watch-registrations client)
  (let (ids)
    (maphash (lambda (k _v) (push k ids)) (vortel-lsp-client-pending client))
    (dolist (id-key ids)
      (let ((entry (gethash id-key (vortel-lsp-client-pending client))))
        (when-let* ((timer (plist-get entry :timer)))
          (cancel-timer timer))
        (when-let* ((on-error (plist-get entry :on-error)))
          (funcall on-error (vortel-lsp-make-hash
                             "code" -32000
                             "message" "server stream closed")))))
    (clrhash (vortel-lsp-client-pending client)))
  (vortel-lsp-client--set-state client 'exited)
  (vortel-lsp-client--call-notification-handlers client "exit" nil))

(defun vortel-lsp-client--initialize-capabilities ()
  "Return initialize capabilities payload."
  (vortel-lsp-make-hash
   "workspace"
   (vortel-lsp-make-hash
    "configuration" t
    "workspaceFolders" t
    "didChangeWatchedFiles" (vortel-lsp-make-hash "dynamicRegistration" t)
    "applyEdit" t)
   "textDocument"
   (vortel-lsp-make-hash
    "hover" (vortel-lsp-make-hash "contentFormat" (list "markdown" "plaintext"))
    "signatureHelp" (vortel-lsp-make-hash)
    "completion" (vortel-lsp-make-hash
                  "completionItem"
                  (vortel-lsp-make-hash
                    "snippetSupport" t
                    "deprecatedSupport" t
                    "documentationFormat" (list "markdown" "plaintext")
                    "resolveSupport"
                    (vortel-lsp-make-hash
                     "properties"
                     (list "documentation" "detail" "additionalTextEdits"))))
     "publishDiagnostics" (vortel-lsp-make-hash "versionSupport" t))
   "window" (vortel-lsp-make-hash "workDoneProgress" t)
   "general" (vortel-lsp-make-hash
              "positionEncodings" (list "utf-8" "utf-32" "utf-16"))))

(defun vortel-lsp-client--initialize-params (client)
  "Build initialize params for CLIENT."
  (let* ((root-path (vortel-lsp-client-root-path client))
         (root-uri (vortel-lsp-client-root-uri client))
         (workspace-name (file-name-nondirectory (directory-file-name root-path))))
    (vortel-lsp-make-hash
     "processId" (emacs-pid)
     "rootUri" root-uri
     "rootPath" root-path
     "workspaceFolders"
     (list (vortel-lsp-make-hash "name" workspace-name "uri" root-uri))
     "initializationOptions" (vortel-lsp-client-initialization-options client)
     "capabilities" (vortel-lsp-client--initialize-capabilities)
     "clientInfo"
     (vortel-lsp-make-hash "name" "vortel-lsp" "version" "0.1.0"))))

(defun vortel-lsp-client--begin-initialize (client)
  "Start initialize sequence for CLIENT."
  (vortel-lsp-client-request
   client
   "initialize"
   (vortel-lsp-client--initialize-params client)
    :on-success
    (lambda (result)
      (setf (vortel-lsp-client-capabilities client)
            (vortel-lsp-hash-get result "capabilities")
            (vortel-lsp-client-server-info client)
            (vortel-lsp-hash-get result "serverInfo"))
      (vortel-lsp-client-notify client "initialized" (vortel-lsp-make-hash))
      (vortel-lsp-client--set-state client 'ready)
      (vortel-lsp-client--flush-queue client))
   :on-error
   (lambda (error)
     (vortel-lsp-log "initialize failed (%s): %s"
                     (vortel-lsp-client-name client)
                     (if (hash-table-p error)
                         (vortel-lsp-hash-get error "message")
                       error))
     (vortel-lsp-client--set-state client 'failed)
     (vortel-lsp-client-force-stop client))))

(cl-defun vortel-lsp-client-start
    (&key id name command args root-path root-uri initialization-options timeout environment settings)
  "Start and return a `vortel-lsp-client' instance."
  (unless (and command (stringp command) (not (string-empty-p command)))
    (error "invalid language server command for %s" name))
  (unless root-uri
    (setq root-uri (vortel-lsp-path-to-uri root-path)))
  (let* ((client
          (vortel-lsp-client--create
           :id id
           :name name
           :command command
           :args args
           :root-path root-path
            :root-uri root-uri
            :initialization-options initialization-options
            :timeout (or timeout vortel-lsp-default-request-timeout)
            :environment environment
            :settings settings
            :pending (make-hash-table :test #'equal)
            :send-queue nil
            :watched-file-registrations (make-hash-table :test #'equal)
            :notification-handlers nil
            :request-handlers nil
            :state-handlers nil
            :dynamic-capabilities (make-hash-table :test #'equal)))
         (transport
          (vortel-lsp-transport-start
           :name name
           :command command
           :args args
           :cwd root-path
           :environment environment
           :on-message (lambda (_transport message)
                         (vortel-lsp-client--handle-message client message))
           :on-exit (lambda (_transport event)
                      (vortel-lsp-client--transport-exited client event))
           :on-stderr (lambda (_transport chunk)
                        (vortel-lsp-log "%s stderr: %s"
                                        (vortel-lsp-client-name client)
                                        (string-trim-right chunk))))))
    (setf (vortel-lsp-client-transport client) transport)
    (vortel-lsp-client--begin-initialize client)
    client))

(cl-defun vortel-lsp-client-request
    (client method params &key on-success on-error timeout)
  "Send request METHOD with PARAMS via CLIENT.
Callbacks ON-SUCCESS and ON-ERROR are optional.
Return the numeric request ID."
  (let* ((id (vortel-lsp-client--next-id client))
         (payload (vortel-lsp-make-hash
                   "jsonrpc" "2.0"
                   "id" id
                   "method" method))
         (id-key (vortel-lsp-client--id-key id))
         (timeout-seconds (or timeout (vortel-lsp-client-timeout client)
                             vortel-lsp-default-request-timeout))
         (timer
          (run-at-time
           timeout-seconds
           nil
           #'vortel-lsp-client--request-timeout
           client
           id-key)))
    (unless (null params)
      (puthash "params" params payload))
    (puthash id-key
             (list :method method
                   :on-success on-success
                   :on-error on-error
                   :timer timer)
             (vortel-lsp-client-pending client))
    (vortel-lsp-client--send-payload client payload)
    id))

(defun vortel-lsp-client-notify (client method params)
  "Send notification METHOD with PARAMS via CLIENT."
  (let ((payload (vortel-lsp-make-hash "jsonrpc" "2.0" "method" method)))
    (unless (null params)
      (puthash "params" params payload))
    (vortel-lsp-client--send-payload client payload)))

(defun vortel-lsp-client-reply (client id result)
  "Send JSON-RPC success reply with ID and RESULT via CLIENT."
  (let ((payload (vortel-lsp-make-hash
                  "jsonrpc" "2.0"
                  "id" id
                  "result" result)))
    (vortel-lsp-client--send-payload client payload)))

(defun vortel-lsp-client-reply-error (client id error)
  "Send JSON-RPC error reply with ID and ERROR via CLIENT.
ERROR can be a hash-table payload or a message string."
  (let ((payload (vortel-lsp-make-hash
                  "jsonrpc" "2.0"
                  "id" id
                  "error"
                  (if (hash-table-p error)
                      error
                    (vortel-lsp-make-hash "code" -32603 "message" (format "%s" error))))))
    (vortel-lsp-client--send-payload client payload)))

(defun vortel-lsp-client-force-stop (client)
  "Force stop CLIENT transport."
  (vortel-lsp-client--clear-file-watch-registrations client)
  (when (vortel-lsp-client-transport client)
    (vortel-lsp-transport-stop (vortel-lsp-client-transport client))))

(defun vortel-lsp-client-shutdown (client)
  "Attempt graceful shutdown for CLIENT."
  (when (vortel-lsp-client-live-p client)
    (vortel-lsp-client-request
     client
     "shutdown"
     nil
     :on-success (lambda (_result)
                   (vortel-lsp-client-notify client "exit" nil)
                   (vortel-lsp-client-force-stop client))
     :on-error (lambda (_error)
                 (vortel-lsp-client-notify client "exit" nil)
                 (vortel-lsp-client-force-stop client)))))

(defun vortel-lsp-client--sync-kind (client)
  "Return sync kind for CLIENT as one of `none', `full', `incremental'."
  (let* ((caps (vortel-lsp-client-capabilities client))
         (sync-cap (vortel-lsp-hash-get caps "textDocumentSync")))
    (cond
     ((numberp sync-cap)
       (pcase sync-cap
         (0 'none)
         (1 'full)
         (2 'incremental)
         (_ 'incremental)))
     ((hash-table-p sync-cap)
       (let ((change (vortel-lsp-hash-get sync-cap "change")))
         (pcase change
           (0 'none)
           (1 'full)
           (2 'incremental)
           (_ 'incremental))))
     (t
      'incremental))))

(defun vortel-lsp-client-did-open (client uri version text language-id)
  "Send textDocument/didOpen notification."
  (vortel-lsp-client-notify
   client
   "textDocument/didOpen"
   (vortel-lsp-make-hash
    "textDocument"
    (vortel-lsp-make-hash
     "uri" uri
     "languageId" language-id
     "version" version
     "text" text))))

(defun vortel-lsp-client-did-change (client uri version full-text incremental-changes)
  "Send textDocument/didChange based on CLIENT sync kind."
  (let ((sync-kind (vortel-lsp-client--sync-kind client)))
    (pcase sync-kind
      ('none nil)
      ('full
       (vortel-lsp-client-notify
        client
        "textDocument/didChange"
        (vortel-lsp-make-hash
         "textDocument" (vortel-lsp-make-hash "uri" uri "version" version)
         "contentChanges" (list (vortel-lsp-make-hash "text" full-text)))))
      (_
       (vortel-lsp-client-notify
        client
        "textDocument/didChange"
        (vortel-lsp-make-hash
         "textDocument" (vortel-lsp-make-hash "uri" uri "version" version)
         "contentChanges" incremental-changes))))))

(defun vortel-lsp-client-did-save (client uri text)
  "Send textDocument/didSave notification.
TEXT is included only when the server asks for it."
  (let* ((caps (vortel-lsp-client-capabilities client))
          (sync-cap (vortel-lsp-hash-get caps "textDocumentSync"))
          (save-cap (and (hash-table-p sync-cap) (vortel-lsp-hash-get sync-cap "save")))
          (save-enabled
           (or (numberp sync-cap)
               (and (hash-table-p sync-cap)
                    (or (hash-table-p save-cap)
                        (vortel-lsp-truthy-p save-cap)))))
          (include-text
           (and (hash-table-p save-cap)
                (vortel-lsp-truthy-p (vortel-lsp-hash-get save-cap "includeText"))))
          (params (vortel-lsp-make-hash
                   "textDocument" (vortel-lsp-make-hash "uri" uri))))
    (when save-enabled
      (when include-text
        (puthash "text" text params))
      (vortel-lsp-client-notify client "textDocument/didSave" params))))

(defun vortel-lsp-client-did-close (client uri)
  "Send textDocument/didClose notification."
  (vortel-lsp-client-notify
   client
   "textDocument/didClose"
   (vortel-lsp-make-hash
    "textDocument" (vortel-lsp-make-hash "uri" uri))))

(defun vortel-lsp-client-completion-resolve-supported-p (client)
  "Return non-nil when CLIENT supports `completionItem/resolve'."
  (let* ((caps (vortel-lsp-client-capabilities client))
         (completion-provider (vortel-lsp-hash-get caps "completionProvider")))
    (and (hash-table-p completion-provider)
         (vortel-lsp-truthy-p
          (vortel-lsp-hash-get completion-provider "resolveProvider")))))

(defun vortel-lsp-client-code-action-resolve-supported-p (client)
  "Return non-nil when CLIENT supports `codeAction/resolve'."
  (let* ((caps (vortel-lsp-client-capabilities client))
         (code-action-provider (vortel-lsp-hash-get caps "codeActionProvider")))
    (and (hash-table-p code-action-provider)
         (vortel-lsp-truthy-p
          (vortel-lsp-hash-get code-action-provider "resolveProvider")))))

(defun vortel-lsp-client-supports (client feature)
  "Return non-nil when CLIENT supports FEATURE.
FEATURE is a Helix-style feature name string, like `hover' or `goto-definition'."
  (let* ((caps (vortel-lsp-client-capabilities client))
         (dynamic (vortel-lsp-client-dynamic-capabilities client))
         (dynamic-methods (alist-get feature vortel-lsp--feature-method-map nil nil #'string=))
         (dynamic-supported
          (and (hash-table-p dynamic)
               (cl-some (lambda (method) (gethash method dynamic)) dynamic-methods)))
         (feature-value
          (pcase feature
            ("hover" (vortel-lsp-hash-get caps "hoverProvider"))
            ("goto-definition" (vortel-lsp-hash-get caps "definitionProvider"))
            ("goto-declaration" (vortel-lsp-hash-get caps "declarationProvider"))
            ("goto-type-definition" (vortel-lsp-hash-get caps "typeDefinitionProvider"))
            ("goto-reference" (vortel-lsp-hash-get caps "referencesProvider"))
            ("goto-implementation" (vortel-lsp-hash-get caps "implementationProvider"))
            ("signature-help" (vortel-lsp-hash-get caps "signatureHelpProvider"))
            ("completion" (vortel-lsp-hash-get caps "completionProvider"))
            ("code-action" (vortel-lsp-hash-get caps "codeActionProvider"))
            ("document-symbols" (vortel-lsp-hash-get caps "documentSymbolProvider"))
            ("workspace-symbols" (vortel-lsp-hash-get caps "workspaceSymbolProvider"))
            ("rename-symbol" (vortel-lsp-hash-get caps "renameProvider"))
            ("format" (vortel-lsp-hash-get caps "documentFormattingProvider"))
            ("inlay-hints" (vortel-lsp-hash-get caps "inlayHintProvider"))
            ("document-colors" (vortel-lsp-hash-get caps "colorProvider"))
             ;; diagnostics support is effectively always available in push mode
             ("diagnostics" t)
             (_ nil))))
    (or dynamic-supported
        (vortel-lsp-truthy-p feature-value))))

(provide 'vortel-lsp-client)

;;; vortel-lsp-client.el ends here
