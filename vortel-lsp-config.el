;;; vortel-lsp-config.el --- Catalog and language resolution -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors
;; Keywords: languages, tools

;;; Commentary:

;; Loads generated Helix catalog data and resolves language/server config.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(require 'vortel-lsp-util)

(defcustom vortel-lsp-catalog-file
  (expand-file-name
   "vortel-lsp-catalog.json"
   (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Path to generated `vortel-lsp-catalog.json'."
  :type 'file
  :group 'vortel-lsp)

(defcustom vortel-lsp-server-overrides nil
  "Server definition overrides.
This is an alist keyed by server name string. Values are plists.
Supported plist keys:
- :command
- :args
- :timeout
- :environment
- :initialization-options"
  :type '(alist :key-type string :value-type plist)
  :group 'vortel-lsp)

(defcustom vortel-lsp-extra-servers nil
  "Extra servers to attach per language.
Alist mapping language name strings to lists of server entry plists.
Each entry plist requires :name and optionally :only/:except for feature filtering.

Example:
  \\='((\"svelte\" . ((:name \"tailwindcss-ls\")))
    (\"typescript\" . ((:name \"tailwindcss-ls\"))))"
  :type '(alist :key-type string
                :value-type (repeat (plist :key-type symbol :value-type sexp)))
  :group 'vortel-lsp)

(defvar vortel-lsp--catalog-cache nil)
(defvar vortel-lsp--glob-regex-cache (make-hash-table :test #'equal))

(defun vortel-lsp-config-clear-cache ()
  "Clear internal catalog and glob caches."
  (interactive)
  (setq vortel-lsp--catalog-cache nil)
  (clrhash vortel-lsp--glob-regex-cache))

(defun vortel-lsp-config-catalog ()
  "Return cached catalog hash-table loaded from `vortel-lsp-catalog-file'."
  (or vortel-lsp--catalog-cache
      (setq vortel-lsp--catalog-cache
            (with-temp-buffer
              (insert-file-contents vortel-lsp-catalog-file)
              (let ((json-false vortel-lsp--json-false))
                (json-parse-buffer
                 :object-type 'hash-table
                 :array-type 'list
                 :null-object nil
                 :false-object json-false))))))

(defun vortel-lsp-config-languages ()
  "Return catalog language list."
  (vortel-lsp-hash-get (vortel-lsp-config-catalog) "languages"))

(defun vortel-lsp-config-servers ()
  "Return catalog server map."
  (vortel-lsp-hash-get (vortel-lsp-config-catalog) "servers"))

(defun vortel-lsp-config-language-id (language)
  "Return LSP language id for LANGUAGE entry hash-table."
  (or (vortel-lsp-hash-get language "language_id")
      (vortel-lsp-hash-get language "name")
      ""))

(defun vortel-lsp-config--extra-server-to-hash (plist)
  "Convert a user PLIST server entry to a catalog-compatible hash-table.
PLIST must contain :name, and may contain :only and :except."
  (let ((ht (make-hash-table :test #'equal)))
    (puthash "name" (plist-get plist :name) ht)
    (when (plist-member plist :only)
      (puthash "only" (plist-get plist :only) ht))
    (when (plist-member plist :except)
      (puthash "except" (plist-get plist :except) ht))
    ht))

(defun vortel-lsp-config-language-servers (language)
  "Return server entries for LANGUAGE entry hash-table.
Merges catalog entries with `vortel-lsp-extra-servers', deduplicating
by server name (catalog entries take priority)."
  (let* ((catalog-servers (or (vortel-lsp-hash-get language "servers") '()))
         (lang-name (or (vortel-lsp-hash-get language "name") ""))
         (extras-raw (alist-get lang-name vortel-lsp-extra-servers nil nil #'string=))
         (extras (mapcar #'vortel-lsp-config--extra-server-to-hash
                         (or extras-raw '())))
         (seen (make-hash-table :test #'equal))
         (result '()))
    (dolist (srv catalog-servers)
      (let ((name (vortel-lsp-hash-get srv "name")))
        (unless (gethash name seen)
          (puthash name t seen)
          (push srv result))))
    (dolist (srv extras)
      (let ((name (vortel-lsp-hash-get srv "name")))
        (unless (gethash name seen)
          (puthash name t seen)
          (push srv result))))
    (nreverse result)))

(defun vortel-lsp-config-server-definition (server-name)
  "Resolve SERVER-NAME into a plist with merged overrides.
Return nil when the server is not defined."
  (let* ((base (vortel-lsp-hash-get (vortel-lsp-config-servers) server-name))
         (override (alist-get server-name vortel-lsp-server-overrides nil nil #'string=)))
    (when base
      (list
       :name server-name
       :command (or (plist-get override :command)
                    (vortel-lsp-hash-get base "command"))
       :args (or (plist-get override :args)
                 (vortel-lsp-hash-get base "args")
                 '())
       :timeout (or (plist-get override :timeout)
                    (vortel-lsp-hash-get base "timeout")
                    20)
       :environment (or (plist-get override :environment)
                        (vortel-lsp-hash-get base "environment"))
       :initialization-options
       (or (plist-get override :initialization-options)
           (vortel-lsp-hash-get base "config"))))))

(defun vortel-lsp-config--file-extension-or-name (path)
  "Return values (basename extension) for PATH."
  (list (file-name-nondirectory path)
        (file-name-extension path)))

(defun vortel-lsp-config--expand-braces (pattern)
  "Expand a single-level shell brace expression in PATTERN.
Returns a list of expanded patterns."
  (if (string-match "{\\([^{}]+\\)}" pattern)
      (let* ((start (substring pattern 0 (match-beginning 0)))
             (end (substring pattern (match-end 0)))
             (items (split-string (match-string 1 pattern) "," t)))
        (apply #'append
               (mapcar (lambda (item)
                         (vortel-lsp-config--expand-braces
                          (concat start item end)))
                       items)))
    (list pattern)))

(defun vortel-lsp-config--glob-regexps (pattern)
  "Return compiled regex list for glob PATTERN."
  (or (gethash pattern vortel-lsp--glob-regex-cache)
      (let* ((glob (if (or (string-prefix-p "/" pattern)
                           (string-prefix-p "*/" pattern))
                       pattern
                     (concat "*/" pattern)))
             (expanded (vortel-lsp-config--expand-braces glob))
             (regexps (mapcar #'wildcard-to-regexp expanded)))
        (puthash pattern regexps vortel-lsp--glob-regex-cache)
        regexps)))

(defun vortel-lsp-config--glob-match-p (pattern path)
  "Return non-nil when glob PATTERN matches PATH."
  (let ((basename (file-name-nondirectory path)))
    (cl-some
     (lambda (regexp)
       (or (string-match-p regexp path)
           (string-match-p regexp basename)))
     (vortel-lsp-config--glob-regexps pattern))))

(defun vortel-lsp-config--language-match-score (language path)
  "Return match score for LANGUAGE against PATH.
Higher score wins. Returns -1 for no match."
  (let* ((file-types (or (vortel-lsp-hash-get language "file_types") '()))
         (base-and-ext (vortel-lsp-config--file-extension-or-name path))
         (base (car base-and-ext))
         (extension (cadr base-and-ext))
         (best -1))
    (dolist (entry file-types best)
      (let ((ext (vortel-lsp-hash-get entry "ext"))
            (glob (vortel-lsp-hash-get entry "glob")))
        (cond
         ((and ext
               (or (and extension (string= ext extension))
                   (string= ext base)))
          (setq best (max best (length ext))))
         ((and glob (vortel-lsp-config--glob-match-p glob path))
          (setq best (max best (length glob)))))))))

(defun vortel-lsp-config-language-for-path (path)
  "Resolve PATH to the best language entry.
Returns nil when no language entry matches."
  (let ((absolute (expand-file-name path))
        (best-language nil)
        (best-score -1))
    (dolist (language (vortel-lsp-config-languages) best-language)
      (let ((score (vortel-lsp-config--language-match-score language absolute)))
        (when (> score best-score)
          (setq best-score score
                best-language language))))))

(defun vortel-lsp-config--parent-directory (dir)
  "Return parent directory for DIR, or nil when DIR is root."
  (let* ((normalized (directory-file-name (expand-file-name dir)))
         (parent (file-name-directory normalized)))
    (unless (or (null parent)
                (equal normalized (directory-file-name parent)))
      parent)))

(defun vortel-lsp-config-find-workspace (path)
  "Find workspace root for PATH.
Looks for `.git', `.svn', `.jj', or `.helix' upward.
Returns the first matching ancestor, or PATH's directory when none is found."
  (let ((dir (if (file-directory-p path)
                 (expand-file-name path)
               (file-name-directory (expand-file-name path))))
        (found nil))
    (while (and dir (not found))
      (if (or (file-directory-p (expand-file-name ".git" dir))
              (file-directory-p (expand-file-name ".svn" dir))
              (file-directory-p (expand-file-name ".jj" dir))
              (file-directory-p (expand-file-name ".helix" dir)))
          (setq found dir)
        (setq dir (vortel-lsp-config--parent-directory dir))))
    (or found
        (if (file-directory-p path)
            (expand-file-name path)
          (file-name-directory (expand-file-name path))))))

(defun vortel-lsp-config--directory-has-root-marker-p (dir markers)
  "Return non-nil when DIR contains any of MARKERS.
MARKERS can include literal names or globs."
  (cl-some
   (lambda (marker)
     (let ((matches (file-expand-wildcards (expand-file-name marker dir))))
       (and matches (not (null matches)))))
   markers))

(defun vortel-lsp-config-find-root (path language)
  "Resolve workspace root for PATH using LANGUAGE root markers.
Returns a normalized absolute directory path."
  (let* ((workspace (vortel-lsp-config-find-workspace path))
         (markers (or (vortel-lsp-hash-get language "roots") '()))
         (dir (if (file-directory-p path)
                  (expand-file-name path)
                (file-name-directory (expand-file-name path))))
         (top-marker nil)
         (done nil))
    (while (and dir (not done))
      (when (and markers
                 (vortel-lsp-config--directory-has-root-marker-p dir markers))
        (setq top-marker dir))
      (if (equal (directory-file-name dir)
                 (directory-file-name workspace))
          (setq done t)
        (setq dir (vortel-lsp-config--parent-directory dir))))
    (expand-file-name (or top-marker workspace))))

(provide 'vortel-lsp-config)

;;; vortel-lsp-config.el ends here
