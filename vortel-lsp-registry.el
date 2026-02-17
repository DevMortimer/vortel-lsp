;;; vortel-lsp-registry.el --- Shared client registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors
;; Keywords: languages, tools

;;; Commentary:

;; Client registry keyed by (server-name, workspace-root).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'vortel-lsp-client)
(require 'vortel-lsp-config)
(require 'vortel-lsp-transport)
(require 'vortel-lsp-util)

(defcustom vortel-lsp-missing-server-policy 'warn-and-skip
  "What to do when a configured language server executable is missing.

`warn-and-skip' shows a warning once per (server, workspace) and skips it.
`silent-skip' skips it without any user-visible message.
`error' raises an error when the server cannot be started."
  :type '(choice (const :tag "Warn and skip" warn-and-skip)
                 (const :tag "Silent skip" silent-skip)
                 (const :tag "Error" error))
  :group 'vortel-lsp)

(defcustom vortel-lsp-preferred-servers nil
  "Preferred server allowlist per language.

Alist mapping language name string to a list of server name strings.
When a language has an entry here, only those servers are considered,
and they are tried in the order specified.

Example:
  \='((\"python\" . (\"ty\"))
     (\"typescript\" . (\"typescript-language-server\")))"
  :type '(alist :key-type string :value-type (repeat string))
  :group 'vortel-lsp)

(defcustom vortel-lsp-disabled-servers nil
  "Server denylist.

List of server name strings that should never be started."
  :type '(repeat string)
  :group 'vortel-lsp)

(defvar vortel-lsp-registry--clients (make-hash-table :test #'equal))
(defvar vortel-lsp-registry--client-keys (make-hash-table :test #'eq))
(defvar vortel-lsp-registry--next-id 0)
(defvar vortel-lsp-registry--missing-warnings (make-hash-table :test #'equal))

(defun vortel-lsp-registry-clear ()
  "Clear in-memory client registry tables.
This does not stop live clients."
  (interactive)
  (clrhash vortel-lsp-registry--clients)
  (clrhash vortel-lsp-registry--client-keys)
  (setq vortel-lsp-registry--next-id 0))

(defun vortel-lsp-registry--key (server-name root)
  "Build registry key for SERVER-NAME and ROOT."
  (format "%s::%s" server-name (expand-file-name root)))

(defun vortel-lsp-registry--warn-missing-once (server-name root message-text)
  "Warn once per (SERVER-NAME, ROOT) with MESSAGE-TEXT."
  (let ((key (vortel-lsp-registry--key server-name root)))
    (unless (gethash key vortel-lsp-registry--missing-warnings)
      (puthash key t vortel-lsp-registry--missing-warnings)
      (display-warning 'vortel-lsp message-text :warning))))

(defun vortel-lsp-registry--filter-language-server-entries (language server-entries)
  "Filter SERVER-ENTRIES for LANGUAGE according to user preferences.

Applies `vortel-lsp-disabled-servers' and `vortel-lsp-preferred-servers'.
Returns a list of server entry hash-tables." 
  (let* ((lang-name (or (and (hash-table-p language)
                             (vortel-lsp-hash-get language "name"))
                        ""))
         (preferred (alist-get lang-name vortel-lsp-preferred-servers nil nil #'string=))
         (disabled vortel-lsp-disabled-servers)
         (entries
          (cl-remove-if
           (lambda (entry)
             (let ((name (vortel-lsp-hash-get entry "name")))
               (and (stringp name)
                    (member name disabled))))
           (or server-entries '()))))
    (if (and preferred (listp preferred))
        (let ((by-name (make-hash-table :test #'equal))
              (result nil))
          (dolist (entry entries)
            (let ((name (vortel-lsp-hash-get entry "name")))
              (when (stringp name)
                (puthash name entry by-name))))
          (dolist (name preferred)
            (when-let* ((entry (gethash name by-name)))
              (push entry result)))
          (nreverse result))
      entries)))

(defun vortel-lsp-registry--next-client-id ()
  "Allocate a monotonic client ID."
  (prog1 vortel-lsp-registry--next-id
    (setq vortel-lsp-registry--next-id (1+ vortel-lsp-registry--next-id))))

(defun vortel-lsp-registry--cleanup-on-state-change (client new-state _old-state)
  "Remove CLIENT from registry after terminal NEW-STATE."
  (when (memq new-state '(failed exited))
    (let ((key (gethash client vortel-lsp-registry--client-keys)))
      (when key
        (remhash key vortel-lsp-registry--clients)
        (remhash client vortel-lsp-registry--client-keys)))))

(defun vortel-lsp-registry--start-client (server-name root)
  "Start a client for SERVER-NAME at ROOT.
Return a `vortel-lsp-client' or nil when config is invalid."
  (let* ((definition (vortel-lsp-config-server-definition server-name))
          (command (plist-get definition :command))
          (args (plist-get definition :args))
          (timeout (plist-get definition :timeout))
          (environment (plist-get definition :environment))
          (initialization-options (plist-get definition :initialization-options))
          (settings (vortel-lsp-config-server-settings server-name)))
    (if (not (and definition command))
        (progn
          (vortel-lsp-log "missing server definition for %s" server-name)
          nil)
      (let* ((resolved (vortel-lsp-transport--resolve-command command root))
             (resolved-command (and resolved (plist-get resolved :command))))
        (cond
         ((not resolved-command)
          (let ((msg (vortel-lsp-transport--format-command-missing-error
                      server-name command root)))
            (pcase vortel-lsp-missing-server-policy
              ('error (error "%s" msg))
              ('warn-and-skip
               (vortel-lsp-registry--warn-missing-once server-name root msg)
               nil)
              (_ nil))))
         (t
          (let* ((client
                  (vortel-lsp-client-start
                   :id (vortel-lsp-registry--next-client-id)
                   :name server-name
                   :command resolved-command
                   :args args
                   :root-path root
                   :initialization-options initialization-options
                   :timeout timeout
                   :environment environment
                   :settings settings)))
            (vortel-lsp-client-add-state-handler
             client
             #'vortel-lsp-registry--cleanup-on-state-change)
            client)))))))

(defun vortel-lsp-registry-get-or-start (server-name root)
  "Return existing live client or start a new one.
SERVER-NAME and ROOT define the client identity key."
  (let* ((key (vortel-lsp-registry--key server-name root))
         (existing (gethash key vortel-lsp-registry--clients)))
    (if (and existing (vortel-lsp-client-live-p existing))
        existing
      (let ((client (vortel-lsp-registry--start-client server-name root)))
        (when client
          (puthash key client vortel-lsp-registry--clients)
          (puthash client key vortel-lsp-registry--client-keys))
        client))))

(defun vortel-lsp-registry-attachments-for-path (path)
  "Return client attachment records for PATH.
Each record is a plist with keys:
- :client
- :server-entry
- :language
- :root"
  (let* ((language (vortel-lsp-config-language-for-path path))
         (root (and language (vortel-lsp-config-find-root path language)))
         (server-entries (and language (vortel-lsp-config-language-servers language)))
         (server-entries (and language
                              (vortel-lsp-registry--filter-language-server-entries
                               language
                               server-entries)))
         attachments)
    (when language
      (dolist (entry server-entries)
        (let* ((server-name (vortel-lsp-hash-get entry "name"))
               (client (and server-name (vortel-lsp-registry-get-or-start server-name root))))
          (when client
            (push (list :client client
                        :server-entry entry
                        :language language
                        :root root)
                  attachments)))))
    (nreverse attachments)))

(defun vortel-lsp-registry-stop-all ()
  "Attempt graceful shutdown for all live clients."
  (interactive)
  (let (clients)
    (maphash (lambda (_key client) (push client clients)) vortel-lsp-registry--clients)
    (dolist (client clients)
      (ignore-errors (vortel-lsp-client-shutdown client)))))

(provide 'vortel-lsp-registry)

;;; vortel-lsp-registry.el ends here
