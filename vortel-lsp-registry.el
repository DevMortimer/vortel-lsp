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
(require 'vortel-lsp-util)

(defvar vortel-lsp-registry--clients (make-hash-table :test #'equal))
(defvar vortel-lsp-registry--client-keys (make-hash-table :test #'eq))
(defvar vortel-lsp-registry--next-id 0)

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
         (initialization-options (plist-get definition :initialization-options)))
    (if (not (and definition command))
        (progn
          (vortel-lsp-log "missing server definition for %s" server-name)
          nil)
      (let* ((client
              (vortel-lsp-client-start
               :id (vortel-lsp-registry--next-client-id)
               :name server-name
               :command command
               :args args
               :root-path root
               :initialization-options initialization-options
               :timeout timeout
               :environment environment)))
        (vortel-lsp-client-add-state-handler
         client
         #'vortel-lsp-registry--cleanup-on-state-change)
        client))))

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
