;;; vortel-lsp-transport.el --- Process transport for vortel-lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors
;; Keywords: languages, tools

;;; Commentary:

;; Asynchronous stdio transport for LSP with JSON-RPC framing.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'vortel-lsp-jsonrpc)
(require 'vortel-lsp-util)

(cl-defstruct (vortel-lsp-transport
               (:constructor vortel-lsp-transport--create))
  "Process transport for a single language server."
  name
  process
  stderr-process
  parser
  on-message
  on-exit
  on-stderr)

(defun vortel-lsp-transport-live-p (transport)
  "Return non-nil when TRANSPORT process is live."
  (process-live-p (vortel-lsp-transport-process transport)))

(defun vortel-lsp-transport--normalize-env (environment)
  "Normalize ENVIRONMENT into an alist of (KEY . VALUE) strings."
  (cond
   ((null environment) nil)
   ((hash-table-p environment)
    (vortel-lsp-hash-to-alist environment))
   ((and (listp environment)
         (cl-every #'consp environment))
    environment)
   (t
    (error "Unsupported environment type: %S" environment))))

(defun vortel-lsp-transport--find-node-bin (dir)
  "Search up from DIR for node_modules/.bin directory.
Returns the path to node_modules/.bin if found, nil otherwise."
  (let ((current (expand-file-name dir))
        found)
    (while (and current (not (string= current "/")) (not found))
      (let ((bin-dir (expand-file-name "node_modules/.bin" current)))
        (if (file-directory-p bin-dir)
            (setq found bin-dir)
          (setq current (file-name-directory (directory-file-name current))))))
    found))

(defun vortel-lsp-transport--maybe-prepend-node-bin (env dir)
  "Return ENV with node_modules/.bin prepended to PATH if it exists under DIR or parent dirs."
  (let ((bin-dir (vortel-lsp-transport--find-node-bin dir)))
    (if bin-dir
        (let* ((path-entry (cl-find-if
                            (lambda (e) (string-prefix-p "PATH=" e))
                            env))
               (new-path (if path-entry
                             (concat "PATH=" bin-dir ":" (substring path-entry 5))
                           (concat "PATH=" bin-dir))))
          (cons new-path
                (cl-remove-if
                 (lambda (e) (string-prefix-p "PATH=" e))
                 env)))
      env)))

(cl-defun vortel-lsp-transport-start
    (&key name command args cwd environment on-message on-exit on-stderr)
  "Start an LSP transport.
NAME is used for process naming.
COMMAND and ARGS define the process invocation.
CWD sets the process working directory.
ENVIRONMENT provides extra environment variables.
ON-MESSAGE is called as (fn transport message).
ON-EXIT is called as (fn transport event).
ON-STDERR is called as (fn transport chunk)."
  (let* ((process-name (format "vortel-lsp[%s]" name))
         (stderr-name (format "%s-stderr" process-name))
         (parser (vortel-lsp-jsonrpc-parser-create))
         (transport nil)
         (stderr-process
          (make-pipe-process
           :name stderr-name
           :noquery t
           :coding 'utf-8-unix
           :buffer nil
           :filter
           (lambda (_proc chunk)
             (when transport
               (when on-stderr
                 (funcall on-stderr transport chunk))))))
         (node-bin-dir (vortel-lsp-transport--find-node-bin
                        (or cwd default-directory)))
         (process-environment
          (let ((merged (copy-sequence process-environment)))
            (dolist (pair (vortel-lsp-transport--normalize-env environment))
              (let* ((key (format "%s" (car pair)))
                     (value (format "%s" (cdr pair)))
                     (prefix (concat key "="))
                     (entry (concat prefix value)))
                (setq merged
                      (cons entry
                            (cl-remove-if
                             (lambda (item)
                               (string-prefix-p prefix item))
                             merged)))))
            (if node-bin-dir
                (vortel-lsp-transport--maybe-prepend-node-bin
                 merged
                 (or cwd default-directory))
              merged)))
         (exec-path (if node-bin-dir
                        (cons node-bin-dir exec-path)
                      exec-path))
         (default-directory
          (file-name-as-directory (expand-file-name (or cwd default-directory))))
         (process
          (make-process
           :name process-name
           :command (append (list command) args)
           :connection-type 'pipe
           :coding 'no-conversion
           :stderr stderr-process
           :noquery t
           :file-handler t
           :buffer nil)))
    (set-process-query-on-exit-flag process nil)
    (setq transport
          (vortel-lsp-transport--create
           :name name
           :process process
           :stderr-process stderr-process
           :parser parser
           :on-message on-message
           :on-exit on-exit
           :on-stderr on-stderr))
    (set-process-filter
     process
     (lambda (_proc chunk)
       (condition-case err
           (let ((messages (vortel-lsp-jsonrpc-parser-feed parser chunk)))
              (dolist (message messages)
                (when vortel-lsp-log-io
                  (vortel-lsp-log "%s <- %s" process-name (json-encode message)))
                (when on-message
                  (funcall on-message transport message))))
         (error
          (when on-stderr
            (funcall on-stderr transport
                     (format "[%s] parser error: %s" process-name err)))))))
    (set-process-sentinel
     process
     (lambda (_proc event)
       (when on-exit
         (funcall on-exit transport event))))
    transport))

(defun vortel-lsp-transport-send (transport payload)
  "Send PAYLOAD through TRANSPORT.
Return non-nil when the payload was sent."
  (let ((process (vortel-lsp-transport-process transport)))
    (when (process-live-p process)
      (let ((framed (vortel-lsp-jsonrpc-encode-message payload)))
        (when vortel-lsp-log-io
          (vortel-lsp-log "%s -> %s"
                          (process-name process)
                          (if (hash-table-p payload)
                              (json-encode payload)
                            (format "%S" payload))))
        (process-send-string process framed)
        t))))

(defun vortel-lsp-transport-stop (transport)
  "Stop TRANSPORT and associated stderr process."
  (let ((process (vortel-lsp-transport-process transport))
        (stderr (vortel-lsp-transport-stderr-process transport)))
    (when (process-live-p process)
      (ignore-errors (delete-process process)))
    (when (and stderr (process-live-p stderr))
      (ignore-errors (delete-process stderr)))))

(provide 'vortel-lsp-transport)

;;; vortel-lsp-transport.el ends here
