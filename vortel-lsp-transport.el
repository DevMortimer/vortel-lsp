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

(defcustom vortel-lsp-local-bin-directories
  '(".venv/bin" "venv/bin" "node_modules/.bin")
  "Project-local bin directories checked for server executables.
Each value is relative to the server working directory."
  :type '(repeat directory)
  :group 'vortel-lsp)

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

(defun vortel-lsp-transport--prepend-path-entry (env dir)
  "Return ENV with DIR prepended to PATH if needed."
  (if (or (null dir) (string-empty-p dir))
      env
    (let* ((path-entry (cl-find-if
                        (lambda (entry) (string-prefix-p "PATH=" entry))
                        env))
           (existing-path (and path-entry (substring path-entry 5))))
      (if (and existing-path
               (member dir (split-string existing-path path-separator t)))
          env
        (let ((new-path (if existing-path
                            (concat "PATH=" dir path-separator existing-path)
                          (concat "PATH=" dir))))
          (cons new-path
                (cl-remove-if
                 (lambda (entry) (string-prefix-p "PATH=" entry))
                 env)))))))

(defun vortel-lsp-transport--find-project-command (command dir)
  "Find COMMAND inside `vortel-lsp-local-bin-directories' for DIR.
Return an absolute executable path when found, or nil otherwise."
  (let ((root (expand-file-name (or dir default-directory)))
        (found nil))
    (dolist (relative-dir vortel-lsp-local-bin-directories found)
      (let* ((bin-dir (expand-file-name relative-dir root))
             (candidate (expand-file-name command bin-dir)))
        (when (and (file-directory-p bin-dir)
                   (file-executable-p candidate)
                   (not found))
          (setq found candidate))))))

(defun vortel-lsp-transport--resolve-command (command dir)
  "Resolve COMMAND for process start rooted at DIR.
Return plist with keys `:command' and `:bin-dir' on success.
Return nil when COMMAND cannot be resolved."
  (cond
   ((or (not (stringp command)) (string-empty-p command))
    nil)
   ((file-name-absolute-p command)
    (and (file-executable-p command)
         (list :command command
               :bin-dir (file-name-directory command))))
   ((file-name-directory command)
    (let ((absolute (expand-file-name command (or dir default-directory))))
      (and (file-executable-p absolute)
           (list :command absolute
                 :bin-dir (file-name-directory absolute)))))
   (t
    (let* ((global (executable-find command))
           (project-local (and (not global)
                               (vortel-lsp-transport--find-project-command command dir)))
           (resolved (or global project-local)))
      (and resolved
           (list :command resolved
                 :bin-dir (file-name-directory resolved)))))))

(defun vortel-lsp-transport--format-command-missing-error (name command cwd)
  "Build a readable error string when COMMAND can't be found.
NAME is the server name and CWD is the intended working directory."
  (format
   (concat "unable to start LSP server `%s`: executable `%s` was not found\n"
           "workspace: %s\n"
           "searched project bins: %s\n"
           "hints: run your dependency install/sync command (for example `uv sync`), "
           "ensure `direnv allow` has been applied, or set `vortel-lsp-server-overrides` "
           "with an absolute :command")
   (or name command)
   command
   (expand-file-name (or cwd default-directory))
   (mapconcat #'identity vortel-lsp-local-bin-directories ", ")))

(defun vortel-lsp-transport--maybe-prepend-node-bin (env dir)
  "Return ENV with node_modules/.bin prepended to PATH.
Searches DIR and parent directories for the bin folder."
  (let ((bin-dir (vortel-lsp-transport--find-node-bin dir)))
    (if bin-dir
        (vortel-lsp-transport--prepend-path-entry env bin-dir)
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
         (resolved-command (vortel-lsp-transport--resolve-command command cwd))
         (resolved-command-path (plist-get resolved-command :command))
         (resolved-command-bin (plist-get resolved-command :bin-dir))
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
            (setq merged (vortel-lsp-transport--prepend-path-entry
                          merged
                          resolved-command-bin))
            (if node-bin-dir
                (vortel-lsp-transport--maybe-prepend-node-bin
                 merged
                 (or cwd default-directory))
              merged)))
         (exec-path (let ((with-command-bin
                           (if resolved-command-bin
                               (cons resolved-command-bin exec-path)
                             exec-path)))
                      (if node-bin-dir
                          (cons node-bin-dir with-command-bin)
                        with-command-bin)))
         (default-directory
          (file-name-as-directory (expand-file-name (or cwd default-directory))))
         (process
          (if (not resolved-command-path)
              (error "%s"
                     (vortel-lsp-transport--format-command-missing-error
                      name command cwd))
            (condition-case nil
                (make-process
                 :name process-name
                 :command (append (list resolved-command-path) args)
                 :connection-type 'pipe
                 :coding 'no-conversion
                 :stderr stderr-process
                 :noquery t
                 :file-handler t
                 :buffer nil)
              (file-missing
               (error "%s"
                      (vortel-lsp-transport--format-command-missing-error
                       name command cwd)))))))
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
