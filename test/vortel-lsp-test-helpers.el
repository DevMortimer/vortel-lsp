;;; vortel-lsp-test-helpers.el --- Test helpers for vortel-lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors

;;; Code:

(require 'cl-lib)

(require 'vortel-lsp)

(cl-defun vortel-lsp-test-make-client
    (&key
     (id 1)
     (name "test-client")
     (command "test-server")
     (root-path default-directory)
     root-uri
     (state 'ready)
     capabilities
     dynamic-capabilities)
  "Create a minimal `vortel-lsp-client' object for tests."
  (vortel-lsp-client--create
   :id id
   :name name
   :command command
   :args nil
   :root-path root-path
   :root-uri (or root-uri (vortel-lsp-path-to-uri root-path))
   :initialization-options nil
   :timeout 1
   :environment nil
   :transport nil
   :state state
   :next-request-id 0
   :pending (make-hash-table :test #'equal)
   :send-queue nil
   :capabilities (or capabilities (vortel-lsp-make-hash))
   :dynamic-capabilities (or dynamic-capabilities (make-hash-table :test #'equal))
   :server-info nil
   :notification-handlers nil
   :request-handlers nil
   :state-handlers nil))

(defmacro vortel-lsp-test-with-temp-file-buffer (file-name content &rest body)
  "Evaluate BODY in a temporary buffer visiting FILE-NAME containing CONTENT."
  (declare (indent 2) (debug (form form body)))
  `(let* ((temp-dir (make-temp-file "vortel-lsp-test-" t))
          (temp-file (expand-file-name ,file-name temp-dir)))
     (unwind-protect
         (progn
           (with-temp-file temp-file
             (insert ,content))
           (with-current-buffer (find-file-noselect temp-file t)
             (unwind-protect
                 (progn ,@body)
               (when (buffer-live-p (current-buffer))
                 (set-buffer-modified-p nil)
                 (kill-buffer (current-buffer))))))
       (ignore-errors (delete-directory temp-dir t)))))

(provide 'vortel-lsp-test-helpers)

;;; vortel-lsp-test-helpers.el ends here
