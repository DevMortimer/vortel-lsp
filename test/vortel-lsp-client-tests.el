;;; vortel-lsp-client-tests.el --- Client tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors

;;; Code:

(require 'ert)

(require 'vortel-lsp-client)
(require 'vortel-lsp-util)

(ert-deftest vortel-lsp-test-client-initialize-sends-initialized-before-ready ()
  "The initialized notification is sent before entering ready state."
  (let* ((client (vortel-lsp-client--create
                  :id 1
                  :name "ty"
                  :command "ty"
                  :args '("server")
                  :root-path default-directory
                  :root-uri (vortel-lsp-path-to-uri default-directory)
                  :initialization-options nil
                  :timeout 1
                  :environment nil
                  :transport nil
                  :state 'starting
                  :next-request-id 0
                  :pending (make-hash-table :test #'equal)
                  :send-queue nil
                  :capabilities (make-hash-table :test #'equal)
                  :dynamic-capabilities (make-hash-table :test #'equal)
                  :server-info nil
                  :notification-handlers nil
                  :request-handlers nil
                  :state-handlers nil))
         (on-success nil)
         (events nil))
    (vortel-lsp-client-add-state-handler
     client
     (lambda (_client new-state _old-state)
       (when (eq new-state 'ready)
         (push 'ready events))))
    (cl-letf (((symbol-function 'vortel-lsp-client-request)
               (lambda (_client _method _params &rest keys)
                 (setq on-success (plist-get keys :on-success))
                 1))
              ((symbol-function 'vortel-lsp-client-notify)
               (lambda (_client method _params)
                 (when (string= method "initialized")
                   (push 'initialized events)))))
      (vortel-lsp-client--begin-initialize client)
      (funcall on-success (vortel-lsp-make-hash "capabilities" (vortel-lsp-make-hash))))
    (should (equal (nreverse events) '(initialized ready)))))

(ert-deftest vortel-lsp-test-client-initialize-capabilities-advertises-snippets ()
  "Completion capabilities should claim snippet support."
  (let* ((caps (vortel-lsp-client--initialize-capabilities))
         (text-document (vortel-lsp-hash-get caps "textDocument"))
         (completion (vortel-lsp-hash-get text-document "completion"))
         (completion-item (vortel-lsp-hash-get completion "completionItem")))
    (should (vortel-lsp-hash-get completion-item "snippetSupport"))))

(provide 'vortel-lsp-client-tests)

;;; vortel-lsp-client-tests.el ends here
