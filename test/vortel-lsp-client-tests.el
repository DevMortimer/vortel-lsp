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

(ert-deftest vortel-lsp-test-client-initialize-capabilities-advertises-file-watch-registration ()
  "Workspace file-watch dynamic registration should be advertised."
  (let* ((caps (vortel-lsp-client--initialize-capabilities))
         (workspace (vortel-lsp-hash-get caps "workspace"))
         (file-watch (vortel-lsp-hash-get workspace "didChangeWatchedFiles")))
    (should (vortel-lsp-hash-get file-watch "dynamicRegistration"))))

(ert-deftest vortel-lsp-test-client-register-capabilities-watched-files-notifies-on-events ()
  "Watched-files dynamic registrations should emit didChange notifications."
  (let* ((root default-directory)
         (client (vortel-lsp-client--create
                  :id 1
                  :name "ty"
                  :command "ty"
                  :args '()
                  :root-path root
                  :root-uri (vortel-lsp-path-to-uri root)
                  :initialization-options nil
                  :timeout 1
                  :environment nil
                  :transport nil
                  :state 'ready
                  :next-request-id 0
                  :pending (make-hash-table :test #'equal)
                  :send-queue nil
                  :capabilities (make-hash-table :test #'equal)
                  :dynamic-capabilities (make-hash-table :test #'equal)
                  :watched-file-registrations (make-hash-table :test #'equal)
                  :server-info nil
                  :notification-handlers nil
                  :request-handlers nil
                  :state-handlers nil))
         (watch-callback nil)
         (notifications nil)
         (changed-path (expand-file-name "foo.py" root))
         (registration
          (vortel-lsp-make-hash
           "id" "watch-1"
           "method" "workspace/didChangeWatchedFiles"
           "registerOptions"
           (vortel-lsp-make-hash
            "watchers"
            (list (vortel-lsp-make-hash
                   "globPattern" "**/*.py"
                   "kind" 7))))))
    (cl-letf (((symbol-function 'file-notify-add-watch)
               (lambda (_path _flags callback)
                 (setq watch-callback callback)
                 :watch-1))
              ((symbol-function 'vortel-lsp-client-notify)
               (lambda (_client method params)
                 (push (list method params) notifications))))
      (vortel-lsp-client-register-capabilities client (list registration))
      (should watch-callback)
      (funcall watch-callback (list :watch-1 'changed changed-path)))
    (let* ((notif (car notifications))
           (method (car notif))
           (params (cadr notif))
           (changes (vortel-lsp-hash-get params "changes"))
           (first-change (car changes)))
      (should (string= method "workspace/didChangeWatchedFiles"))
      (should (= (length changes) 1))
      (should (= (vortel-lsp-hash-get first-change "type") 2))
      (should (string= (vortel-lsp-hash-get first-change "uri")
                       (vortel-lsp-path-to-uri changed-path))))))

(ert-deftest vortel-lsp-test-client-unregister-capabilities-watched-files-removes-watch ()
  "Unregistering watched-files capabilities should remove watch descriptors."
  (let* ((root default-directory)
         (client (vortel-lsp-client--create
                  :id 1
                  :name "ty"
                  :command "ty"
                  :args '()
                  :root-path root
                  :root-uri (vortel-lsp-path-to-uri root)
                  :initialization-options nil
                  :timeout 1
                  :environment nil
                  :transport nil
                  :state 'ready
                  :next-request-id 0
                  :pending (make-hash-table :test #'equal)
                  :send-queue nil
                  :capabilities (make-hash-table :test #'equal)
                  :dynamic-capabilities (make-hash-table :test #'equal)
                  :watched-file-registrations (make-hash-table :test #'equal)
                  :server-info nil
                  :notification-handlers nil
                  :request-handlers nil
                  :state-handlers nil))
         (removed nil)
         (registration
          (vortel-lsp-make-hash
           "id" "watch-1"
           "method" "workspace/didChangeWatchedFiles"
           "registerOptions"
           (vortel-lsp-make-hash
            "watchers"
            (list (vortel-lsp-make-hash
                   "globPattern" "**/*.py"
                   "kind" 7)))))
         (unregistration
          (vortel-lsp-make-hash
           "id" "watch-1"
           "method" "workspace/didChangeWatchedFiles")))
    (cl-letf (((symbol-function 'file-notify-add-watch)
               (lambda (_path _flags _callback)
                 :watch-1))
              ((symbol-function 'file-notify-rm-watch)
               (lambda (descriptor)
                 (setq removed descriptor))))
      (vortel-lsp-client-register-capabilities client (list registration))
      (vortel-lsp-client-unregister-capabilities client (list unregistration)))
    (should (eq removed :watch-1))))

(ert-deftest vortel-lsp-test-client-dispatch-request-stops-after-first-reply ()
  "Dispatch should stop once a request handler replies."
  (let* ((client (vortel-lsp-client--create
                  :id 1
                  :name "ty"
                  :command "ty"
                  :args '()
                  :root-path default-directory
                  :root-uri (vortel-lsp-path-to-uri default-directory)
                  :initialization-options nil
                  :timeout 1
                  :environment nil
                  :transport nil
                  :state 'ready
                  :next-request-id 0
                  :pending (make-hash-table :test #'equal)
                  :send-queue nil
                  :capabilities (make-hash-table :test #'equal)
                  :dynamic-capabilities (make-hash-table :test #'equal)
                  :server-info nil
                  :notification-handlers nil
                  :request-handlers nil
                  :state-handlers nil))
         (first-called 0)
         (second-called 0)
         (replies 0)
         (reply-errors 0)
         (reply-id nil)
         (reply-result nil))
    (setf (vortel-lsp-client-request-handlers client)
          (list
           (lambda (_client _id _method _params reply)
             (setq first-called (1+ first-called))
             (funcall reply (vortel-lsp-make-hash "ok" t)))
           (lambda (_client _id _method _params _reply)
             (setq second-called (1+ second-called))
             (error "should not run"))))
    (cl-letf (((symbol-function 'vortel-lsp-client-reply)
               (lambda (_client id result)
                 (setq replies (1+ replies))
                 (setq reply-id id)
                 (setq reply-result result)))
              ((symbol-function 'vortel-lsp-client-reply-error)
               (lambda (_client _id _error)
                 (setq reply-errors (1+ reply-errors)))))
      (vortel-lsp-client--dispatch-request
       client
       99
       "workspace/configuration"
       (vortel-lsp-make-hash)))
    (should (= first-called 1))
    (should (= second-called 0))
    (should (= replies 1))
    (should (= reply-errors 0))
    (should (= reply-id 99))
    (should (vortel-lsp-hash-get reply-result "ok"))))

(ert-deftest vortel-lsp-test-client-dispatch-request-uses-next-handler-when-unhandled ()
  "Dispatch should continue when a request handler neither handles nor replies."
  (let* ((client (vortel-lsp-client--create
                  :id 1
                  :name "ty"
                  :command "ty"
                  :args '()
                  :root-path default-directory
                  :root-uri (vortel-lsp-path-to-uri default-directory)
                  :initialization-options nil
                  :timeout 1
                  :environment nil
                  :transport nil
                  :state 'ready
                  :next-request-id 0
                  :pending (make-hash-table :test #'equal)
                  :send-queue nil
                  :capabilities (make-hash-table :test #'equal)
                  :dynamic-capabilities (make-hash-table :test #'equal)
                  :server-info nil
                  :notification-handlers nil
                  :request-handlers nil
                  :state-handlers nil))
         (first-called 0)
         (second-called 0)
         (replies 0)
         (reply-errors 0)
         (reply-id nil)
         (reply-result nil))
    (setf (vortel-lsp-client-request-handlers client)
          (list
           (lambda (_client _id _method _params _reply)
             (setq first-called (1+ first-called))
             nil)
           (lambda (_client _id _method _params reply)
             (setq second-called (1+ second-called))
             (funcall reply (vortel-lsp-make-hash "ok" t)))))
    (cl-letf (((symbol-function 'vortel-lsp-client-reply)
               (lambda (_client id result)
                 (setq replies (1+ replies))
                 (setq reply-id id)
                 (setq reply-result result)))
              ((symbol-function 'vortel-lsp-client-reply-error)
               (lambda (_client _id _error)
                 (setq reply-errors (1+ reply-errors)))))
      (vortel-lsp-client--dispatch-request
       client
       77
       "workspace/configuration"
       (vortel-lsp-make-hash)))
    (should (= first-called 1))
    (should (= second-called 1))
    (should (= replies 1))
    (should (= reply-errors 0))
    (should (= reply-id 77))
    (should (vortel-lsp-hash-get reply-result "ok"))))

(ert-deftest vortel-lsp-test-client-did-save-skips-when-save-disabled ()
  "didSave should not be sent when textDocumentSync.save is disabled."
  (let* ((client (vortel-lsp-client--create
                  :id 1
                  :name "ty"
                  :command "ty"
                  :args '()
                  :root-path default-directory
                  :root-uri (vortel-lsp-path-to-uri default-directory)
                  :initialization-options nil
                  :timeout 1
                  :environment nil
                  :transport nil
                  :state 'ready
                  :next-request-id 0
                  :pending (make-hash-table :test #'equal)
                  :send-queue nil
                  :capabilities (vortel-lsp-make-hash
                                 "textDocumentSync"
                                 (vortel-lsp-make-hash
                                  "change" 2
                                  "save" vortel-lsp--json-false))
                  :dynamic-capabilities (make-hash-table :test #'equal)
                  :server-info nil
                  :notification-handlers nil
                  :request-handlers nil
                  :state-handlers nil))
         (notifications nil))
    (cl-letf (((symbol-function 'vortel-lsp-client-notify)
               (lambda (_client method params)
                 (push (list method params) notifications))))
      (vortel-lsp-client-did-save client "file:///tmp/test.py" "print(1)"))
    (should (null notifications))))

(ert-deftest vortel-lsp-test-client-did-save-sends-with-optional-text ()
  "didSave should send and include text only when includeText is set."
  (let* ((client (vortel-lsp-client--create
                  :id 1
                  :name "ty"
                  :command "ty"
                  :args '()
                  :root-path default-directory
                  :root-uri (vortel-lsp-path-to-uri default-directory)
                  :initialization-options nil
                  :timeout 1
                  :environment nil
                  :transport nil
                  :state 'ready
                  :next-request-id 0
                  :pending (make-hash-table :test #'equal)
                  :send-queue nil
                  :capabilities (vortel-lsp-make-hash
                                 "textDocumentSync"
                                 (vortel-lsp-make-hash
                                  "change" 2
                                  "save" (vortel-lsp-make-hash "includeText" t)))
                  :dynamic-capabilities (make-hash-table :test #'equal)
                  :server-info nil
                  :notification-handlers nil
                  :request-handlers nil
                  :state-handlers nil))
         (notifications nil)
         (uri "file:///tmp/test.py")
         (text "print(1)"))
    (cl-letf (((symbol-function 'vortel-lsp-client-notify)
               (lambda (_client method params)
                 (push (list method params) notifications))))
      (vortel-lsp-client-did-save client uri text))
    (let* ((payload (car notifications))
           (method (car payload))
           (params (cadr payload))
           (text-document (vortel-lsp-hash-get params "textDocument")))
      (should (= (length notifications) 1))
      (should (equal method "textDocument/didSave"))
      (should (equal (vortel-lsp-hash-get text-document "uri") uri))
      (should (equal (vortel-lsp-hash-get params "text") text)))))

(provide 'vortel-lsp-client-tests)

;;; vortel-lsp-client-tests.el ends here
