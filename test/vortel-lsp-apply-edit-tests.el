;;; vortel-lsp-apply-edit-tests.el --- Workspace applyEdit tests for vortel-lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors

;;; Code:

(require 'ert)

(require 'vortel-lsp-test-helpers)

(ert-deftest vortel-lsp-test-collect-workspace-edit-operations-document-changes-order ()
  (vortel-lsp-test-with-temp-dir dir
    (let* ((first (expand-file-name "first.txt" dir))
           (second (expand-file-name "second.txt" dir))
           (third (expand-file-name "third.txt" dir))
           (workspace-edit
            (vortel-lsp-make-hash
             "documentChanges"
             (list
              (vortel-lsp-make-hash "kind" "create"
                                    "uri" (vortel-lsp-path-to-uri first))
              (vortel-lsp-make-hash "kind" "rename"
                                    "oldUri" (vortel-lsp-path-to-uri first)
                                    "newUri" (vortel-lsp-path-to-uri second))
              (vortel-lsp-make-hash "kind" "delete"
                                    "uri" (vortel-lsp-path-to-uri third)))))
           (collected (vortel-lsp--collect-workspace-edit-operations workspace-edit))
           (operations (plist-get collected :operations)))
      (should (plist-get collected :ok))
      (should (equal (mapcar (lambda (op) (plist-get op :kind)) operations)
                     '(create-file rename-file delete-file))))))

(ert-deftest vortel-lsp-test-apply-workspace-edit-mixed-resource-and-text-edits ()
  (vortel-lsp-test-with-temp-dir dir
    (let* ((client (vortel-lsp-test-make-client :root-path dir))
           (source (expand-file-name "source.c" dir))
           (created (expand-file-name "created.tmp" dir))
           (renamed (expand-file-name "renamed.tmp" dir))
           (changes (make-hash-table :test #'equal))
           (workspace-edit nil)
           (result nil))
      (with-temp-file source
        (insert "hello\n"))
      (puthash
       (vortel-lsp-path-to-uri source)
       (list (vortel-lsp-make-hash
              "range" (vortel-lsp-make-hash
                       "start" (vortel-lsp-make-hash "line" 0 "character" 0)
                       "end" (vortel-lsp-make-hash "line" 0 "character" 5))
              "newText" "bye"))
       changes)
      (setq workspace-edit
            (vortel-lsp-make-hash
             "changes" changes
             "documentChanges"
             (list
              (vortel-lsp-make-hash "kind" "create"
                                    "uri" (vortel-lsp-path-to-uri created))
              (vortel-lsp-make-hash "kind" "rename"
                                    "oldUri" (vortel-lsp-path-to-uri created)
                                    "newUri" (vortel-lsp-path-to-uri renamed))
              (vortel-lsp-make-hash "kind" "delete"
                                    "uri" (vortel-lsp-path-to-uri renamed)))))
      (setq result (vortel-lsp--apply-workspace-edit client workspace-edit))
      (should (vortel-lsp-truthy-p (vortel-lsp-hash-get result "applied")))
      (should (equal (with-temp-buffer
                       (insert-file-contents source)
                       (buffer-string))
                     "bye\n"))
      (should-not (file-exists-p created))
      (should-not (file-exists-p renamed)))))

(ert-deftest vortel-lsp-test-apply-workspace-edit-rejects-unsafe-path ()
  (let* ((client (vortel-lsp-test-make-client))
         (workspace-edit
          (vortel-lsp-make-hash
           "documentChanges"
           (list
            (vortel-lsp-make-hash "kind" "create" "uri" "file:///"))))
         (result (vortel-lsp--apply-workspace-edit client workspace-edit))
         (reason (vortel-lsp-hash-get result "failureReason")))
    (should-not (vortel-lsp-truthy-p (vortel-lsp-hash-get result "applied")))
    (should (string-match-p "unsafe path" reason))))

(ert-deftest vortel-lsp-test-workspace-edit-create-file-requires-parent-directory ()
  (vortel-lsp-test-with-temp-dir dir
    (let* ((path (expand-file-name "missing/child.txt" dir))
           (result (vortel-lsp--workspace-edit-create-file path (vortel-lsp-make-hash))))
      (should-not (plist-get result :ok))
      (should (string-match-p "parent directory does not exist"
                              (plist-get result :reason))))))

(ert-deftest vortel-lsp-test-workspace-edit-delete-directory-requires-recursive ()
  (vortel-lsp-test-with-temp-dir dir
    (let ((target (expand-file-name "subdir" dir)))
      (make-directory target t)
      (let ((non-recursive (vortel-lsp--workspace-edit-delete-file
                            target
                            (vortel-lsp-make-hash))))
        (should-not (plist-get non-recursive :ok))
        (should (file-directory-p target)))
      (let ((recursive (vortel-lsp--workspace-edit-delete-file
                        target
                        (vortel-lsp-make-hash "recursive" t))))
        (should (plist-get recursive :ok))
        (should-not (file-exists-p target))))))

(provide 'vortel-lsp-apply-edit-tests)

;;; vortel-lsp-apply-edit-tests.el ends here
