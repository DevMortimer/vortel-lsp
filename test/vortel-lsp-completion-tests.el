;;; vortel-lsp-completion-tests.el --- Completion tests for vortel-lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors

;;; Code:

(require 'cl-lib)
(require 'ert)

(require 'vortel-lsp-test-helpers)

(ert-deftest vortel-lsp-test-completion-item-text-prefers-filter-text ()
  (let ((item (vortel-lsp-make-hash
               "label" " scanf(const char *restrict, ...)"
               "filterText" "scanf"
               "insertText" "scanf(${1:args})")))
    (should (equal (vortel-lsp--completion-item-text item) "scanf"))))

(ert-deftest vortel-lsp-test-completion-item-text-falls-back-to-trimmed-label ()
  (let ((item (vortel-lsp-make-hash
               "label" "  printf(const char *fmt, ...)  ")))
    (should (equal (vortel-lsp--completion-item-text item)
                   "printf(const char *fmt, ...)"))))

(ert-deftest vortel-lsp-test-completion-resolve-item-caches-result ()
  (let* ((client (vortel-lsp-test-make-client))
         (item (vortel-lsp-make-hash "label" "printf"))
         (resolved (vortel-lsp-make-hash "label" "printf" "detail" "int printf(...)"))
         (calls 0)
         (vortel-lsp--completion-resolve-cache (make-hash-table :test #'eq)))
    (cl-letf (((symbol-function 'vortel-lsp-client-completion-resolve-supported-p)
               (lambda (_client) t))
              ((symbol-function 'vortel-lsp--request-sync)
               (lambda (_client method params timeout)
                 (setq calls (1+ calls))
                 (should (string= method "completionItem/resolve"))
                 (should (eq params item))
                 (should (= timeout vortel-lsp-completion-timeout))
                 (list :ok t :result resolved))))
      (should (eq (vortel-lsp--completion-resolve-item item client) resolved))
      (should (eq (vortel-lsp--completion-resolve-item item client) resolved))
      (should (= calls 1)))))

(ert-deftest vortel-lsp-test-completion-item-documentation-renders-detail-plus-doc ()
  (let* ((markup (vortel-lsp-make-hash "kind" "markdown" "value" "Formatted output."))
         (item (vortel-lsp-make-hash "detail" "int printf(const char *format, ...)"
                                     "documentation" markup)))
    (should (equal (vortel-lsp--completion-item-documentation item)
                   "int printf(const char *format, ...)\n\nFormatted output."))))

(ert-deftest vortel-lsp-test-completion-docsig-uses-first-documentation-line ()
  (let* ((item (vortel-lsp-make-hash "documentation" "first line\nsecond line"))
         (candidate (propertize "printf"
                                'vortel-lsp-item item
                                'vortel-lsp-client nil)))
    (cl-letf (((symbol-function 'vortel-lsp--completion-resolve-item)
               (lambda (in-item _client) in-item)))
      (should (equal (vortel-lsp--completion-docsig candidate) "first line")))))

(ert-deftest vortel-lsp-test-completion-doc-buffer-renders-documentation ()
  (let* ((item (vortel-lsp-make-hash "documentation" "line one\nline two"))
         (candidate (propertize "printf"
                                'vortel-lsp-item item
                                'vortel-lsp-client nil))
         (buffer nil))
    (cl-letf (((symbol-function 'vortel-lsp--completion-resolve-item)
               (lambda (in-item _client) in-item)))
      (setq buffer (vortel-lsp--completion-doc-buffer candidate)))
    (unwind-protect
        (progn
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (should (equal (buffer-string) "line one\nline two"))
            (should buffer-read-only)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest vortel-lsp-test-completion-at-point-matches-filter-text-prefix ()
  (with-temp-buffer
    (insert "sca")
    (setq buffer-file-name "/tmp/main.c")
    (goto-char (point-max))
    (let* ((client
            (vortel-lsp-test-make-client
             :capabilities (vortel-lsp-make-hash "completionProvider"
                                                 (vortel-lsp-make-hash))))
           (item (vortel-lsp-make-hash
                  "label" " scanf(const char *restrict, ...)"
                  "filterText" "scanf"
                  "insertText" "scanf(${1:const char *restrict, ...})"))
           (vortel-lsp--completion-candidates nil)
           (vortel-lsp--completion-resolve-cache nil))
      (cl-letf (((symbol-function 'vortel-lsp--attachments-for-feature)
                 (lambda (feature)
                   (if (string= feature "completion")
                       (list (list :client client))
                     nil)))
                ((symbol-function 'vortel-lsp--request-sync)
                 (lambda (_client method _params _timeout)
                   (should (string= method "textDocument/completion"))
                   (list :ok t :result (list item)))))
        (let* ((capf (vortel-lsp--completion-at-point))
               (collection (nth 2 capf))
               (hits (all-completions "sca" collection)))
          (should (member "scanf" hits)))))))

(ert-deftest vortel-lsp-test-completion-exit-applies-resolved-additional-edits ()
  (with-temp-buffer
    (let* ((client (vortel-lsp-test-make-client))
           (item (vortel-lsp-make-hash "label" "printf"))
           (candidate (propertize "printf"
                                  'vortel-lsp-item item
                                  'vortel-lsp-client client))
           (additional (list (vortel-lsp-make-hash "newText" "#include <stdio.h>\n")))
           (called-buffer nil)
           (called-edits nil)
           (called-encoding nil))
      (cl-letf (((symbol-function 'vortel-lsp--completion-resolve-item)
                 (lambda (_item _client)
                   (vortel-lsp-make-hash "additionalTextEdits" additional)))
                ((symbol-function 'vortel-lsp--apply-text-edits-in-buffer)
                 (lambda (buffer edits encoding)
                   (setq called-buffer buffer)
                   (setq called-edits edits)
                   (setq called-encoding encoding)))
                ((symbol-function 'message)
                 (lambda (_fmt &rest _args) nil)))
        (vortel-lsp--completion-exit candidate 'finished)
        (should (eq called-buffer (current-buffer)))
        (should (equal called-edits additional))
        (should (eq called-encoding 'utf-16))))))

(provide 'vortel-lsp-completion-tests)

;;; vortel-lsp-completion-tests.el ends here
