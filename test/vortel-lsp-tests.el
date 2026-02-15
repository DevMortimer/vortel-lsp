;;; vortel-lsp-tests.el --- Test entrypoint for vortel-lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors

;;; Code:

(require 'ert)

(require 'vortel-lsp-test-helpers)
(require 'vortel-lsp-util)
(require 'vortel-lsp-apply-edit-tests)
(require 'vortel-lsp-completion-tests)
(require 'vortel-lsp-actions-rename-tests)
(require 'vortel-lsp-transport-tests)
(require 'vortel-lsp-config-tests)
(require 'vortel-lsp-registry-tests)

;;; --- hash-get-section ---

(ert-deftest vortel-lsp-test-hash-get-section-nil-section ()
  "Nil section returns the whole table."
  (let ((ht (vortel-lsp-make-hash "a" 1)))
    (should (eq (vortel-lsp-hash-get-section ht nil) ht))))

(ert-deftest vortel-lsp-test-hash-get-section-empty-section ()
  "Empty string section returns the whole table."
  (let ((ht (vortel-lsp-make-hash "a" 1)))
    (should (eq (vortel-lsp-hash-get-section ht "") ht))))

(ert-deftest vortel-lsp-test-hash-get-section-single-key ()
  "Single key without dots returns direct value."
  (let ((ht (vortel-lsp-make-hash "foo" 42)))
    (should (= (vortel-lsp-hash-get-section ht "foo") 42))))

(ert-deftest vortel-lsp-test-hash-get-section-dotted-path ()
  "Dotted path traverses nested hash-tables."
  (let ((ht (vortel-lsp-make-hash
             "typescript" (vortel-lsp-make-hash
                           "inlayHints" (vortel-lsp-make-hash
                                         "enabled" t)))))
    (should (eq (vortel-lsp-hash-get-section ht "typescript.inlayHints.enabled") t))
    (should (hash-table-p (vortel-lsp-hash-get-section ht "typescript.inlayHints")))))

(ert-deftest vortel-lsp-test-hash-get-section-missing-key ()
  "Missing key returns nil."
  (let ((ht (vortel-lsp-make-hash "a" 1)))
    (should-not (vortel-lsp-hash-get-section ht "b"))
    (should-not (vortel-lsp-hash-get-section ht "a.b.c"))))

(ert-deftest vortel-lsp-test-hash-get-section-nil-table ()
  "Nil table returns nil for any section."
  (should-not (vortel-lsp-hash-get-section nil "foo"))
  (should-not (vortel-lsp-hash-get-section nil nil)))

(provide 'vortel-lsp-tests)

;;; vortel-lsp-tests.el ends here
