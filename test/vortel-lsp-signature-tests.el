;;; vortel-lsp-signature-tests.el --- Signature help tests for vortel-lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors

;;; Code:

(require 'cl-lib)
(require 'ert)

(require 'vortel-lsp-test-helpers)

(ert-deftest vortel-lsp-test-signature-format-basic-label ()
  "Single signature, no params, returns prefixed label."
  (let ((result (vortel-lsp-make-hash
                 "signatures" (list (vortel-lsp-make-hash
                                     "label" "foo(x, y)")))))
    (should (equal (vortel-lsp--signature-help-format result)
                   "sig: foo(x, y)"))))

(ert-deftest vortel-lsp-test-signature-format-active-parameter-offsets ()
  "Parameter offsets [start, end] mark active param with brackets and upcase."
  (let* ((param-a (vortel-lsp-make-hash "label" [4 5]))
         (param-b (vortel-lsp-make-hash "label" [7 8]))
         (sig (vortel-lsp-make-hash
               "label" "foo(x, y)"
               "parameters" (list param-a param-b)))
         (result (vortel-lsp-make-hash
                  "signatures" (list sig)
                  "activeParameter" 1)))
    (should (equal (vortel-lsp--signature-help-format result)
                   "sig: foo(x, [Y])"))))

(ert-deftest vortel-lsp-test-signature-format-active-parameter-list-offsets ()
  "Parameter offsets as a list (start end) also work."
  (let* ((param-a (vortel-lsp-make-hash "label" '(4 5)))
         (param-b (vortel-lsp-make-hash "label" '(7 8)))
         (sig (vortel-lsp-make-hash
               "label" "foo(x, y)"
               "parameters" (list param-a param-b)))
         (result (vortel-lsp-make-hash
                  "signatures" (list sig)
                  "activeParameter" 0)))
    (should (equal (vortel-lsp--signature-help-format result)
                   "sig: foo([X], y)"))))

(ert-deftest vortel-lsp-test-signature-format-string-parameter-label ()
  "String parameter label highlights by search in signature label."
  (let* ((param-a (vortel-lsp-make-hash "label" "x"))
         (param-b (vortel-lsp-make-hash "label" "y"))
         (sig (vortel-lsp-make-hash
               "label" "bar(x, y)"
               "parameters" (list param-a param-b)))
         (result (vortel-lsp-make-hash
                  "signatures" (list sig)
                  "activeParameter" 0)))
    (should (equal (vortel-lsp--signature-help-format result)
                   "sig: bar([X], y)"))))

(ert-deftest vortel-lsp-test-signature-format-with-docs ()
  "Signature + parameter docs appear after label."
  (let* ((param (vortel-lsp-make-hash
                 "label" "x"
                 "documentation" "The x parameter."))
         (sig (vortel-lsp-make-hash
               "label" "fn(x)"
               "parameters" (list param)
               "documentation" "A function."))
         (result (vortel-lsp-make-hash
                  "signatures" (list sig)
                  "activeParameter" 0)))
    (should (equal (vortel-lsp--signature-help-format result)
                   "sig: fn([X])\nA function.\nThe x parameter."))))

(ert-deftest vortel-lsp-test-signature-format-doc-truncation ()
  "Docs beyond max-doc-lines are cut."
  (let* ((long-doc "line1\nline2\nline3\nline4\nline5\nline6\nline7")
         (sig (vortel-lsp-make-hash
               "label" "fn()"
               "documentation" long-doc))
         (result (vortel-lsp-make-hash
                  "signatures" (list sig)))
         (vortel-lsp-signature-help-max-doc-lines 3))
    (should (equal (vortel-lsp--signature-help-format result)
                   "sig: fn()\nline1\nline2\nline3"))))

(ert-deftest vortel-lsp-test-signature-format-nil-result ()
  "Returns nil for nil result."
  (should (null (vortel-lsp--signature-help-format nil))))

(ert-deftest vortel-lsp-test-signature-format-empty-signatures ()
  "Returns nil for empty signatures array."
  (let ((result (vortel-lsp-make-hash "signatures" '())))
    (should (null (vortel-lsp--signature-help-format result)))))

(ert-deftest vortel-lsp-test-signature-format-active-signature-out-of-bounds ()
  "Falls back to first signature when activeSignature is out of bounds."
  (let* ((sig (vortel-lsp-make-hash "label" "first(a)"))
         (result (vortel-lsp-make-hash
                  "signatures" (list sig)
                  "activeSignature" 99)))
    (should (equal (vortel-lsp--signature-help-format result)
                   "sig: first(a)"))))

(ert-deftest vortel-lsp-test-signature-format-markup-content-docs ()
  "MarkupContent documentation is extracted correctly."
  (let* ((sig (vortel-lsp-make-hash
               "label" "fn()"
               "documentation" (vortel-lsp-make-hash
                                "kind" "markdown"
                                "value" "Some docs.")))
         (result (vortel-lsp-make-hash
                  "signatures" (list sig))))
    (should (equal (vortel-lsp--signature-help-format result)
                   "sig: fn()\nSome docs."))))

(ert-deftest vortel-lsp-test-signature-trigger-paren ()
  "Typing `(' sets up a debounce timer."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo(")
    (let* ((client (vortel-lsp-test-make-client
                    :capabilities (vortel-lsp-make-hash
                                   "signatureHelpProvider"
                                   (vortel-lsp-make-hash))))
           (vortel-lsp-mode t)
           (vortel-lsp-signature-help-mode 'auto)
           (vortel-lsp--attachments (list (list :client client
                                               :server-entry (vortel-lsp-make-hash))))
           (vortel-lsp--signature-timer nil)
           (vortel-lsp--signature-active nil)
           (this-command 'self-insert-command)
           (timer-started nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat _fn &rest _args)
                   (setq timer-started t)
                   'fake-timer)))
        (vortel-lsp--signature-post-command)
        (should timer-started)))))

(ert-deftest vortel-lsp-test-signature-trigger-comma ()
  "Typing `,' sets up a debounce timer."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo a,")
    (let* ((client (vortel-lsp-test-make-client
                    :capabilities (vortel-lsp-make-hash
                                   "signatureHelpProvider"
                                   (vortel-lsp-make-hash))))
           (vortel-lsp-mode t)
           (vortel-lsp-signature-help-mode 'auto)
           (vortel-lsp--attachments (list (list :client client
                                               :server-entry (vortel-lsp-make-hash))))
           (vortel-lsp--signature-timer nil)
           (vortel-lsp--signature-active nil)
           (this-command 'self-insert-command)
           (timer-started nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat _fn &rest _args)
                   (setq timer-started t)
                   'fake-timer)))
        (vortel-lsp--signature-post-command)
        (should timer-started)))))

(ert-deftest vortel-lsp-test-signature-clear-at-depth-zero ()
  "Leaving paren context clears active signature."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo(x) ")
    (goto-char (point-max))
    (let* ((client (vortel-lsp-test-make-client
                    :capabilities (vortel-lsp-make-hash
                                   "signatureHelpProvider"
                                   (vortel-lsp-make-hash))))
           (vortel-lsp-mode t)
           (vortel-lsp-signature-help-mode 'auto)
           (vortel-lsp--attachments (list (list :client client
                                               :server-entry (vortel-lsp-make-hash))))
           (vortel-lsp--signature-timer nil)
           (vortel-lsp--signature-active "sig: foo(x)")
           (this-command 'forward-char))
      (vortel-lsp--signature-post-command)
      (should (null vortel-lsp--signature-active)))))

(ert-deftest vortel-lsp-test-signature-active-parameter-from-signature ()
  "activeParameter on the signature itself is used when not on result."
  (let* ((param-a (vortel-lsp-make-hash "label" [4 5]))
         (param-b (vortel-lsp-make-hash "label" [7 8]))
         (sig (vortel-lsp-make-hash
               "label" "foo(x, y)"
               "parameters" (list param-a param-b)
               "activeParameter" 1))
         (result (vortel-lsp-make-hash
                  "signatures" (list sig))))
    (should (equal (vortel-lsp--signature-help-format result)
                   "sig: foo(x, [Y])"))))

(provide 'vortel-lsp-signature-tests)

;;; vortel-lsp-signature-tests.el ends here
