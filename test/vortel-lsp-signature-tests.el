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

(ert-deftest vortel-lsp-test-signature-infers-active-parameter-for-named-args ()
  "Named arguments infer active parameter when server omits activeParameter."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "fn(gamma=3")
    (goto-char (point-max))
    (let* ((params (list (vortel-lsp-make-hash "label" "alpha")
                         (vortel-lsp-make-hash "label" "beta")
                         (vortel-lsp-make-hash "label" "gamma")))
           (sig (vortel-lsp-make-hash
                 "label" "fn(alpha, beta, gamma)"
                 "parameters" params))
           (result (vortel-lsp-make-hash "signatures" (list sig))))
      (should (equal (vortel-lsp--signature-help-format result)
                     "sig: fn(alpha, beta, [GAMMA])")))))

(ert-deftest vortel-lsp-test-signature-popup-uses-face-for-active-parameter ()
  "Signature popup highlights the active parameter with face properties."
  (let* ((parsed '(:label "fn(alpha, beta)" :param-start 3 :param-end 8))
         (popup (vortel-lsp--signature-help-popup-text parsed))
         (face (get-text-property 3 'face popup)))
    (should (equal popup "fn(alpha, beta)"))
    (if (listp face)
        (should (memq 'vortel-lsp-signature-active-parameter-face face))
      (should (eq face 'vortel-lsp-signature-active-parameter-face)))))

(ert-deftest vortel-lsp-test-inside-round-parens-detects-outer-context ()
  "Round-paren context stays active even inside nested [] or {}."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "foo(bar[0])")
    (search-backward "0")
    (should (vortel-lsp--inside-round-parens-p))))

(ert-deftest vortel-lsp-test-doc-placement-prefers-above-when-below-does-not-fit ()
  "Popup placement falls back to above when lower space is insufficient."
  (let ((placement (vortel-lsp--doc-placement
                    20 170
                    8 16
                    120 60
                    '(0 0 300 200))))
    (should (eq (plist-get placement :side) 'above))))

(ert-deftest vortel-lsp-test-signature-trigger-paren ()
  "Typing `(' requests signature help immediately."
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
            (requested nil))
      (cl-letf (((symbol-function 'vortel-lsp--signature-help-request)
                 (lambda ()
                   (setq requested t))))
        (vortel-lsp--signature-post-command)
        (should requested)))))

(ert-deftest vortel-lsp-test-signature-trigger-comma ()
  "Typing `,' requests signature help immediately."
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
            (requested nil))
      (cl-letf (((symbol-function 'vortel-lsp--signature-help-request)
                 (lambda ()
                   (setq requested t))))
        (vortel-lsp--signature-post-command)
        (should requested)))))

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
           (vortel-lsp--signature-active '(:label "foo(x)"))
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

(ert-deftest vortel-lsp-test-signature-inline-hints-disabled ()
  "Inline signature hints are disabled in favor of floating docs only."
  (with-temp-buffer
    (insert "foo(")
    (goto-char (point-max))
    (should (null (vortel-lsp--signature-clear-hint)))))

(ert-deftest vortel-lsp-test-hover-uses-floating-doc-display ()
  "Hover docs are routed to floating doc display instead of echo message."
  (with-temp-buffer
    (let ((doc-shown nil)
          (message-called nil))
      (cl-letf (((symbol-function 'vortel-lsp--attachments-for-feature)
                 (lambda (feature)
                   (when (equal feature "hover")
                     (list (list :client 'fake-client)))))
                ((symbol-function 'vortel-lsp-client-request)
                 (lambda (_client _method _params &rest args)
                   (let ((on-success (plist-get args :on-success)))
                     (funcall on-success (vortel-lsp-make-hash "contents" "hover docs")))))
                ((symbol-function 'vortel-lsp--text-document-position-params)
                 (lambda (_client)
                   (vortel-lsp-make-hash)))
                ((symbol-function 'vortel-lsp--doc-show)
                 (lambda (doc-text &optional _source)
                   (setq doc-shown doc-text)))
                ((symbol-function 'message)
                 (lambda (&rest _args)
                   (setq message-called t))))
        (vortel-lsp-hover)
        (should (equal doc-shown "hover docs"))
        (should-not message-called)))))

(ert-deftest vortel-lsp-test-diagnostic-at-point-message-prefix ()
  "Diagnostic helper returns prefixed Flymake message."
  (with-temp-buffer
    (let ((vortel-lsp-enable-flymake t)
          (flymake-mode t)
          (vortel-lsp-flymake-echo-prefix "diag: "))
      (cl-letf (((symbol-function 'flymake-diagnostics)
                 (lambda (&rest _args)
                   (list (flymake-make-diagnostic (current-buffer) 1 2 :warning "warn msg")))))
        (should (equal (vortel-lsp--diagnostic-at-point-message)
                       "diag: warn msg"))))))

(ert-deftest vortel-lsp-test-ui-post-command-requests-signature-inside-parens ()
  "Moving inside argument lists requests signature help."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "input()")
    (goto-char (1- (point-max)))
    (let ((vortel-lsp-mode t)
          (vortel-lsp-signature-help-mode 'auto)
          (requested nil)
          (this-command 'forward-char))
      (cl-letf (((symbol-function 'vortel-lsp--attachments-for-feature)
                 (lambda (feature)
                   (when (equal feature "signature-help")
                     (list (list :client 'fake-client :server-entry (vortel-lsp-make-hash))))))
                ((symbol-function 'vortel-lsp--signature-help-request)
                 (lambda ()
                   (setq requested t))))
        (vortel-lsp--ui-post-command)
        (should requested)))))

(ert-deftest vortel-lsp-test-ui-post-command-mouse-forces-hover-refresh ()
  "Mouse commands force hover refresh even at same point." 
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "value")
    (goto-char (point-min))
    (let ((vortel-lsp-mode t)
          (vortel-lsp-auto-hover t)
          (vortel-lsp--hover-last-point (point))
          (this-command 'mouse-set-point)
          (requested nil))
      (cl-letf (((symbol-function 'vortel-lsp--auto-hover-request-at-point)
                 (lambda ()
                   (setq requested t)))
                ((symbol-function 'thing-at-point)
                 (lambda (&rest _args)
                   "value")))
        (vortel-lsp--ui-post-command)
        (should requested)))))

(ert-deftest vortel-lsp-test-ui-post-command-schedules-hover-while-point-stays ()
  "When point stays on a symbol, hover is scheduled on idle."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "value")
    (goto-char (point-min))
    (let ((vortel-lsp-mode t)
          (vortel-lsp-auto-hover t)
          (vortel-lsp--hover-last-point (point))
          (this-command 'forward-char)
          (scheduled nil))
      (cl-letf (((symbol-function 'thing-at-point)
                 (lambda (&rest _args)
                   "value"))
                ((symbol-function 'vortel-lsp--auto-hover-schedule)
                 (lambda (&optional _force)
                   (setq scheduled t))))
        (vortel-lsp--ui-post-command)
        (should scheduled)))))

(provide 'vortel-lsp-signature-tests)

;;; vortel-lsp-signature-tests.el ends here
