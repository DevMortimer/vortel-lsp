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

(ert-deftest vortel-lsp-test-completion-item-text-strips-menu-style-label-suffix ()
  (let ((item (vortel-lsp-make-hash
               "label" "signa (import signal)")))
    (should (equal (vortel-lsp--completion-item-text item)
                   "signa"))))

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

(ert-deftest vortel-lsp-test-completion-sort-candidates-prefers-prefix-matches ()
  (let ((vortel-lsp-completion-fuzzy-sort t)
        (candidates '("BaseException" "print" "property" "__repr__")))
    (should (equal (vortel-lsp--completion-sort-candidates "pr" candidates)
                   '("print" "property" "__repr__" "BaseException")))))

(ert-deftest vortel-lsp-test-completion-at-point-sorts-noisy-results-by-query ()
  (with-temp-buffer
    (insert "pr")
    (setq buffer-file-name "/tmp/main.py")
    (goto-char (point-max))
    (let* ((client
            (vortel-lsp-test-make-client
             :capabilities (vortel-lsp-make-hash "completionProvider"
                                                 (vortel-lsp-make-hash))))
           (items
            (list (vortel-lsp-make-hash "label" "BaseException")
                  (vortel-lsp-make-hash "label" "print")
                  (vortel-lsp-make-hash "label" "property")))
           (vortel-lsp-completion-fuzzy-sort t)
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
                   (list :ok t :result items))))
         (vortel-lsp--completion-at-point)
         (should (equal (car vortel-lsp--completion-candidates) "print"))))))

(ert-deftest vortel-lsp-test-completion-at-point-uses-shared-text-edit-range ()
  (with-temp-buffer
    (insert "obj.scx")
    (setq buffer-file-name "/tmp/main.ts")
    (goto-char (point-max))
    (let* ((client
            (vortel-lsp-test-make-client
             :capabilities (vortel-lsp-make-hash "completionProvider"
                                                 (vortel-lsp-make-hash))))
           (item
            (vortel-lsp-make-hash
             "label" "scanf"
             "filterText" "scanf"
             "textEdit" (vortel-lsp-make-hash
                         "range" (vortel-lsp-make-hash
                                  "start" (vortel-lsp-make-hash "line" 0 "character" 3)
                                  "end" (vortel-lsp-make-hash "line" 0 "character" 7))
                         "newText" "scanf"))))
      (cl-letf (((symbol-function 'vortel-lsp--attachments-for-feature)
                 (lambda (feature)
                   (if (string= feature "completion")
                       (list (list :client client))
                     nil)))
                ((symbol-function 'vortel-lsp--request-sync)
                 (lambda (&rest _args)
                   (list :ok t :result (list item)))))
        (let ((capf (vortel-lsp--completion-at-point)))
          (should (equal (cons (nth 0 capf) (nth 1 capf))
                         (cons 4 8))))))))

(ert-deftest vortel-lsp-test-completion-at-point-falls-back-on-conflicting-ranges ()
  (with-temp-buffer
    (insert "obj.scx")
    (setq buffer-file-name "/tmp/main.ts")
    (goto-char (point-max))
    (let* ((client
            (vortel-lsp-test-make-client
             :capabilities (vortel-lsp-make-hash "completionProvider"
                                                 (vortel-lsp-make-hash))))
           (item-a
            (vortel-lsp-make-hash
             "label" "scanf"
             "filterText" "scanf"
             "textEdit" (vortel-lsp-make-hash
                         "range" (vortel-lsp-make-hash
                                  "start" (vortel-lsp-make-hash "line" 0 "character" 3)
                                  "end" (vortel-lsp-make-hash "line" 0 "character" 7))
                         "newText" "scanf")))
           (item-b
            (vortel-lsp-make-hash
             "label" "schema"
             "filterText" "schema"
             "textEdit" (vortel-lsp-make-hash
                         "range" (vortel-lsp-make-hash
                                  "start" (vortel-lsp-make-hash "line" 0 "character" 4)
                                  "end" (vortel-lsp-make-hash "line" 0 "character" 7))
                         "newText" "schema"))))
      (cl-letf (((symbol-function 'vortel-lsp--attachments-for-feature)
                 (lambda (feature)
                   (if (string= feature "completion")
                       (list (list :client client))
                     nil)))
                ((symbol-function 'vortel-lsp--request-sync)
                 (lambda (&rest _args)
                   (list :ok t :result (list item-a item-b)))))
        (let* ((capf (vortel-lsp--completion-at-point))
               (symbol-bounds (bounds-of-thing-at-point 'symbol)))
          (should (equal (cons (nth 0 capf) (nth 1 capf))
                         symbol-bounds)))))))

(ert-deftest vortel-lsp-test-completion-at-point-uses-insert-replace-edit-range ()
  (with-temp-buffer
    (insert "obj.scx")
    (setq buffer-file-name "/tmp/main.ts")
    (goto-char (point-max))
    (let* ((client
            (vortel-lsp-test-make-client
             :capabilities (vortel-lsp-make-hash "completionProvider"
                                                 (vortel-lsp-make-hash))))
           (item
            (vortel-lsp-make-hash
             "label" "scanf"
             "filterText" "scanf"
             "textEdit" (vortel-lsp-make-hash
                         "insert" (vortel-lsp-make-hash
                                   "start" (vortel-lsp-make-hash "line" 0 "character" 4)
                                   "end" (vortel-lsp-make-hash "line" 0 "character" 7))
                         "replace" (vortel-lsp-make-hash
                                    "start" (vortel-lsp-make-hash "line" 0 "character" 3)
                                    "end" (vortel-lsp-make-hash "line" 0 "character" 7))
                         "newText" "scanf"))))
      (cl-letf (((symbol-function 'vortel-lsp--attachments-for-feature)
                 (lambda (feature)
                   (if (string= feature "completion")
                       (list (list :client client))
                     nil)))
                ((symbol-function 'vortel-lsp--request-sync)
                 (lambda (&rest _args)
                   (list :ok t :result (list item)))))
        (let ((capf (vortel-lsp--completion-at-point)))
          (should (equal (cons (nth 0 capf) (nth 1 capf))
                         (cons 4 8))))))))

(ert-deftest vortel-lsp-test-completion-at-point-suppressed-inside-round-parens ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "fn(arg")
    (goto-char (point-max))
    (let* ((client
            (vortel-lsp-test-make-client
             :capabilities (vortel-lsp-make-hash "completionProvider"
                                                 (vortel-lsp-make-hash))))
           (requested nil))
      (cl-letf (((symbol-function 'vortel-lsp--attachments-for-feature)
                 (lambda (feature)
                   (if (string= feature "completion")
                       (list (list :client client))
                     nil)))
                ((symbol-function 'vortel-lsp--request-sync)
                 (lambda (&rest _args)
                   (setq requested t)
                   (list :ok t :result nil))))
        (should-not (vortel-lsp--completion-at-point))
        (should-not requested)))))

(ert-deftest vortel-lsp-test-completion-request-context-uses-trigger-character ()
  (with-temp-buffer
    (insert "mainRef.")
    (goto-char (point-max))
    (let ((client
           (vortel-lsp-test-make-client
            :capabilities
            (vortel-lsp-make-hash
             "completionProvider"
             (vortel-lsp-make-hash
              "triggerCharacters" (list "." ":"))))))
      (let ((context (vortel-lsp--completion-request-context client)))
        (should (= (vortel-lsp-hash-get context "triggerKind") 2))
        (should (equal (vortel-lsp-hash-get context "triggerCharacter") "."))))))

(ert-deftest vortel-lsp-test-after-change-auto-completes-on-single-prefix-char ()
  (with-temp-buffer
    (let* ((client
            (vortel-lsp-test-make-client
             :capabilities
             (vortel-lsp-make-hash
              "completionProvider"
              (vortel-lsp-make-hash "triggerCharacters" (list ".")))))
           (vortel-lsp-mode t)
           (vortel-lsp-enable-capf t)
           (vortel-lsp-auto-completion t)
           (vortel-lsp-auto-completion-min-chars 1)
           (vortel-lsp-auto-completion-trigger-characters t)
           (vortel-lsp--attachments (list (list :client client
                                              :server-entry (vortel-lsp-make-hash))))
           (vortel-lsp--pending-changes nil)
           (vortel-lsp--before-change-ranges nil)
           (vortel-lsp--change-timer nil)
           (this-command 'self-insert-command)
           (auto-complete-called nil))
      (insert "s")
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _args) 'timer))
                ((symbol-function 'completion-at-point)
                 (lambda ()
                   (setq auto-complete-called t))))
        (vortel-lsp--after-change 1 2 0)
        (should auto-complete-called)))))

(ert-deftest vortel-lsp-test-after-change-auto-completes-on-trigger-char ()
  (with-temp-buffer
    (let* ((client
            (vortel-lsp-test-make-client
             :capabilities
             (vortel-lsp-make-hash
              "completionProvider"
              (vortel-lsp-make-hash "triggerCharacters" (list ".")))))
           (vortel-lsp-mode t)
           (vortel-lsp-enable-capf t)
           (vortel-lsp-auto-completion t)
           (vortel-lsp-auto-completion-min-chars 3)
           (vortel-lsp-auto-completion-trigger-characters t)
           (vortel-lsp--attachments (list (list :client client
                                              :server-entry (vortel-lsp-make-hash))))
           (vortel-lsp--pending-changes nil)
           (vortel-lsp--before-change-ranges nil)
           (vortel-lsp--change-timer nil)
           (this-command 'self-insert-command)
           (auto-complete-called nil))
      (insert ".")
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _args) 'timer))
                ((symbol-function 'completion-at-point)
                 (lambda ()
                   (setq auto-complete-called t))))
        (vortel-lsp--after-change 1 2 0)
        (should auto-complete-called)))))

(ert-deftest vortel-lsp-test-after-change-no-auto-complete-in-string-by-default ()
  (with-temp-buffer
    (let* ((client
            (vortel-lsp-test-make-client
             :capabilities
             (vortel-lsp-make-hash
              "completionProvider"
              (vortel-lsp-make-hash "triggerCharacters" (list ".")))))
           (vortel-lsp-mode t)
           (vortel-lsp-enable-capf t)
           (vortel-lsp-auto-completion t)
           (vortel-lsp-auto-completion-in-strings nil)
           (vortel-lsp-auto-completion-min-chars 1)
           (vortel-lsp-auto-completion-trigger-characters t)
           (vortel-lsp--attachments (list (list :client client
                                              :server-entry (vortel-lsp-make-hash))))
           (vortel-lsp--pending-changes nil)
           (vortel-lsp--before-change-ranges nil)
           (vortel-lsp--change-timer nil)
           (this-command 'self-insert-command)
           (auto-complete-called nil))
      (insert "\"x\"")
      (goto-char 3)
      (insert "i")
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (&rest _args) 'timer))
                ((symbol-function 'completion-at-point)
                 (lambda ()
                   (setq auto-complete-called t))))
        (vortel-lsp--after-change 3 4 0)
        (should-not auto-complete-called)))))

(ert-deftest vortel-lsp-test-auto-trigger-company-does-not-retrigger-when-active ()
  (with-temp-buffer
    (setq-local company-mode t)
    (setq-local company-candidates '("AssertionError"))
    (let ((called nil))
      (cl-letf (((symbol-function 'company-manual-begin)
                 (lambda () (setq called t))))
        (vortel-lsp--auto-trigger-completion)
        (should-not called)))))

(ert-deftest vortel-lsp-test-auto-trigger-company-begins-when-inactive ()
  (with-temp-buffer
    (setq-local company-mode t)
    (setq-local company-candidates nil)
    (let ((called nil))
      (cl-letf (((symbol-function 'company-manual-begin)
                 (lambda () (setq called t))))
        (vortel-lsp--auto-trigger-completion)
        (should called)))))

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
