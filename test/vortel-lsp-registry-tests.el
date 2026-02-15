;;; vortel-lsp-registry-tests.el --- Registry tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors

;;; Code:

(require 'cl-lib)
(require 'ert)

(require 'vortel-lsp-registry)
(require 'vortel-lsp-util)

(ert-deftest vortel-lsp-test-registry-skips-missing-server-when-warn ()
  "Missing executables are warned once and skipped." 
  (let ((vortel-lsp-missing-server-policy 'warn-and-skip)
        (vortel-lsp-registry--missing-warnings (make-hash-table :test #'equal))
        (warn-count 0))
    (cl-letf (((symbol-function 'vortel-lsp-config-server-definition)
               (lambda (_name) (list :name "ruff" :command "ruff" :args nil :timeout 1)))
              ((symbol-function 'vortel-lsp-transport--resolve-command)
               (lambda (_cmd _root) nil))
              ((symbol-function 'vortel-lsp-transport--format-command-missing-error)
               (lambda (_name _cmd _cwd) "missing"))
              ((symbol-function 'display-warning)
               (lambda (_type _message &rest _args) (setq warn-count (1+ warn-count)))))
      (should-not (vortel-lsp-registry--start-client "ruff" "/tmp"))
      (should (= warn-count 1))
      ;; second attempt: still skipped, but no additional warning
      (should-not (vortel-lsp-registry--start-client "ruff" "/tmp"))
      (should (= warn-count 1)))))

(ert-deftest vortel-lsp-test-registry-errors-missing-server-when-error ()
  "Missing executables error when policy is `error'." 
  (let ((vortel-lsp-missing-server-policy 'error))
    (cl-letf (((symbol-function 'vortel-lsp-config-server-definition)
               (lambda (_name) (list :name "ruff" :command "ruff" :args nil :timeout 1)))
              ((symbol-function 'vortel-lsp-transport--resolve-command)
               (lambda (_cmd _root) nil))
              ((symbol-function 'vortel-lsp-transport--format-command-missing-error)
               (lambda (_name _cmd _cwd) "missing")))
      (should-error (vortel-lsp-registry--start-client "ruff" "/tmp") :type 'error))))

(ert-deftest vortel-lsp-test-registry-filters-preferred-and-disabled ()
  "Preferred servers are ordered and disabled servers are excluded." 
  (let* ((language (vortel-lsp-make-hash "name" "python"))
         (entries (list (vortel-lsp-make-hash "name" "ty")
                        (vortel-lsp-make-hash "name" "ruff")
                        (vortel-lsp-make-hash "name" "jedi")))
         (vortel-lsp-preferred-servers '(("python" . ("ruff" "ty"))))
         (vortel-lsp-disabled-servers '("ruff")))
    (let* ((filtered (vortel-lsp-registry--filter-language-server-entries language entries))
           (names (mapcar (lambda (e) (vortel-lsp-hash-get e "name")) filtered)))
      (should (equal names '("ty"))))))

(provide 'vortel-lsp-registry-tests)

;;; vortel-lsp-registry-tests.el ends here
