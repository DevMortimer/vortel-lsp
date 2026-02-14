;;; vortel-lsp-transport-tests.el --- Transport tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors

;;; Code:

(require 'cl-lib)
(require 'ert)

(require 'vortel-lsp-transport)
(require 'vortel-lsp-test-helpers)

;;; --- node_modules/.bin PATH prepend ---

(ert-deftest vortel-lsp-test-transport-prepend-node-bin-when-exists ()
  "PATH is prepended with node_modules/.bin when the directory exists."
  (vortel-lsp-test-with-temp-dir dir
    (let* ((bin-dir (expand-file-name "node_modules/.bin" dir))
           (env (list "PATH=/usr/bin:/bin" "HOME=/home/user")))
      (make-directory bin-dir t)
      (let ((result (vortel-lsp-transport--maybe-prepend-node-bin env dir)))
        (should (string-prefix-p (concat "PATH=" bin-dir ":")
                                 (car result)))
        (should (string-match-p "/usr/bin:/bin" (car result)))
        ;; HOME unchanged
        (should (member "HOME=/home/user" result))))))

(ert-deftest vortel-lsp-test-transport-no-prepend-when-absent ()
  "ENV is returned unchanged when node_modules/.bin doesn't exist."
  (vortel-lsp-test-with-temp-dir dir
    (let ((env (list "PATH=/usr/bin" "HOME=/home/user")))
      (let ((result (vortel-lsp-transport--maybe-prepend-node-bin env dir)))
        (should (equal result env))))))

(ert-deftest vortel-lsp-test-transport-creates-path-when-missing ()
  "A PATH entry is created when none exists in ENV."
  (vortel-lsp-test-with-temp-dir dir
    (let* ((bin-dir (expand-file-name "node_modules/.bin" dir))
           (env (list "HOME=/home/user")))
      (make-directory bin-dir t)
      (let ((result (vortel-lsp-transport--maybe-prepend-node-bin env dir)))
        (should (equal (car result) (concat "PATH=" bin-dir)))
        (should (member "HOME=/home/user" result))))))

(ert-deftest vortel-lsp-test-transport-finds-node-bin-in-parent ()
  "PATH is prepended with node_modules/.bin found in parent directory."
  (vortel-lsp-test-with-temp-dir dir
    (let* ((bin-dir (expand-file-name "node_modules/.bin" dir))
           (subdir (expand-file-name "src" dir))
           (env (list "PATH=/usr/bin:/bin" "HOME=/home/user")))
      (make-directory bin-dir t)
      (make-directory subdir t)
      (let ((result (vortel-lsp-transport--maybe-prepend-node-bin env subdir)))
        (should (string-prefix-p (concat "PATH=" bin-dir ":")
                                 (car result)))
        (should (string-match-p "/usr/bin:/bin" (car result)))
        ;; HOME unchanged
        (should (member "HOME=/home/user" result))))))

(provide 'vortel-lsp-transport-tests)

;;; vortel-lsp-transport-tests.el ends here
