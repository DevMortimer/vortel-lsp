;;; vortel-lsp-tests.el --- Test entrypoint for vortel-lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors

;;; Code:

(require 'ert)

(require 'vortel-lsp-test-helpers)
(require 'vortel-lsp-apply-edit-tests)
(require 'vortel-lsp-completion-tests)
(require 'vortel-lsp-actions-rename-tests)
(require 'vortel-lsp-transport-tests)
(require 'vortel-lsp-config-tests)

(provide 'vortel-lsp-tests)

;;; vortel-lsp-tests.el ends here
