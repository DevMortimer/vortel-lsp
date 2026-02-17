;;; vortel-lsp-util.el --- Shared helpers for vortel-lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors
;; Keywords: languages, tools

;;; Commentary:

;; Shared utility helpers used by vortel-lsp modules.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-util)

(defgroup vortel-lsp nil
  "Helix-inspired LSP client for Emacs."
  :group 'tools
  :prefix "vortel-lsp-")

(defcustom vortel-lsp-log-io nil
  "When non-nil, log transport I/O payloads."
  :type 'boolean
  :group 'vortel-lsp)

(defcustom vortel-lsp-log-buffer-name "*vortel-lsp-log*"
  "Buffer used for vortel-lsp logs."
  :type 'string
  :group 'vortel-lsp)

(defconst vortel-lsp--json-false
  (if (boundp 'json-false) json-false :json-false)
  "JSON false sentinel used in this package.")

(defun vortel-lsp-log (format-string &rest args)
  "Append a log line built from FORMAT-STRING and ARGS."
  (with-current-buffer (get-buffer-create vortel-lsp-log-buffer-name)
    (goto-char (point-max))
    (insert (format-time-string "%Y-%m-%d %H:%M:%S.%3N "))
    (insert (apply #'format format-string args))
    (insert "\n")))

(defun vortel-lsp-json-false-p (value)
  "Return non-nil when VALUE is the JSON false sentinel."
  (eq value vortel-lsp--json-false))

(defun vortel-lsp-truthy-p (value)
  "Return non-nil when VALUE is semantically true for JSON fields."
  (and value (not (vortel-lsp-json-false-p value))))

(defun vortel-lsp-hash-get (table key &optional default)
  "Read KEY from hash TABLE.
Return DEFAULT when TABLE is nil or KEY is absent."
  (if (hash-table-p table)
      (gethash key table default)
    default))

(defun vortel-lsp-make-hash (&rest key-values)
  "Build a hash table from KEY-VALUES.
KEY-VALUES is interpreted as alternating key and value entries."
  (let ((table (make-hash-table :test #'equal)))
    (while key-values
      (let ((key (pop key-values))
            (value (pop key-values)))
        (puthash key value table)))
    table))

(defun vortel-lsp-hash-to-alist (table)
  "Convert hash TABLE into an alist.
Return nil when TABLE is nil."
  (when (hash-table-p table)
    (let (items)
      (maphash (lambda (k v) (push (cons k v) items)) table)
      items)))

(defun vortel-lsp-path-to-uri (path)
  "Convert PATH into a file URI string."
  (url-encode-url (concat "file://" (expand-file-name path))))

(defun vortel-lsp-uri-to-path (uri)
  "Convert file URI into a local absolute path.
Return nil for non-file URIs."
  (when (and (stringp uri) (string-prefix-p "file://" uri))
    (let ((path (url-unhex-string (substring uri 7))))
      (if (and (eq system-type 'windows-nt)
               (string-match-p "^/[A-Za-z]:" path))
          (substring path 1)
        path))))

(defun vortel-lsp-hash-get-section (table section)
  "Traverse TABLE by dot-separated SECTION string.
Return the nested value, or nil when any key is missing.
When SECTION is nil or empty, return TABLE itself."
  (if (or (null section) (string-empty-p section))
      table
    (let ((keys (split-string section "\\." t))
          (current table))
      (while (and keys current (hash-table-p current))
        (setq current (gethash (pop keys) current)))
      (if keys nil current))))

(provide 'vortel-lsp-util)

;;; vortel-lsp-util.el ends here
