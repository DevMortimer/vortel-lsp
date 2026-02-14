;;; vortel-lsp-jsonrpc.el --- JSON-RPC framing for vortel-lsp -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors
;; Keywords: languages, tools

;;; Commentary:

;; Content-Length framing and JSON encode/decode helpers for LSP transport.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'vortel-lsp-util)

(cl-defstruct (vortel-lsp-jsonrpc-parser
               (:constructor vortel-lsp-jsonrpc-parser-create))
  "Incremental parser for JSON-RPC Content-Length framed messages."
  (buffer "" :type string)
  (expected-bytes nil :type (or null integer)))

(defun vortel-lsp-jsonrpc--content-length (header)
  "Extract Content-Length from HEADER string.
Return nil when the header does not include a valid Content-Length line."
  (let ((case-fold-search t)
        (content-length nil))
    (dolist (line (split-string header "\r\n" t))
      (when (string-match "\\`Content-Length:[ \\t]*\\([0-9]+\\)[ \\t]*\\'" line)
        (setq content-length (string-to-number (match-string 1 line)))))
    content-length))

(defun vortel-lsp-jsonrpc-encode-message (message)
  "Serialize MESSAGE as a framed JSON-RPC payload.
The returned string is unibyte data ready for `process-send-string'."
  (let* ((json-false vortel-lsp--json-false)
         (json-text (json-serialize message))
         (json-bytes (encode-coding-string json-text 'utf-8))
         (header (format "Content-Length: %d\r\n\r\n" (string-bytes json-bytes))))
    (concat (encode-coding-string header 'us-ascii) json-bytes)))

(defun vortel-lsp-jsonrpc-parser-feed (parser chunk)
  "Feed CHUNK into PARSER and return a list of parsed JSON objects.
CHUNK may contain partial frames or multiple frames."
  (setq chunk (encode-coding-string chunk 'binary))
  (setf (vortel-lsp-jsonrpc-parser-buffer parser)
        (concat (vortel-lsp-jsonrpc-parser-buffer parser) chunk))
  (let (messages)
    (cl-loop
     with keep-going = t
     while keep-going
     do
     (setq keep-going nil)
     (let ((buffer (vortel-lsp-jsonrpc-parser-buffer parser))
           (expected (vortel-lsp-jsonrpc-parser-expected-bytes parser)))
       (if expected
           (when (>= (string-bytes buffer) expected)
             (let* ((payload-bytes (substring buffer 0 expected))
                    (payload-text (decode-coding-string payload-bytes 'utf-8 t))
                    (payload
                     (json-parse-string
                      payload-text
                      :object-type 'hash-table
                      :array-type 'list
                      :null-object nil
                      :false-object vortel-lsp--json-false)))
               (setf (vortel-lsp-jsonrpc-parser-buffer parser)
                     (substring buffer expected)
                     (vortel-lsp-jsonrpc-parser-expected-bytes parser) nil)
               (push payload messages)
               (setq keep-going t)))
         (let ((header-end (string-match "\r\n\r\n" buffer)))
           (when header-end
             (let* ((header (substring buffer 0 header-end))
                    (header-size (+ header-end 4))
                    (content-length (vortel-lsp-jsonrpc--content-length header)))
               (unless (integerp content-length)
                 (error "Malformed LSP header (missing Content-Length): %s" header))
               (setf (vortel-lsp-jsonrpc-parser-buffer parser)
                     (substring buffer header-size)
                     (vortel-lsp-jsonrpc-parser-expected-bytes parser)
                     content-length)
               (setq keep-going t)))))))
    (nreverse messages)))

(provide 'vortel-lsp-jsonrpc)

;;; vortel-lsp-jsonrpc.el ends here
