;;; vortel-lsp-config-tests.el --- Config tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors

;;; Code:

(require 'cl-lib)
(require 'ert)

(require 'vortel-lsp-config)
(require 'vortel-lsp-util)
(require 'vortel-lsp-test-helpers)

;;; --- extra-server-to-hash conversion ---

(ert-deftest vortel-lsp-test-config-extra-server-to-hash-basic ()
  "Plist with :name converts to hash with \"name\" key."
  (let ((ht (vortel-lsp-config--extra-server-to-hash '(:name "tailwindcss-ls"))))
    (should (hash-table-p ht))
    (should (equal (gethash "name" ht) "tailwindcss-ls"))
    (should-not (gethash "only" ht))
    (should-not (gethash "except" ht))))

(ert-deftest vortel-lsp-test-config-extra-server-to-hash-with-only-except ()
  "Plist with :only and :except populates corresponding hash keys."
  (let ((ht (vortel-lsp-config--extra-server-to-hash
             '(:name "css-ls" :only ("completion") :except ("diagnostics")))))
    (should (equal (gethash "name" ht) "css-ls"))
    (should (equal (gethash "only" ht) '("completion")))
    (should (equal (gethash "except" ht) '("diagnostics")))))

;;; --- language-servers merge ---

(ert-deftest vortel-lsp-test-config-language-servers-catalog-only ()
  "Returns catalog servers when no extras are configured."
  (let ((vortel-lsp-extra-servers nil)
        (lang (vortel-lsp-make-hash
               "name" "svelte"
               "servers" (list (vortel-lsp-make-hash "name" "svelteserver")))))
    (let ((servers (vortel-lsp-config-language-servers lang)))
      (should (= (length servers) 1))
      (should (equal (gethash "name" (car servers)) "svelteserver")))))

(ert-deftest vortel-lsp-test-config-language-servers-merges-extras ()
  "Extra servers are appended after catalog servers."
  (let ((vortel-lsp-extra-servers
         '(("svelte" . ((:name "tailwindcss-ls")))))
        (lang (vortel-lsp-make-hash
               "name" "svelte"
               "servers" (list (vortel-lsp-make-hash "name" "svelteserver")))))
    (let ((servers (vortel-lsp-config-language-servers lang)))
      (should (= (length servers) 2))
      (should (equal (gethash "name" (car servers)) "svelteserver"))
      (should (equal (gethash "name" (cadr servers)) "tailwindcss-ls")))))

(ert-deftest vortel-lsp-test-config-language-servers-dedup-catalog-wins ()
  "When an extra has the same name as a catalog server, catalog wins."
  (let ((vortel-lsp-extra-servers
         '(("svelte" . ((:name "svelteserver" :only ("completion"))))))
        (lang (vortel-lsp-make-hash
               "name" "svelte"
               "servers" (list (vortel-lsp-make-hash "name" "svelteserver")))))
    (let ((servers (vortel-lsp-config-language-servers lang)))
      (should (= (length servers) 1))
      ;; The catalog entry (no "only" key) should be kept
      (should-not (gethash "only" (car servers))))))

(ert-deftest vortel-lsp-test-config-language-servers-empty-extras ()
  "Empty extras for a language don't affect results."
  (let ((vortel-lsp-extra-servers '(("svelte" . ())))
        (lang (vortel-lsp-make-hash
               "name" "svelte"
               "servers" (list (vortel-lsp-make-hash "name" "svelteserver")))))
    (let ((servers (vortel-lsp-config-language-servers lang)))
      (should (= (length servers) 1)))))

(ert-deftest vortel-lsp-test-config-language-servers-no-catalog-servers ()
  "Extras work even when language has no catalog servers."
  (let ((vortel-lsp-extra-servers
         '(("markdown" . ((:name "marksman")))))
        (lang (vortel-lsp-make-hash "name" "markdown")))
    (let ((servers (vortel-lsp-config-language-servers lang)))
      (should (= (length servers) 1))
      (should (equal (gethash "name" (car servers)) "marksman")))))

(provide 'vortel-lsp-config-tests)

;;; vortel-lsp-config-tests.el ends here
