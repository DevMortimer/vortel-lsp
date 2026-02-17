;;; vortel-lsp-config-tests.el --- Config tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: vortel-lsp contributors

;;; Code:

(require 'cl-lib)
(require 'ert)

(require 'vortel-lsp-config)
(require 'vortel-lsp-util)
(require 'vortel-lsp-test-helpers)

;;; --- catalog path resolution ---

(ert-deftest vortel-lsp-test-config-elpaca-repo-catalog-fallback ()
  "Resolve catalog from Elpaca repos when loading from builds dir."
  (vortel-lsp-test-with-temp-dir root
    (let* ((build-dir (expand-file-name "elpaca/builds/vortel-lsp" root))
           (repo-dir (expand-file-name "elpaca/repos/vortel-lsp" root))
           (catalog (expand-file-name "vortel-lsp-catalog.json" repo-dir)))
      (make-directory build-dir t)
      (make-directory repo-dir t)
      (with-temp-file catalog
        (insert "{}"))
      (should (equal (vortel-lsp-config--elpaca-repo-catalog build-dir)
                     catalog)))))

(ert-deftest vortel-lsp-test-config-default-catalog-prefers-local-file ()
  "Use catalog in package dir before trying Elpaca fallback."
  (vortel-lsp-test-with-temp-dir root
    (let* ((build-dir (expand-file-name "elpaca/builds/vortel-lsp" root))
           (repo-dir (expand-file-name "elpaca/repos/vortel-lsp" root))
           (build-catalog (expand-file-name "vortel-lsp-catalog.json" build-dir))
           (repo-catalog (expand-file-name "vortel-lsp-catalog.json" repo-dir)))
      (make-directory build-dir t)
      (make-directory repo-dir t)
      (with-temp-file build-catalog
        (insert "{}"))
      (with-temp-file repo-catalog
        (insert "{\"from\":\"repo\"}"))
      (let ((load-file-name (expand-file-name "vortel-lsp-config.el" build-dir))
            (buffer-file-name nil)
            (default-directory root))
        (should (equal (vortel-lsp-config--default-catalog-file)
                       build-catalog))))))

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

;;; --- deep-merge ---

(ert-deftest vortel-lsp-test-config-deep-merge-disjoint-keys ()
  "Disjoint keys from both tables appear in result."
  (let* ((a (vortel-lsp-make-hash "x" 1))
         (b (vortel-lsp-make-hash "y" 2))
         (result (vortel-lsp-config--deep-merge a b)))
    (should (= (gethash "x" result) 1))
    (should (= (gethash "y" result) 2))))

(ert-deftest vortel-lsp-test-config-deep-merge-override-wins ()
  "Override value replaces base for same key."
  (let* ((a (vortel-lsp-make-hash "x" 1))
         (b (vortel-lsp-make-hash "x" 42))
         (result (vortel-lsp-config--deep-merge a b)))
    (should (= (gethash "x" result) 42))))

(ert-deftest vortel-lsp-test-config-deep-merge-nested ()
  "Nested hash-tables are merged recursively."
  (let* ((a (vortel-lsp-make-hash
             "outer" (vortel-lsp-make-hash "a" 1 "b" 2)))
         (b (vortel-lsp-make-hash
             "outer" (vortel-lsp-make-hash "b" 99 "c" 3)))
         (result (vortel-lsp-config--deep-merge a b))
         (inner (gethash "outer" result)))
    (should (= (gethash "a" inner) 1))
    (should (= (gethash "b" inner) 99))
    (should (= (gethash "c" inner) 3))))

(ert-deftest vortel-lsp-test-config-deep-merge-does-not-mutate ()
  "Original tables are not mutated."
  (let* ((a (vortel-lsp-make-hash "x" 1))
         (b (vortel-lsp-make-hash "x" 2 "y" 3))
         (_result (vortel-lsp-config--deep-merge a b)))
    (should (= (gethash "x" a) 1))
    (should-not (gethash "y" a))))

;;; --- server-settings resolver ---

(ert-deftest vortel-lsp-test-config-server-settings-catalog-only ()
  "Returns catalog config when no user overrides exist."
  (let* ((catalog-config (vortel-lsp-make-hash "typescript" (vortel-lsp-make-hash "tsdk" "lib")))
         (server-ht (vortel-lsp-make-hash "config" catalog-config))
         (servers-ht (vortel-lsp-make-hash "test-server" server-ht))
         (catalog (vortel-lsp-make-hash "servers" servers-ht "languages" '()))
         (vortel-lsp--catalog-cache catalog)
         (vortel-lsp-server-settings nil))
    (let ((result (vortel-lsp-config-server-settings "test-server")))
      (should (hash-table-p result))
      (should (equal (gethash "tsdk" (gethash "typescript" result)) "lib")))))

(ert-deftest vortel-lsp-test-config-server-settings-user-override ()
  "User overrides are merged on top of catalog config."
  (let* ((catalog-config (vortel-lsp-make-hash "key1" "catalog-val"))
         (server-ht (vortel-lsp-make-hash "config" catalog-config))
         (servers-ht (vortel-lsp-make-hash "test-server" server-ht))
         (catalog (vortel-lsp-make-hash "servers" servers-ht "languages" '()))
         (vortel-lsp--catalog-cache catalog)
         (vortel-lsp-server-settings
          '(("test-server" . (("key1" . "user-val") ("key2" . "new"))))))
    (let ((result (vortel-lsp-config-server-settings "test-server")))
      (should (equal (gethash "key1" result) "user-val"))
      (should (equal (gethash "key2" result) "new")))))

(ert-deftest vortel-lsp-test-config-server-settings-unknown-server ()
  "Unknown server returns empty hash-table."
  (let* ((servers-ht (vortel-lsp-make-hash))
         (catalog (vortel-lsp-make-hash "servers" servers-ht "languages" '()))
         (vortel-lsp--catalog-cache catalog)
         (vortel-lsp-server-settings nil))
    (let ((result (vortel-lsp-config-server-settings "nonexistent")))
      (should (hash-table-p result))
      (should (= (hash-table-count result) 0)))))

(provide 'vortel-lsp-config-tests)

;;; vortel-lsp-config-tests.el ends here
