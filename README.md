# vortel-lsp

`vortel-lsp` is an Emacs LSP package with a Helix-inspired architecture.

## Usage

### 1) Load package files

Add this to your Emacs config:

```elisp
(add-to-list 'load-path "/path/to/vortel-lsp")
(require 'vortel-lsp)
```

### 2) Enable in programming buffers

```elisp
(add-hook 'prog-mode-hook #'vortel-lsp-mode)
```

Or enable manually with `M-x vortel-lsp-mode`.

### 3) Override server commands when needed

```elisp
  (setq vortel-lsp-server-overrides
        '(("typescript-language-server"
          :command "typescript-language-server"
          :args ("--stdio"))))
```

### 4) Missing servers and server selection

By default, `vortel-lsp` skips missing language servers and warns once per
(server, workspace). This avoids repeated errors when your catalog lists
multiple servers but you only have one installed.

Prefer a single server for a language (example: only `ty` for Python):

```elisp
(setq vortel-lsp-preferred-servers
      '(("python" . ("ty"))))
```

Silence missing-server warnings (still skips missing executables):

```elisp
(setq vortel-lsp-missing-server-policy 'silent-skip)
```

Disable a server globally:

```elisp
(setq vortel-lsp-disabled-servers '("pylsp" "jedi" "ruff"))
```

### 5) Useful commands

- `M-x vortel-lsp-hover`
- `M-x vortel-lsp-find-definition`
- `M-x vortel-lsp-find-references`
- `M-x vortel-lsp-rename-symbol`
- `M-x vortel-lsp-code-actions`
- `M-x vortel-lsp-signature-help`

### 6) Signature help

`vortel-lsp` displays a Helix-style argument hint box while you type
inside `(...)` argument lists. Trigger characters `(` and `,` activate
it automatically, and the active argument is highlighted for both
positional and named/non-positional arguments when possible.

Signature hints and completion can both be used inside `(...)`.
Typing completion trigger characters (such as `.`) still requests LSP
candidates while the signature hint box remains available. The floating
box is placed below point when possible, and falls back beside or above
based on available window space.

Moving or clicking inside argument lists also refreshes signature help.

```elisp
;; Disable auto signature help (use M-x vortel-lsp-signature-help manually)
(setq vortel-lsp-signature-help-mode 'manual)

;; Adjust doc line limit
(setq vortel-lsp-signature-help-max-doc-lines 3)
```

Manual invocation: `M-x vortel-lsp-signature-help`

### 7) Notes

- Keep `vortel-lsp-catalog.json` available (default: package dir).
- Elpaca installs are auto-detected: when loaded from `elpaca/builds`,
  `vortel-lsp` falls back to the matching `elpaca/repos` catalog file.
- Diagnostics are integrated with Flymake by default.
- Diagnostics at point are shown in the echo area by default.
- Hover docs are shown in a floating frame while point is on a symbol.
- Completion is exposed through Emacs CAPF.
