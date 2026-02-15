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

### 6) Notes

- Keep `vortel-lsp-catalog.json` available (default: repo root).
- Diagnostics are integrated with Flymake by default.
- Completion is exposed through Emacs CAPF.
