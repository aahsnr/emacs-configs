# Comprehensive Doom Emacs Setup with Nix Shell

This guide will walk you through setting up Doom Emacs to work
seamlessly with a Nix shell, providing a reproducible and isolated
development environment for your programming projects.

------------------------------------------------------------------------

## Why Nix Shell with Doom Emacs?

# **Reproducibility**: Ensure that everyone working on a project has the exact same development tools and versions.

# **Isolation**: Avoid polluting your global system with project-specific dependencies.

# **Declarative**: Define your development environment in a simple `shell.nix`{.verbatim} file.

# **Flexibility**: Easily switch between different project environments without conflicts.

------------------------------------------------------------------------

## Prerequisites

1.  **Nix Package Manager**: Install Nix on your system. Follow the
    instructions on the official Nix website: [Nix
    Download](https://nixos.org/download.html)
2.  **Doom Emacs**: Install Doom Emacs. You can find instructions here:
    [Doom Emacs GitHub](https://github.com/doomemacs/doomemacs)
    -   It\'s recommended to install Doom Emacs globally or in your user
        directory, and then manage project-specific dependencies via
        Nix.

------------------------------------------------------------------------

## Step 1: Create Your `shell.nix`{.verbatim} File

This file defines the development environment for your project. Place
this `shell.nix`{.verbatim} file in the root of your project directory.

Let\'s create an example `shell.nix`{.verbatim} for a Python and Node.js
project, including common tools like `git`{.verbatim},
`docker`{.verbatim}, and language servers.

``` nix
# shell.nix
{ pkgs ? import <nixpkgs> {} }:

let
  # Define specific versions for languages if needed
  pythonEnv = pkgs.python311.withPackages (p: with p; [
    pip
    venv
    black # Python formatter
    isort # Python import sorter
    pylint # Python linter
  ]);

  nodejsEnv = pkgs.nodejs_20.withPackages (p: with p; [
    npm
    yarn
  ]);

in
pkgs.mkShell {
  # Build inputs are packages available in the shell environment
  buildInputs = with pkgs; [
    # Core development tools
    git
    docker # For containerized development
    sqlite-interactive # Example database client

    # Language-specific tools
    pythonEnv
    nodejsEnv

    # Language Servers (LSPs) for Emacs
    # Ensure these match what your Doom Emacs configuration expects
    nil # Nix Language Server
    nodePackages.typescript-language-server # TypeScript LSP
    nodePackages.vscode-langservers-extracted # HTML, CSS, JSON LSP
    nodePackages.prettier # Code formatter
    pyright # Python LSP (often preferred over pylsp)
    gopls # Go LSP
    rust-analyzer # Rust LSP
    clang-tools # C/C++ LSP and formatters
    cmake # Build system for C/C++
    texlive.combined.scheme-medium # LaTeX tools

    # Other useful utilities
    ripgrep # Fast grep, used by many Emacs packages (e.g., consult, vertico)
    fd # Fast find alternative
    jq # JSON processor
    direnv # For automatic shell loading (highly recommended)
  ];

  # Environment variables (optional)
  shellHook = ''
    echo "Entering Nix shell for project: $(basename $(pwd))"
    # Example: Set up a virtual environment for Python
    if [ -d "venv" ]; then
      source venv/bin/activate
      echo "Activated Python virtual environment."
    fi
  '';

  # Optional: Allow specific packages to be overridden
  # This is useful if you need a very specific version of a tool
  # For example, to use an older version of Node.js:
  # nodejs = pkgs.nodejs_18;
}
```

## Explanation of `shell.nix`{.verbatim}:

# `pkgs ? import <nixpkgs> {}`{.verbatim}: Imports the Nix Packages collection.

# `pythonEnv`{.verbatim}, `nodejsEnv`{.verbatim}: Custom derivations to include specific Python/Node.js packages. This is cleaner than listing all packages directly in `buildInputs`{.verbatim}.

# `pkgs.mkShell`{.verbatim}: The core function to create a development shell.

# `buildInputs`{.verbatim}: A list of packages that will be made available in your shell\'s `PATH`{.verbatim}. This is where you put your compilers, interpreters, language servers, formatters, linters, etc.

# `shellHook`{.verbatim}: A script that runs when you enter the Nix shell. Useful for activating virtual environments, setting environment variables, or displaying messages.

# `direnv`{.verbatim} integration: If you install `direnv`{.verbatim} (highly recommended), it can automatically load and unload your Nix shell when you `cd`{.verbatim} into and out of your project directory. Install `direnv`{.verbatim} and add `eval "$(direnv hook nix)"`{.verbatim} to your shell\'s `rc`{.verbatim} file (e.g., `.bashrc`{.verbatim}, `.zshrc`{.verbatim}).

------------------------------------------------------------------------

## Step 2: Configure Doom Emacs

Your Doom Emacs configuration needs to be aware of the tools provided by
the Nix shell. Doom Emacs generally handles this well because it
inherits the `PATH`{.verbatim} and other environment variables from the
shell it\'s launched from.

Here\'s how you might adjust your Doom Emacs
`~/.doom.d/config.el`{.verbatim} and `~/.doom.d/packages.el`{.verbatim}
to take advantage of the Nix shell.

### `~/.doom.d/init.el`{.verbatim}

Ensure you have the necessary modules enabled. For programming, you\'ll
likely want:

``` elisp
;; ~/.doom.d/init.el

;; Core modules
(doom! :input
       ;; ...
       :completion
       (company +childframe) ; Autocompletion
       (ivy +prescient)      ; Ivy completion framework
       ;; ...
       :ui
       ;; ...
       :editor
       (evil +everywhere) ; Vim keybindings
       (file-templates)  ; Snippets
       (format)          ; Code formatting
       (lookup +docsets) ; Documentation lookup
       (multiple-cursors)
       (nav-flash)
       (parinfer)
       (rotate-text)
       (snippets)
       (word-wrap)
       ;; ...
       :emacs
       (dired +icons)    ; File manager
       (electric)
       (undo-tree)
       (vc)              ; Version control
       ;; ...
       :checkers
       (syntax)          ; Syntax checking (flycheck)
       (spell)           ; Spell checking (flyspell)
       ;; ...
       :tools
       (ansible)
       (biblio)
       (debugger +lsp)   ; Debugger integration
       (docker)          ; Docker integration
       (editorconfig)
       (eval)            ; Code evaluation
       (gist)
       (lookup +docsets)
       (lsp)             ; Language Server Protocol
       (magit)           ; Git client
       (make)
       (pass)
       (pdf)
       (plantuml)
       (prodigy)
       (rgb)
       (terraform)
       (tmux)
       (upload)
       (vterm)           ; Terminal emulator
       ;; ...
       :lang
       (cc +lsp)         ; C/C++
       (clojure +lsp)
       (common-lisp)
       (crystal)
       (csharp +lsp)
       (dart +lsp)
       (elixir +lsp)
       (elm)
       (emacs-lisp)
       (erlang)
       (ess)             ; R, S
       (fsharp +lsp)
       (go +lsp)         ; Go
       (haskell +lsp)
       (java +lsp)       ; Java
       (javascript +lsp) ; JavaScript/TypeScript
       (json)
       (jsx)
       (kotlin +lsp)
       (latex +lsp)      ; LaTeX
       (ledger)
       (lua +lsp)        ; Lua
       (markdown)
       (nim)
       (nix)             ; Nix language support
       (ocaml +lsp)
       (org +dragndrop +hugo +present +roam) ; Org mode
       (perl +lsp)
       (php +lsp)
       (plantuml)
       (protobuf)
       (purescript)
       (python +lsp)     ; Python
       (qt)
       (racket)
       (rest)
       (ruby +lsp)       ; Ruby
       (rust +lsp)       ; Rust
       (scala +lsp)
       (sh)              ; Shell scripting
       (sql)
       (swift +lsp)
       (terra)
       (web)             ; HTML, CSS
       (yaml)
       ;; ...
       :app
       ;; ...
       :config
       (default +bindings +smartparens)
       ;; ...
       )
```

### `~/.doom.d/config.el`{.verbatim}

This is where you\'ll put your custom configurations. For Nix shell
integration, the key is that Doom Emacs will automatically pick up the
`PATH`{.verbatim} from the shell it\'s launched from. You generally
don\'t need explicit Nix-specific paths here, but you might want to
configure LSP servers or formatters to be aware of project-specific
settings.

``` elisp
;; ~/.doom.d/config.el

;; General Emacs configuration
(setq display-line-numbers 'relative ; Show relative line numbers
      fill-column 80               ; Set fill column for auto-wrapping
      tab-width 2                  ; Default tab width
      indent-tabs-mode nil         ; Use spaces instead of tabs
      )

;; LSP configuration (example for Python and TypeScript)
(use-package! lsp-pyright
  :after lsp
  :config
  (setq lsp-pyright-auto-venv-activate t) ; Automatically activate venv if found
  (add-hook 'python-mode-hook #'lsp))

(use-package! lsp-mode
  :config
  (setq lsp-enable-text-document-code-action t
        lsp-enable-text-document-code-lens t
        lsp-enable-snippet t
        lsp-headerline-breadcrumb-enable t
        lsp-completion-provider :none) ; Let company-mode handle completion
  (set-lsp-priority! 'pyright 200) ; Prioritize pyright over other Python LSPs
  (set-lsp-priority! 'typescript-language-server 200)
  )

;; Formatters (e.g., Black for Python, Prettier for JS/TS)
(use-package! apheleia
  :config
  (setq apheleia-mode-alist
        '((python-mode . black)
          (js-mode . prettier)
          (typescript-mode . prettier)
          (css-mode . prettier)
          (json-mode . prettier)
          (web-mode . prettier)
          (nix-mode . nixpkgs-fmt) ; If you install nixpkgs-fmt in your shell
          ))
  (apheleia-global-mode +1))

;; Vterm configuration
;; NOTE: vterm typically inherits the shell from the environment Emacs was launched from.
;; Hardcoding a path like "/run/current-system/sw/bin/bash" is usually only necessary
;; if running Emacs directly on NixOS, or if you need a specific shell not in your PATH.
;; For most cases, when launching Emacs from within a Nix shell (or via direnv),
;; vterm will automatically use the correct shell from that environment.
;; (setq vterm-shell "/run/current-system/sw/bin/bash")
(setq vterm-max-scrollback 10000)

;; Direnv integration (optional, but highly recommended)
;; This ensures Emacs picks up environment variables set by direnv
(use-package! direnv
  :config
  (direnv-mode))

;; Magit configuration (example)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;; Other useful configurations
(setq projectile-project-search-path '("~/projects/" "~/work/")) ; Customize project root search paths

;; Set up `exec-path` for GUI Emacs (important if you launch Emacs outside direnv)
;; If you always launch Emacs from within the Nix shell (or via direnv),
;; this might not be strictly necessary, but it's good practice.
(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH"))) ; For Homebrew on macOS
  (setq exec-path (append '("/opt/homebrew/bin") exec-path)))

;; Add Nix-specific paths to exec-path for GUI Emacs
;; This is crucial if you launch GUI Emacs *outside* of a direnv-managed shell
;; and still want it to find tools from your Nix profile.
;; However, for project-specific tools, it's better to rely on direnv
;; or launching Emacs from within the `nix-shell` itself.
(setq exec-path (append (split-string (getenv "PATH") ":") exec-path))
```

### `~/.doom.d/packages.el`{.verbatim}

You generally don\'t need to add many packages here if Doom Emacs
modules provide what you need. However, if there\'s a specific Emacs
package not covered by Doom modules, you can add it here.

``` elisp
;; ~/.doom.d/packages.el

;; Example: A package for Nix formatting (if not already included by doom's nix module)
;; (package! nixpkgs-fmt)
```

After modifying your Doom Emacs configuration files, remember to run:

``` bash
doom sync
```

------------------------------------------------------------------------

## Step 3: Launching Doom Emacs with Nix Shell

The most important step is ensuring Emacs inherits the environment
variables and `PATH`{.verbatim} from your Nix shell.

### Option 1: Manual `nix-shell`{.verbatim} (Simplest)

1.  Navigate to your project directory in your terminal:

    ``` bash
    cd /path/to/your/project
    ```

2.  Enter the Nix shell:

    ``` bash
    nix-shell
    ```

    You will see a message indicating you\'ve entered the shell (e.g.,
    \"Entering Nix shell...\").

3.  From **within** the Nix shell, launch Doom Emacs:

    ``` bash
    doom run # For terminal Emacs
    emacsclient -c -a "" # For GUI Emacs (if your Emacs daemon is running)
    emacs # For GUI Emacs (if no daemon)
    ```

    When you launch Emacs this way, it will inherit the
    `PATH`{.verbatim} and all other environment variables set by your
    `shell.nix`{.verbatim}.

### Option 2: Using `direnv`{.verbatim} (Recommended for Automation) \[\]

`direnv`{.verbatim} automates the process of loading and unloading Nix
shells.

1.  **Install `direnv`{.verbatim}**: Make sure `direnv`{.verbatim} is in
    your `shell.nix`{.verbatim} or installed globally.

    ``` bash
    nix-env -iA nixpkgs.direnv # Global installation
    ```

2.  **Hook `direnv`{.verbatim} into your shell**: Add the following to
    your shell\'s `rc`{.verbatim} file ( `.bashrc`{.verbatim},
    `.zshrc`{.verbatim}, etc.):

    ``` bash
    eval "$(direnv hook bash)" # For Bash
    # eval "$(direnv hook zsh)" # For Zsh
    ```

3.  **Enable Nix hook for `direnv`{.verbatim}**: Add this to your
    shell\'s `rc`{.verbatim} file as well:

    ``` bash
    eval "$(direnv hook nix)"
    ```

4.  **Allow `direnv`{.verbatim} in your project**:

    -   Navigate to your project directory:
        `cd /path/to/your/project`{.verbatim}
    -   Run: `direnv allow`{.verbatim}
    -   This will execute your `shell.nix`{.verbatim} and load the
        environment.

5.  **Launch Emacs**: Now, whenever you `cd`{.verbatim} into your
    project directory, the Nix shell environment will be automatically
    loaded. You can then launch Doom Emacs as usual from that directory,
    and it will pick up the correct `PATH`{.verbatim} and tools.

    ``` bash
    emacsclient -c -a "" # Or =doom run=
    ```

------------------------------------------------------------------------

## Verifying the Setup

Once Emacs is running within the Nix shell environment:

1.  **Check `PATH`{.verbatim}**: In Emacs, open `*scratch*`{.verbatim}
    buffer and evaluate:

    ``` elisp
    (getenv "PATH")
    ```

    You should see paths to the Nix store (`/nix/store/...`{.verbatim})
    containing the tools you specified in `shell.nix`{.verbatim}.

2.  **Test Language Servers**: Open a file of a language you\'ve
    configured (e.g., a Python file, a TypeScript file). The LSP should
    automatically start and provide completions, diagnostics, etc.

3.  **Test Formatters**: Try running a formatter (e.g.,
    `SPC c f`{.verbatim} for `+format/buffer`{.verbatim} in Doom). It
    should use the formatter provided by the Nix shell.

4.  **Open `vterm`{.verbatim}**: `SPC o t`{.verbatim} (or
    `M-x vterm`{.verbatim}). You should be in the Nix shell environment,
    and commands like `python --version`{.verbatim},
    `node --version`{.verbatim}, `pyright --version`{.verbatim} should
    reflect the versions specified in your `shell.nix`{.verbatim}.

------------------------------------------------------------------------

## Advanced Tips

# **Project-specific Emacs Config**: For very specific project needs, you can use `dir-locals.el`{.verbatim} to set variables that apply only to files within that directory. For example, to force a specific Python interpreter path if `pyright`{.verbatim} isn\'t picking it up correctly:

``` elisp
;; .dir-locals.el in your project root
((python-mode . ((lsp-pyright-executable . "/path/to/your/nix/store/pyright/bin/pyright"))))
```

However, relying on the `PATH`{.verbatim} from the Nix shell is
generally preferred.

# **Pinning `nixpkgs`{.verbatim}**: For ultimate reproducibility, pin the `nixpkgs`{.verbatim} version in your `shell.nix`{.verbatim} using a specific commit or channel. This prevents your environment from changing when `nixpkgs`{.verbatim} updates.

``` nix
# shell.nix (with pinned nixpkgs)
{ pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/YOUR_NIXPKGS_COMMIT_HASH.tar.gz") {} }:
# ... rest of your shell.nix
```

Replace `YOUR_NIXPKGS_COMMIT_HASH`{.verbatim} with a specific commit
from the `nixpkgs`{.verbatim} repository.

# **Multiple Nix Shells**: You can have different `shell.nix`{.verbatim} files in different subdirectories of your project if you have distinct environments for different components (e.g., `frontend/shell.nix`{.verbatim}, `backend/shell.nix`{.verbatim}).

This setup provides a robust and consistent development environment for
your programming projects with Doom Emacs and Nix.
