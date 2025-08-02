```sh
* Development Environment
This section configures Emacs to be a modern Integrated Development Environment. It includes the Language Server Protocol (LSP) for code intelligence, a Debug Adapter Protocol (DAP) client for debugging, advanced syntax checking, auto-formatting, and Tree-sitter for superior syntax highlighting and structural code editing.

** LSP (Language Server Protocol)
This setup uses `lsp-mode` as the core client and `lsp-ui` for the user interface, with a focus on performance and deep integration with the completion framework.

*** Core LSP Setup with Performance Booster
#+begin_src emacs-lisp
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((prog-mode . lsp-deferred)
         ;; Enable LSP in org-mode source blocks for live feedback.
         (org-src-mode . lsp-deferred))
  :init
  ;; Performance Settings.
  (setq lsp-idle-delay 0.500
        lsp-log-io nil
        lsp-enable-snippet nil ; Rely on the main completion framework (Corfu).
        lsp-enable-file-watchers nil ; Resource-intensive; global-auto-revert-mode is sufficient.
        lsp-enable-semantic-tokens nil) ; Use Tree-sitter for semantic highlighting.

  :custom
  (lsp-headerline-breadcrumb-enable t)
  (lsp-eldoc-render-all nil) ; Keep eldoc clean.
  (lsp-signature-render-documentation nil) ; Avoid overly large signature popups.
  ;; CRUCIAL for Corfu/Cape integration: This tells lsp-mode to provide completions
  ;; via the standard `completion-at-point-functions` (CAPF), which Corfu uses.
  (lsp-completion-provider :capf)

  :config
  ;; INSTRUCTIONS for lsp-booster:
  ;; 1. Install the binary: `cargo install emacs-lsp-booster`
  ;; 2. Ensure the binary is in your system's PATH.
  (defun lsp-booster--advice (old-fn &rest args)
    "Advice to boost LSP server communication."
    (let ((proc (apply old-fn args)))
      (when (and proc (derived-mode 'lsp-mode))
        (let* ((command (process-command proc))
               (booster-command (cons "emacs-lsp-booster" command)))
          (when (executable-find (car booster-command))
            (message "LSP Booster: Boosting %s" command)
            (let ((new-proc (apply #'start-process (process-name proc) (process-buffer proc) booster-command)))
              (set-process-sentinel proc (lambda (p e) nil))
              (kill-process proc)
              (setq proc new-proc)))))
      proc))
  (advice-add 'lsp--open-socket :around #'lsp-booster--advice)

  ;; Remap standard keys to their LSP counterparts for ergonomic navigation.
  (define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)
  (define-key lsp-mode-map (kbd "M-,") #'lsp-find-references))
#end_src

*** LSP User Interface (with Manual Nerd Icons)
This provides the sidebars, documentation popups, and code peeking functionality. Icons are configured manually.

#+begin_src emacs-lisp
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-peek-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 2)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-peek-always-show t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-flycheck-list-position 'right))
#+end_src

*** LSP Integrations
#+begin_src emacs-lisp
;; Treemacs integration for LSP symbols.
(use-package lsp-treemacs
  :after lsp-mode
  :commands lsp-treemacs-errors-list)

;; Consult integration for LSP.
(use-package consult-lsp
  :after (consult lsp-mode))
#end_src

*** LSP Keybindings
#+begin_src emacs-lisp
(ar/global-leader
  "l" '(:ignore t :which-key "lsp")
  "l a" '(lsp-execute-code-action :wk "code action")
  "l d" '(lsp-find-definition :wk "go to definition")
  "l D" '(lsp-find-declaration :wk "go to declaration")
  "l i" '(lsp-find-implementation :wk "go to implementation")
  "l r" '(lsp-find-references :wk "find references")
  "l R" '(lsp-rename :wk "rename")
  "l s" '(lsp-treemacs-symbols :wk "workspace symbols")
  "l e" '(lsp-treemacs-errors-list :wk "workspace errors")
  "l f" '(lsp-format-buffer :wk "format buffer")
  "l h" '(:ignore t :which-key "help")
  "l h h" '(lsp-describe-thing-at-point :wk "describe at point")
  "l h s" '(lsp-signature-help :wk "signature help"))
#end_src

** Debugging (DAP - Debug Adapter Protocol)
This section configures `dap-mode` for a VSCode-like debugging experience within Emacs. It is loaded on-demand when you start a debug session.

*** Core DAP Setup
#+begin_src emacs-lisp
(use-package dap-mode
  :after lsp-mode
  :commands (dap-mode dap-debug dap-debug-recent)
  :custom
  ;; Automatically configure features for a full UI experience.
  (dap-auto-configure-features '(sessions locals controls expressions repl tooltip))
  (dap-ui-mode t)
  (dap-ui-controls-enable t)
  (dap-ui-tool-tip-mode t)
  (dap-tooltip-mode t)
  (dap-auto-show-output t)

  :config
  ;; Also enable dap-mode in org-mode for debugging code blocks.
  (add-hook 'org-src-mode-hook #'dap-mode)

  ;; Set a consistent layout for the debug windows.
  (dap-ui-configure-layout
   :elements
   '((:id "controls" :size 2)
     ("repl" "locals")
     ("expressions" "sessions" "breakpoints"))
   :layouts
   '((:width 40 :position right
            :elements ("controls" "repl"))
     (:width 40 :position bottom
             :elements ("locals" "expressions" "sessions" "breakpoints")))
   :autofocus
   '("repl")))

#+end_src

*** DAP Keybindings
#+begin_src emacs-lisp
(ar/global-leader
  "d" '(:ignore t :which-key "debug")
  "d b" '(dap-toggle-breakpoint :wk "breakpoint")
  "d c" '(dap-continue :wk "continue")
  "d n" '(dap-next :wk "next")
  "d i" '(dap-step-in :wk "step in")
  "d o" '(dap-step-out :wk "step out")
  "d q" '(dap-disconnect :wk "quit")
  "d r" '(dap-debug-recent :wk "debug recent")
  "d e" '(dap-debug :wk "debug new"))
#end_src

** Syntax Checking & Formatting
This section contains the generic setup for syntax checking and formatting.

*** Flycheck
#+begin_src emacs-lisp
(use-package flycheck
  :init (global-flycheck-mode)
  :custom
  ;; Check syntax on newline and after a short idle period.
  (flycheck-check-syntax-automatically '(newline idle-change))
  (flycheck-idle-change-delay 0.4)
  :config
  (flycheck-multiple-checkers-enable))

;; Use a popup frame for displaying Flycheck errors, which is less intrusive.
(use-package flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode))

#end_src

*** Apheleia (Auto-Formatting)
#+begin_src emacs-lisp
(use-package apheleia
  :init (apheleia-global-mode +1)
  :config
  ;; Enable formatting on save. Apheleia is smart enough not to interfere
  ;; if the buffer is already clean or being handled by another process like LSP.
  (add-hook 'before-save-hook #'apheleia-format-buffer))
#+end_src

** Tree-sitter
This leverages the modern, built-in Tree-sitter functionality for faster and more accurate syntax highlighting, code folding, and powerful new text objects for Evil.

*** Core Tree-sitter
#+begin_src emacs-lisp
(use-package treesit-auto
  :when (treesit-available-p)
  :config
  ;; Configure which languages to auto-install grammars for.
  (setq treesit-auto-langs '(bash c cpp css python toml yaml))

  ;; Install and configure tree-sitter modes automatically.
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold
  :hook (treesit-auto-mode-hook . treesit-fold-mode))
#end_src

*** Evil Text Objects via Tree-sitter
#+begin_src emacs-lisp
(use-package evil-textobj-tree-sitter
  :after evil
  :config
  (evil-textobj-tree-sitter-setup)
  ;; Define a comprehensive set of text objects for common structures.
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "comment.inner"))
  (define-key evil-outer-text-objects-map "C" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
  (define-key evil-inner-text-objects-map "p" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
  (define-key evil-outer-text-objects-map "p" (evil-textobj-tree-sitter-get-textobj "parameter.outer")))
#end_src

* Python Development
This section contains a complete, integrated setup for Python development.

** Language Server (lsp-pyright)
We use `pyright` for its performance and comprehensive features.

#+begin_src emacs-lisp
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))
  :custom
  ;; If you use a virtualenv, lsp-pyright should auto-detect it.
  ;; For projects with heavy type-checking, you can defer full analysis
  ;; from "onType" to "onSave" for better performance.
  (lsp-pyright-analysis-diagnostic-mode "onSave")

  ;; Disable reporting of certain diagnostics if they conflict with your linter.
  (lsp-pyright-disable-diagnostics '("reportUnusedImport")))
#end_src

** Linting (LSP + Pylint)
This setup allows both `lsp-pyright` and `pylint` to report diagnostics simultaneously for the most comprehensive feedback.

#+begin_src emacs-lisp
(with-eval-after-load 'flycheck
  (defun ar/python-flycheck-setup ()
    "Set up multiple checkers for Python mode."
    (setq-local flycheck-checkers '(lsp python-pylint)))
  (add-hook 'python-mode-hook #'ar/python-flycheck-setup))
#end_src

** Debugging (dap-python)
This provides an out-of-the-box debugging experience for Python.

#+begin_src emacs-lisp
;; This functionality is part of dap-mode itself.
;; Ensure `debugpy` is installed in your Python environment (`pip install debugpy`).
(with-eval-after-load 'dap-mode
  (require 'dap-python))
#end_src
```
