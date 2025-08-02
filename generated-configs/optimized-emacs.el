;;               _                    _           _
;;  ___ _ __   __| | ___  ___ ___   __| | ___  ___| | __
;; / _ \ '_ \ / _` |/ _ \/ __/ _ \ / _` |/ _ \/ __| |/ /
;;|  __/ | | | (_| |  __/ (_| (_) | (_| |  __/ (__|   <
;; \___|_| |_|\__,_|\___|\___\___/ \__,_|\___|\___|_|\_\
;; =======================================================
;;                      CONFIG.EL
;; =======================================================

;;;; Startup Performance Tuning

;; Defer garbage collection during startup
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024)) ;; 100MB
(setq read-process-output-max (* 1024 1024)) ;; 1MB
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold gc-cons-threshold-original)))

;; Advise to inhibit file handler during startup (for Emacs 28+)
(when (fboundp 'file-name-handler-alist-inhibit)
  (defun inhibit-file-name-handlers-during-startup (orig-fun &rest args)
    (let ((file-name-handler-alist-inhibit t))
      (apply orig-fun args)))
  (advice-add #'find-file-noselect :around #'inhibit-file-name-handlers-during-startup)
  (advice-add #'normal-top-level-add-subdirs-to-load-path :around #'inhibit-file-name-handlers-during-startup))

;;;; Package Management & use-package

;; Initialize package sources
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Configure use-package
(require 'use-package)
(setq use-package-always-ensure t
      use-package-compute-statistics t
      ;; Defer loading of packages by default
      use-package-always-defer t)

;; Add custom lisp directory to load-path
(add-to-list 'load-path "~/.config/emacs/lisp/")

;;;; Core Emacs Configuration
(use-package emacs
  :ensure nil
  :demand t ;; Force this block to be evaluated at startup
  :init
  ;; Native Compilation Settings (if available)
  (when (fboundp 'native-compile-prune-cache)
    (setq native-comp-async-report-warnings-errors nil
          native-comp-deferred-compilation-deny-list '("org" "transient"))
    (add-hook 'emacs-startup-hook
              (lambda ()
                (add-hook 'after-init-hook
                          (lambda ()
                            (unless (or noninteractive eglot--server-process)
                              (native-compile-prune-cache)))))))

  ;; Reload config function
  (defun reload-config ()
    "Reload the Emacs configuration from config.org."
    (interactive)
    (let ((config-file (expand-file-name "config.org" user-emacs-directory)))
      (if (file-exists-p config-file)
          (progn
            (message "Reloading configuration...")
            (org-babel-load-file config-file)
            (message "Configuration reloaded successfully!"))
        (error "Configuration file %s not found" config-file))))
  :bind (("C-c r" . reload-config))

  :custom
  ;; UI & Basic Editing
  (inhibit-startup-screen t)
  (visible-bell nil)
  (ring-bell-function 'ignore)
  (sentence-end-double-space nil)
  (fill-column 80)
  (global-visual-line-mode 1)
  (delete-by-moving-to-trash t)
  (scroll-conservatively 101)
  (scroll-margin 3)
  (setq-default line-spacing 0.02)
  (setq-default cursor-type 'box)
  (x-stretch-cursor t)
  (setq-default display-line-numbers-grow-only t
                display-line-numbers-width 2)

  ;; Completion & Minibuffer
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (completion-cycle-threshold 3)
  (completions-detailed t)
  (completion-auto-help 'always)
  (completion-auto-select 'second-tab)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Unicode and Font Handling
  (locale-coding-system 'utf-8)
  (x-select-enable-clipboard t)
  :config
  ;; Font Setup
  (defun efs/set-font-faces ()
    (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 145 :weight 'medium)
    (set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 145 :weight 'medium)
    (set-face-attribute 'variable-pitch nil :font "JetBrainsMono Nerd Font" :height 145 :weight 'medium)
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
    (set-face-attribute 'font-lock-keyword-face nil :slant 'italic))

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (efs/set-font-faces))))
    (efs/set-font-faces))

  ;; Line Numbers
  (dolist (mode '(prog-mode-hook conf-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode t))))

  ;; Auto-fill mode
  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'prog-mode-hook 'auto-fill-mode)

  ;; Unicode fallbacks
  (set-fontset-font t 'unicode "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

  ;; Prettify Symbols
  (global-prettify-symbols-mode 1)

  ;; Auto Composition for ligatures
  (global-auto-composition-mode 1))
  

;;;; THEME & UI PACKAGES

(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-miramare t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

(use-package doom-modeline
  :demand t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon (display-graphic-p))
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-project-detection 'auto)
  (doom-modeline-lsp t)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t))

(use-package solaire-mode
  :hook ((after-init . solaire-global-mode))
  :config
  ;; Integration with popups
  (add-hook 'completion-list-mode-hook #'solaire-mode)
  (add-hook 'which-key-mode-hook #'solaire-mode)
  (add-hook 'help-mode-hook #'solaire-mode)
  (add-hook 'info-mode-hook #'solaire-mode)
  (add-hook 'org-src-mode-hook #'solaire-mode)
  (advice-add 'vertico--display-candidates :after
              (lambda (&rest _)
                (when (minibufferp)
                  (with-selected-window (minibuffer-window)
                    (solaire-mode +1))))))

(use-package which-key
  :init (which-key-mode 1)
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.1)
  (which-key-popup-type 'minibuffer)
  (which-key-sort-order 'which-key-key-order-alpha)
  :config
  (custom-set-faces
   '(which-key-key-face ((t (:foreground "#83a598" :weight bold))))
   '(which-key-description-face ((t (:foreground "#ebdbb2"))))
   '(which-key-group-description-face ((t (:foreground "#d3869b"))))
   '(which-key-command-description-face ((t (:foreground "#b8bb26"))))
   '(which-key-local-map-description-face ((t (:foreground "#8ec07c"))))
   '(which-key-separator-face ((t (:foreground "#7c6f64"))))))

(use-package nerd-icons
  :if (display-graphic-p)
  :custom
  (nerd-icons-font-family "JetBrainsMono Nerd Font")
  (nerd-icons-color-icons t))

;;;; EVIL (Vim Emulation)

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1)
  (setq evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo
        evil-search-module 'evil-search
        evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-want-fine-undo t)

  ;; Gruvbox cursor colors
  (setq evil-normal-state-cursor '(box "#fe8019")
        evil-insert-state-cursor '(bar "#fb4934")
        evil-visual-state-cursor '(hollow "#fe8019")
        evil-replace-state-cursor '(hbar "#fb4934")
        evil-motion-state-cursor '(box "#b8bb26"))

  ;; Set initial states for various modes
  (dolist (mode '(messages-buffer-mode dashboard-mode compilation-mode
                  grep-mode occur-mode help-mode Info-mode woman-mode
                  man-mode package-menu-mode))
    (evil-set-initial-state mode 'normal))

  (dolist (mode '(term-mode shell-mode eshell-mode))
    (evil-set-initial-state mode 'emacs))

  ;; Custom functions
  (defun my/evil-scroll-down-center () "Scroll down and center cursor." (interactive) (evil-scroll-down nil) (evil-scroll-line-to-center nil))
  (defun my/evil-scroll-up-center () "Scroll up and center cursor." (interactive) (evil-scroll-up nil) (evil-scroll-line-to-center nil)))

(use-package evil-collection
  :after evil
  :init (evil-collection-init))

(use-package evil-surround :hook (evil-mode . global-evil-surround-mode))
(use-package evil-nerd-commenter :after evil)
(use-package evil-numbers :after evil)
(use-package evil-args :after evil)
(use-package evil-anzu :after evil)
(use-package evil-exchange :after evil :config (evil-exchange-install))
(use-package evil-indent-plus :after evil :config (evil-indent-plus-default-bindings))
(use-package evil-visualstar :hook (evil-mode . global-evil-visualstar-mode))
(use-package evil-matchit :hook (evil-mode . global-evil-matchit-mode))
(use-package evil-snipe :after evil :config (evil-snipe-mode +1) (evil-snipe-override-mode +1) (setq evil-snipe-smart-case t))
(use-package evil-lion :hook (evil-mode . evil-lion-mode))
(use-package evil-multiedit :after evil :config (evil-multiedit-default-keybinds))
(use-package evil-goggles :hook (evil-mode . evil-goggles-mode) :custom (evil-goggles-duration 0.1))

(use-package evil-escape
  :hook (evil-mode . evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.2)
  (evil-escape-excluded-modes '(dired-mode)))

;;;; COMPLETION FRAMEWORK (Vertico, Corfu, etc.)

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  (custom-set-faces
   '(orderless-match-face-0 ((t (:foreground "#d3869b" :weight bold))))
   '(orderless-match-face-1 ((t (:foreground "#83a598" :weight bold))))
   '(orderless-match-face-2 ((t (:foreground "#b8bb26" :weight bold))))
   '(orderless-match-face-3 ((t (:foreground "#fabd2f" :weight bold))))))

(use-package vertico
  :init (vertico-mode)
  :diminish vertico-mode
  :custom
  (vertico-cycle t)
  (vertico-count 10)
  :config
  (custom-set-faces
   '(vertico-current ((t (:background "#504945" :foreground "#ebdbb2" :weight bold))))
   '(vertico-group-title ((t (:foreground "#d3869b" :weight bold))))))

(use-package corfu
  :hook (global . global-corfu-mode)
  :diminish global-corfu-mode
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-scroll-margin 5)
  (corfu-preview-current nil) ; Use corfu-popupinfo for previews
  :config
  ;; Use corfu-popupinfo for documentation
  (corfu-popupinfo-mode)
  (custom-set-faces
   '(corfu-default ((t (:background "#282828" :foreground "#ebdbb2"))))
   '(corfu-current ((t (:background "#504945" :foreground "#ebdbb2" :weight bold))))
   '(corfu-bar ((t (:background "#b16286"))))
   '(corfu-border ((t (:background "#7c6f64"))))
   '(corfu-popupinfo ((t (:background "#282828" :foreground "#ebdbb2"))))
   '(corfu-popupinfo-documentation ((t (:foreground "#a89984" :italic t))))))

(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :commands (cape-dabbrev cape-file cape-elisp-symbol cape-keyword)
  :init
  ;; Add useful defaults to completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :config
  ;; Silence the pcomplete capf
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent))

(use-package marginalia
  :after vertico
  :init (marginalia-mode)
  :diminish marginalia-mode)

(use-package nerd-icons-completion
  :after (nerd-icons marginalia)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package consult
  :after vertico
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu))
  :custom
  (consult-narrow-key "<")
  ;; Use fd and ripgrep if available
  (consult-find-args "fd --hidden --strip-cwd --type f")
  (consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --smart-case --no-heading --line-number --hidden --glob '!.git/'")
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package embark
  :after (vertico consult)
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect.*\\*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;; FILE MANAGEMENT (Dired, Dirvish)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :hook ((dired-mode . auto-revert-mode)
         (dired-mode . diredfl-mode))
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-vc-rename-file t)
  (delete-by-moving-to-trash t)
  (dired-omit-files "^\\.[^.]\\|^#\\|^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.o$")
  (dired-omit-verbose nil)
  :config
  (custom-set-faces
   '(dired-directory ((t (:foreground "#83a598" :weight bold))))
   '(dired-marked ((t (:foreground "#fb4934" :weight bold))))
   '(dired-flagged ((t (:foreground "#fb4934" :background "#3c3836"))))
   '(diredfl-dir-name ((t (:inherit dired-directory :bold t))))))

(use-package dired-x :ensure nil :after dired)

(use-package dirvish
  :after dired
  :init (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads/" "Downloads")
     ("D" "~/Documents/" "Documents")
     ("p" "~/Projects/" "Projects")
     ("/" "/" "Root")))
  (dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  :config
  (add-hook 'dirvish-directory-view-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package dired-subtree :after dired)
(use-package dired-narrow :after dired)
(use-package dired-ranger :after dired)
(use-package dired-collapse :hook (dired-mode . dired-collapse-mode))
(use-package nerd-icons-dired :hook (dired-mode . nerd-icons-dired-mode))


;;;; PROJECT & WORKSPACE MANAGEMENT

(use-package project
  :ensure nil
  :after (consult persp)
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (consult-ripgrep "Ripgrep")
     (project-dired "Dired")
     (project-shell "Shell")
     (project-eshell "Eshell")
     (project-switch-to-buffer "Switch buffer")
     (project-kill-buffers "Kill project buffers"))))

(use-package perspective
  :init (persp-mode)
  :diminish persp-mode
  :custom
  (persp-mode-prefix-key (kbd "C-c p"))
  (persp-state-default-file (expand-file-name "perspective-session" user-emacs-directory))
  (persp-autokill-buffer-on-remove 'kill-if-not-modified)
  :config
  (add-hook 'kill-emacs-hook #'persp-state-save))

;;;; DEVELOPMENT (Treesitter, Eglot, DAP)

(use-package treesit-auto
  :if (treesit-available-p)
  :hook (prog-mode . global-treesit-auto-mode)
  :custom (treesit-auto-langs '(bash c cpp css json python rust toml yaml)))

(use-package eglot
  :ensure nil
  :hook ((c-ts-mode c++-ts-mode python-ts-mode bash-ts-mode json-ts-mode yaml-ts-mode) . eglot-ensure)
  :custom
  (eglot-events-buffer-size 0)
  (eglot-connect-timeout 20)
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode) . ("clangd" "--background-index")))
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyright-langserver" "--stdio"))))

(use-package eglot-booster
  :after eglot
  :config (eglot-booster-mode))

(use-package flymake
  :ensure nil
  :diminish flymake-mode
  :hook (eglot-managed-mode . flymake-mode)
  :bind (("C-c f n" . flymake-goto-next-error)
         ("C-c f p" . flymake-goto-prev-error))
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-suppress-zero-counters t)
  :config
  (custom-set-faces
   '(flymake-error ((t (:underline (:color "#fb4934" :style wave)))))
   '(flymake-warning ((t (:underline (:color "#fabd2f" :style wave)))))
   '(flymake-note ((t (:underline (:color "#83a598" :style wave)))))))

(use-package apheleia
  :hook (prog-mode . apheleia-mode)
  :diminish apheleia-mode
  :custom (apheleia-mode-lighter " Φ")
  :config
  (setf (alist-get 'black apheleia-formatters)
        '("black" "--quiet" "-")
        (alist-get 'isort apheleia-formatters)
        '("isort" "--quiet" "-")
        (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))

  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(black isort))
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'web-mode apheleia-mode-alist) 'prettier))

(use-package dape
  :commands dape
  :custom (dape-key-prefix "\C-c d")
  :config
  (add-to-list 'dape-configs
               `(debugpy modes (python-ts-mode)
                         command "python"
                         command-args ("-m" "debugpy.adapter")
                         :request "launch" :type "executable"
                         :cwd dape-cwd :program dape-find-file-buffer-default
                         :console "integratedTerminal"))
  (add-to-list 'dape-configs
               `(gdb modes (c-ts-mode c++-ts-mode)
                     command "gdb" command-args ("--interpreter=dap")
                     :request "launch" :type "gdb"
                     :cwd dape-cwd :program (dape-read-file "Program to debug: " dape-cwd))))

;;;; ORG MODE

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . auto-fill-mode))
  :custom
  (org-directory "~/org/")
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-agenda-files (list org-directory))
  (org-archive-location (concat (expand-file-name "archive/" org-directory) "%s_archive::"))
  (org-startup-folded 'overview)
  (org-ellipsis " ⤵")
  (org-hide-emphasis-markers t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-confirm-babel-evaluate nil)
  (org-todo-keywords
   '((sequence "☛ TODO(t)" "⚡ NEXT(n)" "🔄 PROG(p)" "⏳ WAIT(w@/!)" "|" "✅ DONE(d!)" "❌ CANCELLED(c@)")))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (shell . t) (python . t)))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(use-package org-roam
  :after org
  :init
  (setq org-roam-directory (expand-file-name "roam/" org-directory)
        org-roam-db-location (expand-file-name "org-roam.db" org-directory))
  :config
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-node-display-template "${title:*} ${tags:20} ${file:15}"))

(use-package org-roam-ui
  :after org-roam
  :commands (org-roam-ui-mode org-roam-ui-open)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

;;;; GENERAL KEYBINDINGS

(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-create-definer global-leader-key
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (global-leader-key
    "SPC" '(execute-extended-command :wk "M-x")
    ;; Buffers
    "b b" '(consult-buffer :wk "Switch buffer")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b s" '(save-buffer :wk "Save buffer")
    ;; Files
    "f f" '(find-file :wk "Find file")
    "f s" '(save-buffer :wk "Save file")
    "f r" '(consult-recent-file :wk "Recent files")
    ;; Dired/Dirvish
    "d d" '(dired :wk "Dired")
    "d s" '(dirvish-side :wk "Dirvish Side Panel")
    ;; Project
    "p p" '(project-switch-project :wk "Switch Project")
    "p f" '(project-find-file :wk "Find File in Project")
    "p s" '(consult-ripgrep :wk "Search in Project")
    ;; Windows
    "w h"  '(evil-window-left :wk "window left")
    "w j"  '(evil-window-down :wk "window down")
    "w k"  '(evil-window-up :wk "window up")
    "w l"  '(evil-window-right :wk "window right")
    "w s"  '(evil-window-split :wk "split below")
    "w v"  '(evil-window-vsplit :wk "split right")
    "w d"  '(evil-window-delete :wk "delete window")
    "w o"  '(delete-other-windows :wk "delete other windows")
    "w ="  '(balance-windows :wk "balance windows")
    "w x"  '(evil-window-exchange :wk "exchange windows")
    ;; Emacs
    "q q"  '(save-buffers-kill-terminal :wk "Quit Emacs")
    "q r"  '(reload-config :wk "Reload Config"))

  ;; Evil specific keybindings
  (general-define-key
   :states 'normal
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line
   "gj" 'evil-next-line
   "gk" 'evil-previous-line
   "C-d" 'my/evil-scroll-down-center
   "C-u" 'my/evil-scroll-up-center)

  ;; Corfu keybindings
  (general-define-key
   :keymaps 'corfu-map
   "TAB" 'corfu-next
   "S-TAB" 'corfu-previous
   [return] 'corfu-insert
   "C-g" 'corfu-quit))

;;;; Final Setup
(use-package dashboard
  :init (dashboard-setup-startup-hook)
  :custom
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (dashboard-center-content t)
  (dashboard-items '((recents . 5) (projects . 5) (agenda . 5)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-startup-banner 'logo))

;; All done!
(message "Emacs configuration loaded successfully!")
