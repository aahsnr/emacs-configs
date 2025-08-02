#+TITLE: Emacs Configuration
#+AUTHOR: Ahsanur Rahman
#+STARTUP: overview
#+PROPERTY: :tangle yes :lexical yes

* Core Emacs and Package Management
** Setup User
#+begin_src emacs-lisp
(setq inhibit-startup-screen t
      user-full-name "Ahsanur Rahman"
      user-mail-address "ahsanur041@proton.me")
#+end_src

** Performance Tweaks
#+begin_src emacs-lisp
(setq gc-cons-threshold (* 100 1024 1024)   ; 100 MB GC threshold
      read-process-output-max (* 1024 1024) ; 1 MB read process output max
      native-comp-async-report-warnings-errors 'silent) ; Hide native-comp warnings
#+end_src

** Display Startup Time
#+begin_src emacs-lisp
(defun ar/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'ar/display-startup-time)
#+end_src

** Minimal UI Shell
#+begin_src emacs-lisp
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq-default frame-title-format "%b - Emacs"
              ring-bell-function 'ignore
              visible-bell nil
              cursor-in-non-selected-windows nil
              line-spacing 0.0
              column-number-mode t)
#+end_src

** Package Management
#+begin_src emacs-lisp
(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
#+end_src

** Boostrap use package
#+begin_src emacs-lisp
(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t)
#+end_src

** Quality-of-life Settings
#+begin_src emacs-lisp
(setq-default show-trailing-whitespace t)
(delete-selection-mode 1)
(electric-pair-mode 1)
(recentf-mode 1)
(savehist-mode 1)
(global-hl-line-mode 1)
(global-visual-line-mode 1)
#+end_src

** Reload Configuration
#+begin_src emacs-lisp
(defun ar/reload-config ()
  "Reload the Emacs configuration."
  (interactive)
  ;; Assuming config.org is the main configuration file and this config.el is tangled from it.
  ;; If config.el is the primary config, change to: (load-file (expand-file-name "config.el" user-emacs-directory))
  (let ((config-file (expand-file-name "config.org" user-emacs-directory)))
    (if (file-exists-p config-file)
        (progn
          (message "Reloading Emacs configuration from config.org...")
          (org-babel-load-file config-file)
          (message "Configuration reloaded successfully!"))
      (error "Configuration file %s not found" config-file))))

;; Bind ar/reload-config globally
(global-set-key (kbd "C-c r") 'ar/reload-config)
#+end_src

* General Keybindings
#+begin_src emacs-lisp
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :after evil
  :config
  (general-create-definer ar/global-leader
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (ar/global-leader
   ;; Core
   "SPC" '(execute-extended-command :wk "M-x")
   "q q" '(save-buffers-kill-terminal :wk "Quit Emacs")
   "q r" '(ar/reload-config :wk "Reload Config")))
#+end_src

* Org Mode
#+begin_src emacs-lisp
(use-package org
  :ensure nil
  :hook (org-mode . (lambda ()
                      ;; Restore TAB for folding in normal state
                      (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle))))
#+end_src

** Org Babel & Org Tempo
#+begin_src emacs-lisp
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

#+end_src

* UI & Theming
** Line Numbers 
#+begin_src emacs-lisp
(global-display-line-numbers-mode -1)
(setq-default display-line-numbers-grow-only t
              display-line-numbers-width 2)

;; Enable line numbers for some modes
(dolist (mode '(prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode t))))
#+end_src

** Fonts
#+begin_src emacs-lisp
  (defun ar/set-font-faces ()
    "Set all font faces for the current frame."
    ;; Set default, fixed-pitch, and variable-pitch faces to JetBrainsMono Nerd Font
    ;; Height 145 corresponds to 14.5pt. Adjust as needed.
    (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 145 :weight 'medium)
    (set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 145 :weight 'medium)
    (set-face-attribute 'variable-pitch nil :font "JetBrainsMono Nerd Font" :height 145 :weight 'medium)
    ;; Apply italic slant to comments and keywords for visual distinction
    (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
    (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
    (set-language-environment "UTF-8")
    (set-fontset-font t 'unicode "JetBrainsMono Nerd Font" nil 'append)
    (set-fontset-font t 'symbol "JetBrainsMono Nerd Font" nil 'append))


  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame) (with-selected-frame frame (ar/set-font-faces))))
    (ar/set-font-faces))

  (setq font-lock-maximum-decoration t)
#+end_src

** Theming
#+begin_src emacs-lisp
(use-package catppuccin-theme
  :demand t
  :config
  (setq catppuccin-flavor 'macchiato
	catppuccin-highlight-matches t
	catppuccin-italic-comments t
	catppuccin-italic-variables t)
  (load-theme 'catppuccin t))
#+end_src

** Solaire Mode
#+begin_src emacs-lisp
(use-package solaire-mode
  :init (solaire-global-mode))
#+end_src

** Nerd Icons
#+begin_src emacs-lisp
(use-package nerd-icons
  :demand t
  :custom
  (nerd-icons-font-family "JetBrainsMono Nerd Font")
  (nerd-icons-color-icons t))
#+end_src

** Doom Modeline
#+begin_src emacs-lisp
(use-package doom-modeline
  :demand t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-project-detection 'auto)
  (doom-modeline-lsp t)
  (doom-modeline-minor-modes nil))
#+end_src

** Which Key
#+begin_src emacs-lisp
(use-package which-key
  :init (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.1)
  (which-key-separator " → ")
  (which-key-popup-type 'minibuffer))
#+end_src

** Dashboard
#+begin_src emacs-lisp
(use-package dashboard
  :init (dashboard-setup-startup-hook)
  :custom
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (dashboard-center-content t)
  (dashboard-items '((recents . 5) (projects . 5) (agenda . 5)))
  (dashboard-startup-banner 'logo)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons))
#+end_src

* Evil (Vim Emulation)
** Core Evil
#+begin_src emacs-lisp
(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-vsplit-window-right t
        evil-split-window-below t)
  :config (evil-mode 1))
#+end_src

** Evil Collection
#+begin_src emacs-lisp
(use-package evil-collection
  :after evil
  :config (evil-collection-init))
#+end_src

** Evil Extensions
#+begin_src emacs-lisp
(use-package evil-surround :hook (evil-mode . global-evil-surround-mode))
(use-package evil-nerd-commenter :after evil)
(use-package evil-numbers :after evil)
(use-package evil-args :after evil)
(use-package evil-exchange :after evil :config (evil-exchange-install))
(use-package evil-indent-plus :after evil :config (evil-indent-plus-default-bindings))
(use-package evil-visualstar :hook (evil-mode . global-evil-visualstar-mode))
(use-package evil-matchit :hook (evil-mode . global-evil-matchit-mode))
(use-package evil-snipe :after evil :config (evil-snipe-mode 1) (evil-snipe-override-mode 1))
(use-package evil-lion :after evil :config (evil-lion-mode))
(use-package evil-multiedit :after evil :config (evil-multiedit-default-keybinds))
(use-package evil-goggles :hook (evil-mode . evil-goggles-mode) :custom (evil-goggles-duration 0.1))

(use-package evil-escape
  :hook (evil-mode . evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.2)
  (evil-escape-excluded-modes '(dired-mode)))
#+end_src

* TODO Completion Framework
Match the completion framework with my other configurations.
** Base Completion

#+begin_src emacs-lisp
(use-package emacs
  :ensure nil
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  (completion-cycle-threshold 3)
  (completions-detailed t)
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)
  ;; Check if this messes up
  (completion-auto-help 'always)
  (completion-auto-select 'second-tab)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; see if disabling this allows for tab for cycling corfu selection
  ;; (tab-always-indent 'complete)
  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))
#+end_src

** Orderless
#+begin_src emacs-lisp
(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
#+end_src

** Vertico
#+begin_src emacs-lisp
  (use-package vertico
    :demand t
    :init
    (vertico-mode)
    :custom
    (vertico-cycle t)
    (vertico-scroll-margin 2)
    (vertico-resize nil)
    (vertico-count 10))
#+end_src

** Marginalia
#+begin_src emacs-lisp
(use-package marginalia
  :after vertico
  :init (marginalia-mode))
#+end_src

** Corfu
#+begin_src emacs-lisp
  (use-package corfu
    :demand t
    :init (global-corfu-mode) ; Enable Corfu globally for all modes
    :custom
    (corfu-cycle t) ; Cycle through candidates when reaching end/beginning
    (corfu-auto t) ; Automatically open completion popup
    (corfu-separator ?\s)
    (corfu-auto-delay 0.1) ; Delay for auto-completion popup in seconds
    (corfu-quit-at-end t) ; Quit popup when no more candidates are available
    (corfu-popupinfo-delay 0.5) ; Delay for displaying detailed popup info (e.g., function docs)
    (corfu-preselect 'prompt)
    (corfu-on-exact-match 'insert))
#+end_src

** Nerd Icons Corfu

#+begin_src emacs-lisp
(use-package nerd-icons-corfu
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
#+end_src

** Cape

#+begin_src emacs-lisp
(use-package cape
  :demand t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))
#+end_src

** Nerd Icons Completion

#+begin_src emacs-lisp
(use-package nerd-icons-completion
  :after (vertico marginalia)
  :config
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
#+end_src

** Consult
#+begin_src emacs-lisp
  (use-package consult
    :demand t
    :config
    (defun ar/consult-buffer-predicate (cand)
      "Predicate for `consult-buffer` source.

  Filters the list of buffers presented by `consult-buffer`
  based on the context.

  - In the Emacs Dashboard, it shows file-visiting buffers plus
    `*scratch*` and `*Messages*`.
  - Elsewhere, it hides all buffers starting or ending with '*'."
      (let* ((name (buffer-name cand))
             (is-special-buffer (or (string-prefix-p "*" name)
                                    (string-suffix-p "*" name)))
             (is-dashboard-buffer (and (boundp 'dashboard-buffer-name)
                                       (string-equal (buffer-name) dashboard-buffer-name))))
        (if is-dashboard-buffer
            ;; If on dashboard, allow non-special buffers OR the whitelisted ones.
            (or (not is-special-buffer) (member name '("*scratch*" "*Messages*")))
          ;; Otherwise, only allow non-special buffers.
          (not is-special-buffer))))

    ;; 1. Create a new source by copying the default buffer source
    ;;    and appending our custom predicate.
    (defvar ar/consult-source-buffer
      (append consult--source-buffer
              '((:predicate . ar/consult-buffer-predicate)))
      "Custom buffer source with predicate filtering.")

    ;; 2. Tell `consult-buffer` to use our new source instead of the default one.
    ;;    We do this by substituting it in the `consult-buffer-sources` list.
    (setq consult-buffer-sources
          (cl-subst 'ar/consult-source-buffer  ; The new source to use
                    'consult--source-buffer   ; The default source to replace
                    consult-buffer-sources))  ; The list of sources to modify

    (setq consult-line-start-hint t)

    (consult-customize
     consult-ripgrep
     consult-grep
     consult-bookmark
     consult-recent-file
     consult--source-file-register
     consult--source-bookmark
     consult--source-recent-file
     consult--source-buffer
     consult--source-project-buffer
     :preview-key (kbd "M-.")))
#+end_src

** Embark
#+begin_src emacs-lisp
(use-package embark
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
#+end_src

** Embark Consult
#+begin_src emacs-lisp
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
#+end_src

* Buffer Management
** Core Configuration
#+begin_src emacs-lisp
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :config
  ;; Load nerd-icons-ibuffer first to define the icon format specifier
  (require 'nerd-icons-ibuffer)
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)

  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-expert t
        ibuffer-use-header-line t
        ibuffer-display-summary nil
        ibuffer-movement-cycle t
        ;; Now this format will work correctly
        ibuffer-formats
        '((mark read-only modified " "
                (icon 4 4 :left) ; Provided by nerd-icons-ibuffer
                (name 30 30 :left :elide)
                " "
                (size-h 9 9 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process))
        ibuffer-saved-filter-groups
        '(("default"
           ("Magit" (name . "^magit:"))
           ("Org"   (mode . "org-mode"))
           ("Help"  (or (mode . "help-mode") (mode . "info-mode")))
           ("Emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")))
           ;; Filter rule to hide buffers starting or ending with *
           ("Files" (and (visiting-file)
                         (not (or (string-match-p "^\\*" (buffer-name))
                                  (string-match-p "\\*$" (buffer-name)))))))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))
#+end_src

** Nerd Icons Integration

#+begin_src emacs-lisp
(use-package nerd-icons-ibuffer
  :config (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode))
#+end_src

** Keybindings

#+begin_src emacs-lisp
(ar/global-leader
  ;; Buffer management
  "b" '(:ignore t :which-key "buffers")
  "b b" '(consult-buffer :which-key "switch buffer")
  "b i" '(ibuffer :which-key "ibuffer")
  "b k" '(kill-current-buffer :which-key "kill buffer")
  "b n" '(next-buffer :which-key "next buffer")
  "b p" '(previous-buffer :which-key "previous buffer")
  "b r" '(revert-buffer :which-key "revert buffer")
  "b s" '(save-buffer :which-key "save buffer"))
#+end_src
