#+TITLE: Emacs Configuration
#+AUTHOR: Ahsanur Rahman
#+STARTUP: overview
#+PROPERTY: :tangle yes :lexical yes

* Emacs Initialization
** Built-in Package Management
#+begin_src emacs-lisp
;; Initialize package sources
(require 'package)

;; Add package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


(require 'use-package)
(setq use-package-always-ensure t
      use-package-compute-statistics t)
#+end_src

** Add to Path
#+begin_src emacs-lisp
(add-to-list 'load-path "~/.config/emacs/lisp/")
#+end_src

** Reload Emacs Configuration
#+begin_src emacs-lisp
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

;; Bind to a convenient key combination
(global-set-key (kbd "C-c r") 'reload-config)
#+end_src

** Load Files
#+begin_src emacs-lisp
(require 'org-md-convert)
(require 'src-block-nav)
#+end_src

* TODO Dired
** Core Setup

#+begin_src emacs-lisp
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-hide-details-hide-symlink-targets nil)
  (dired-hide-details-hide-information-lines nil)
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t)
  (dired-do-revert-buffer t)
  (delete-by-moving-to-trash t)
  :config
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil)
  ;; Gruvbox-themed dired faces with direct hex codes
  (custom-set-faces
   '(dired-directory ((t (:foreground "#83a598" :weight bold))))
   '(dired-header ((t (:foreground "#fabd2f" :weight bold))))
   '(dired-symlink ((t (:foreground "#8ec07c"))))
   '(dired-marked ((t (:foreground "#fb4934" :weight bold))))
   '(dired-flagged ((t (:foreground "#fb4934" :background "#3c3836"))))
   '(dired-warning ((t (:foreground "#fe8019" :weight bold))))
   '(dired-perm-write ((t (:foreground "#b8bb26"))))
   '(dired-special ((t (:foreground "#d3869b"))))
   '(dired-ignored ((t (:foreground "#928374"))))))
#+end_src

** Dired-x for additional functionality
#+begin_src emacs-lisp
(use-package dired-x
  :ensure nil
  :after dired
  :custom
  (dired-x-hands-off-my-keys nil)
  :config
  ;; Define dired-omit-files to prevent void-variable errors
  (setq dired-omit-files "^\\.[^.]\\|^#\\|^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.o$")
  (setq dired-omit-verbose nil))
#+end_src

** TODO Dirvish - Modern dired interface
=Icons not working in dirvish mode=
#+begin_src emacs-lisp
(use-package dirvish
  :after (dired dired-x nerd-icons-dired)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads/" "Downloads")
     ("D" "~/Documents/" "Documents")
     ("p" "~/Projects/" "Projects")
     ("/" "/" "Root")))
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes
   '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (dirvish-subtree-state-style 'nerd)
  (dirvish-path-separators (list "  " "  " "  "))
  (dirvish-header-line-format
   '(:left (path) :right (free-space)))
  (dirvish-use-header-line t)
  (dirvish-use-mode-line t)
  :config
  (add-hook 'dirvish-directory-view-mode-hook (lambda () (nlinum-mode 0)))
  ;; Gruvbox-themed dirvish faces with direct hex codes
  (custom-set-faces
   '(dirvish-hl-line ((t (:background "#3c3836"))))
   '(dirvish-emerge-group-title ((t (:foreground "#fabd2f" :weight bold))))
   '(dirvish-emerge-group-separator ((t (:foreground "#928374"))))
   '(dirvish-git-commit-message ((t (:foreground "#bdae93"))))
   '(dirvish-git-commit-author ((t (:foreground "#83a598"))))
   '(dirvish-subtree-guide ((t (:foreground "#928374"))))
   '(dirvish-path-separator ((t (:foreground "#928374"))))
   '(dirvish-free-space ((t (:foreground "#8ec07c"))))
   '(dirvish-yank-line ((t (:background "#504945"))))
   '(dirvish-index-number ((t (:foreground "#fe8019"))))
   '(dirvish-sort-indicator ((t (:foreground "#b8bb26"))))
   '(dirvish-file-size ((t (:foreground "#a89984"))))
   '(dirvish-file-time ((t (:foreground "#a89984")))))

  ;; Initialize dirvish
  (dirvish-override-dired-mode))
  #+end_src

** Dired-subtree for collapsible directory trees

#+begin_src emacs-lisp
(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  (dired-subtree-line-prefix "  ")
  :config
  ;; Gruvbox-themed dired-subtree
  (custom-set-faces
   '(dired-subtree-depth-1-face ((t (:background "#282828"))))
   '(dired-subtree-depth-2-face ((t (:background "#3c3836"))))
   '(dired-subtree-depth-3-face ((t (:background "#282828"))))
   '(dired-subtree-depth-4-face ((t (:background "#3c3836"))))
   '(dired-subtree-depth-5-face ((t (:background "#282828"))))
   '(dired-subtree-depth-6-face ((t (:background "#3c3836"))))))
#+end_src

** Dired-narrow for filtering

#+begin_src emacs-lisp
(use-package dired-narrow
  :after dired
  :config
  ;; Gruvbox-themed dired-narrow
  (custom-set-faces
   '(dired-narrow-blink ((t (:foreground "#fabd2f" :background "#504945"))))
   '(dired-narrow-rejected ((t (:foreground "#928374" :strike-through t))))
   '(dired-narrow-match ((t (:foreground "#b8bb26" :weight bold))))))
#+end_src

** Dired-ranger for copy/move operations

#+begin_src emacs-lisp
(use-package dired-ranger :after dired)
#+end_src

** Dired-collapse for collapsing single-child directories

#+begin_src emacs-lisp
(use-package dired-collapse
  :after dired
  :hook (dired-mode . dired-collapse-mode)
  :config
  ;; Gruvbox-themed dired-collapse faces with direct hex codes
  (custom-set-faces
   '(dired-collapse-face ((t (:foreground "#83a598" :weight normal))))))
#+end_src

** Additional syntax highlighting for dired

#+begin_src emacs-lisp
(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))
#+end_src

** Misc

#+begin_src emacs-lisp
;; Auto-refresh dired buffers
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Ensure dired-omit-mode is available after dired-x loads
(with-eval-after-load 'dired-x
  (add-hook 'dired-mode-hook 'dired-omit-mode))

(setq delete-by-moving-to-trash t)
#+end_src

* Evil
** Core Setup
#+begin_src emacs-lisp
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-want-Y-yank-to-eol t)
  (evil-mode 1)
  :config
  ;; Evil settings
  (setq evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo
        evil-search-module 'evil-search
        evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-shift-round nil
        evil-want-C-d-scroll t
        evil-want-fine-undo t
        evil-kill-on-visual-paste nil
        evil-move-cursor-back nil
        evil-want-minibuffer-navigation t
        evil-ex-visual-char-range t
        evil-shift-width 2
        evil-symbol-word-search t
        evil-cross-lines t
        evil-auto-indent t
        evil-ex-substitute-global t)

  ;; Enhanced cursor appearance
  (setq evil-normal-state-cursor '(box "#fe8019")
        evil-insert-state-cursor '(bar "#fb4934")
        evil-visual-state-cursor '(hollow "#fe8019")
        evil-replace-state-cursor '(hbar "#fb4934")
        evil-operator-state-cursor '(evil-half-cursor "#fb4934")
        evil-motion-state-cursor '(box "#b8bb26")
        evil-emacs-state-cursor '(hbar "#d3869b"))

  ;; Set initial states for various modes
  (dolist (mode '(messages-buffer-mode dashboard-mode compilation-mode
                  grep-mode occur-mode help-mode Info-mode woman-mode
                  man-mode package-menu-mode))
    (evil-set-initial-state mode 'normal))

  (dolist (mode '(term-mode shell-mode eshell-mode))
    (evil-set-initial-state mode 'emacs))

  ;; Custom functions
  (defun my/save-and-kill-buffer ()
    "Save and kill current buffer."
    (interactive)
    (save-buffer)
    (kill-current-buffer))

  (defun my/evil-scroll-down-center ()
    "Scroll down and center cursor."
    (interactive)
    (evil-scroll-down nil)
    (evil-scroll-line-to-center nil))

  (defun my/evil-scroll-up-center ()
    "Scroll up and center cursor."
    (interactive)
    (evil-scroll-up nil)
    (evil-scroll-line-to-center nil)))
#+end_src

** Evil Collection
#+begin_src emacs-lisp
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
#+end_src

** Evil Extensions
#+begin_src emacs-lisp
;; Evil Surround - Surround text objects
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter :after evil)
(use-package evil-numbers :after evil)
(use-package evil-args :after evil)
(use-package evil-anzu :after evil)

;; Evil Exchange - Exchange text regions
(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))

;; Evil Indent Plus - Indent text objects
(use-package evil-indent-plus
  :after evil
  :config
  (evil-indent-plus-default-bindings))

;; Evil Visualstar - Search for selected text
(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode))

;; Evil Matchit - Jump between matching tags/parentheses
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; Evil Snipe - Enhanced f/t motions
(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  (setq evil-snipe-smart-case t))

;; Evil Lion - Align text
(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))

;; Evil Multiedit - Multiple cursors for Evil
(use-package evil-multiedit
  :after evil
  :config
  (evil-multiedit-default-keybinds))

;; Evil Goggles - Visual feedback for Evil operations
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.1))

(use-package evil-escape
  :init
  (evil-escape-mode 1)
  :config
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.2
        evil-escape-excluded-modes '(dired-mode)
        evil-escape-excluded-states '()))
#+end_src

* TODO Completion System
** Core Completion Framework
*** Emacs Base Completion 
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

*** Vertico - Vertical completion interface

#+begin_src emacs-lisp
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-scroll-margin 2)
  (vertico-resize nil)
  (vertico-count 10)
  :config
  ;; Gruvbox theme integration for vertico
  (custom-set-faces
   '(vertico-current ((t (:background "#504945" :foreground "#ebdbb2" :weight bold))))
   '(vertico-group-title ((t (:foreground "#d3869b" :weight bold))))
   '(vertico-group-separator ((t (:foreground "#7c6f64"))))
   '(vertico-multiline ((t (:foreground "#83a598"))))))
#+end_src

*** Corfu - In-buffer completion
#+begin_src emacs-lisp
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.6)
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 5)
  :init
  (global-corfu-mode)
  :config
  ;; Gruvbox theme integration for corfu
  (custom-set-faces
   '(corfu-default ((t (:background "#282828" :foreground "#ebdbb2"))))
   '(corfu-current ((t (:background "#504945" :foreground "#ebdbb2" :weight bold))))
   '(corfu-bar ((t (:background "#b16286"))))
   '(corfu-border ((t (:background "#7c6f64"))))
   '(corfu-annotations ((t (:foreground "#a89984" :italic t))))
   '(corfu-deprecated ((t (:foreground "#7c6f64" :strike-through t))))))
#+end_src

*** Corfu Extensions
#+begin_src emacs-lisp
(use-package corfu-history
  :ensure nil
  :after (corfu savehist)
  :init (corfu-history-mode 1)
  :config (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :init (corfu-popupinfo-mode 1)
  :custom (corfu-popupinfo-delay '(0.5 . 0.2))
  :config
  (custom-set-faces
   '(corfu-popupinfo ((t (:background "#282828" :foreground "#ebdbb2"))))
   '(corfu-popupinfo-documentation ((t (:foreground "#a89984" :italic t))))))
#+end_src

** Annotations, Filtering, and Search
*** Marginalia - Rich annotations in minibuffer

#+begin_src emacs-lisp
(use-package marginalia
  :after vertico
  :init (marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :config
  (add-to-list 'marginalia-command-categories
               '(project-switch-project . project))
  (custom-set-faces
   '(marginalia-archive ((t (:foreground "#b8bb26"))))
   '(marginalia-char ((t (:foreground "#fe8019"))))
   '(marginalia-date ((t (:foreground "#83a598"))))
   '(marginalia-documentation ((t (:foreground "#a89984" :italic t))))
   '(marginalia-file-name ((t (:foreground "#ebdbb2"))))
   '(marginalia-file-owner ((t (:foreground "#d3869b"))))
   '(marginalia-file-priv-dir ((t (:foreground "#83a598"))))
   '(marginalia-file-priv-exec ((t (:foreground "#b8bb26"))))
   '(marginalia-file-priv-link ((t (:foreground "#8ec07c"))))
   '(marginalia-file-priv-read ((t (:foreground "#fe8019"))))
   '(marginalia-file-priv-write ((t (:foreground "#fb4934"))))
   '(marginalia-function ((t (:foreground "#83a598"))))
   '(marginalia-key ((t (:foreground "#fe8019"))))
   '(marginalia-lighter ((t (:foreground "#7c6f64"))))
   '(marginalia-list ((t (:foreground "#8ec07c"))))
   '(marginalia-mode ((t (:foreground "#d3869b"))))
   '(marginalia-modified ((t (:foreground "#fabd2f"))))
   '(marginalia-null ((t (:foreground "#7c6f64"))))
   '(marginalia-number ((t (:foreground "#fe8019"))))
   '(marginalia-size ((t (:foreground "#b8bb26"))))
   '(marginalia-string ((t (:foreground "#b8bb26"))))
   '(marginalia-symbol ((t (:foreground "#d3869b"))))
   '(marginalia-true ((t (:foreground "#b8bb26"))))
   '(marginalia-type ((t (:foreground "#83a598"))))
   '(marginalia-value ((t (:foreground "#ebdbb2"))))
   '(marginalia-variable ((t (:foreground "#8ec07c"))))
   '(marginalia-version ((t (:foreground "#b8bb26"))))))
#+end_src

*** Orderless - Fuzzy matching completion style

#+begin_src emacs-lisp
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  (custom-set-faces
   '(orderless-match-face-0 ((t (:foreground "#d3869b" :weight bold))))
   '(orderless-match-face-1 ((t (:foreground "#83a598" :weight bold))))
   '(orderless-match-face-2 ((t (:foreground "#b8bb26" :weight bold))))
   '(orderless-match-face-3 ((t (:foreground "#fabd2f" :weight bold))))))
#+end_src

*** Corfu/Orderless Integration
#+begin_src emacs-lisp
(with-eval-after-load 'corfu
  (with-eval-after-load 'orderless
    (orderless-define-completion-style orderless-literal-only
      (orderless-style-dispatchers nil)
      (orderless-matching-styles '(orderless-literal)))
    
    (add-hook 'corfu-mode-hook
              (lambda ()
		(setq-local completion-styles '(orderless-literal-only basic)
                            completion-category-overrides nil
                            completion-category-defaults nil)))))
#+end_src

*** Consult - Enhanced search commands

#+begin_src emacs-lisp
(use-package consult
  :after vertico
  :custom
  ;; Basic consult settings
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  
  ;; Use fd-find instead of find
  (consult-find-args "fd --color=never --full-path")
  
  ;; Enhanced ripgrep configuration
  (consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip --hidden --glob '!.git/'")
  
  ;; Use ripgrep for consult-grep as well
  (consult-grep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip")
  
  :config
  ;; Configure xref and registers
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Enhanced project detection
  (when (fboundp 'project-current)
    (setq consult-project-function
          (lambda (_)
            (when-let (project (project-current))
              (if (fboundp 'project-root)
                  (project-root project)
                (car (project-roots project)))))))

  ;; Configure consult preview
  (setq consult-preview-key "M-.")
  
  (custom-set-faces
   '(consult-bookmark ((t (:foreground "#d3869b"))))
   '(consult-buffer ((t (:foreground "#ebdbb2"))))
   '(consult-file ((t (:foreground "#8ec07c"))))
   '(consult-imenu-prefix ((t (:foreground "#7c6f64"))))
   '(consult-key ((t (:foreground "#fe8019"))))
   '(consult-line-number ((t (:foreground "#7c6f64"))))
   '(consult-line-number-prefix ((t (:foreground "#504945"))))
   '(consult-line-number-wrapped ((t (:foreground "#fb4934"))))
   '(consult-narrow-indicator ((t (:foreground "#fabd2f"))))
   '(consult-preview-cursor ((t (:background "#fe8019"))))
   '(consult-preview-error ((t (:foreground "#fb4934"))))
   '(consult-preview-insertion ((t (:background "#b8bb26" :foreground "#282828"))))
   '(consult-preview-line ((t (:background "#504945"))))
   '(consult-preview-match ((t (:background "#d3869b" :foreground "#282828"))))
   '(consult-separator ((t (:foreground "#7c6f64"))))))
#+end_src

*** TODO Keybindings
=separate out the keybindings for their respective setups=
#+begin_src emacs-lisp
;; ;; Buffer operations (SPC b)
;; (global-leader-key
;;  "b" '(:ignore t :which-key "buffer")
;;  "bb" '(consult-buffer :which-key "switch buffer")
;;  "br" '(consult-recent-file :which-key "recent files")
;;  "bB" '(consult-buffer-other-window :which-key "switch buffer other window")
;;  "bi" '(consult-imenu :which-key "imenu")
;;  "bI" '(consult-imenu-multi :which-key "imenu multi")
;;  "bo" '(consult-outline :which-key "outline")
;;  "bm" '(consult-bookmark :which-key "bookmarks")
;;  "by" '(consult-yank-pop :which-key "yank ring"))

;; ;; File operations (SPC f)

;; ;; Search operations (SPC s)
;; (global-leader-key
;;  "s" '(:ignore t :which-key "search")
;;  "ss" '(consult-line :which-key "search line")
;;  "sS" '(consult-line-multi :which-key "search line multi")
;;  "sp" '(consult-ripgrep :which-key "ripgrep project")
;;  "sP" '(consult-git-grep :which-key "git grep")
;;  "sd" '(consult-find :which-key "find file")
;;  "sk" '(consult-keep-lines :which-key "keep lines")
;;  "sK" '(consult-flush-lines :which-key "flush lines")
;;  "sf" '(consult-focus-lines :which-key "focus lines"))

;; ;; Jump/Go operations (SPC j)
;; (global-leader-key
;;  "j" '(:ignore t :which-key "jump")
;;  "jj" '(consult-line :which-key "jump to line")
;;  "jm" '(consult-mark :which-key "jump to mark")
;;  "jM" '(consult-global-mark :which-key "jump to global mark")
;;  "jo" '(consult-outline :which-key "jump to outline")
;;  "ji" '(consult-imenu :which-key "jump to imenu")
;;  "jI" '(consult-imenu-multi :which-key "jump to imenu multi"))

;; ;; Help operations (SPC h)
;; (global-leader-key
;;  "h" '(:ignore t :which-key "help")
;;  "ha" '(consult-apropos :which-key "apropos")
;;  "hm" '(consult-man :which-key "man pages")
;;  "hi" '(consult-info :which-key "info"))

;; ;; Project operations (SPC p)
;; (global-leader-key
;;  "p" '(:ignore t :which-key "project")
;;  "pf" '(consult-find :which-key "find file in project")
;;  "pp" '(consult-project-buffer :which-key "project buffers")
;;  "ps" '(consult-ripgrep :which-key "search in project"))

;; ;; Register operations (SPC r)
;; (global-leader-key
;;  "r" '(:ignore t :which-key "register")
;;  "rr" '(consult-register :which-key "registers")
;;  "rs" '(consult-register-store :which-key "store register")
;;  "rl" '(consult-register-load :which-key "load register"))

;; ;; Error/Compilation operations (SPC e)
;; (global-leader-key
;;  "e" '(:ignore t :which-key "error")
;;  "ee" '(consult-flymake :which-key "flymake errors")
;;  "ec" '(consult-compile-error :which-key "compilation errors"))

;; ;; Version control operations (SPC g)
;; (global-leader-key
;;  "g" '(:ignore t :which-key "git")
;;  "gs" '(consult-git-grep :which-key "git grep"))

;; ;; Alternative single-key bindings for frequently used commands
;; (general-define-key
;;  :keymaps 'override
;;  "C-s" 'consult-line
;;  "C-x b" 'consult-buffer
;;  "C-x C-r" 'consult-recent-file
;;  "M-y" 'consult-yank-pop
;;  "M-g g" 'consult-goto-line
;;  "M-g m" 'consult-mark
;;  "M-g M" 'consult-global-mark
;;  "M-g o" 'consult-outline
;;  "M-g i" 'consult-imenu
;;  "C-c h" 'consult-history
;;  "C-c k" 'consult-kmacro
;;  "C-c m" 'consult-mode-command
;;  "C-c c" 'consult-complex-command)
#+end_src

*** Consult Extensions

#+begin_src emacs-lisp
(use-package consult-flymake :ensure nil :after (consult flymake))
(use-package consult-dir :after consult)
#+end_src

*** Wgrep - Editable grep buffers

#+begin_src emacs-lisp
(use-package wgrep
  :after consult
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-enable-key "r")
  (wgrep-change-readonly-file t)
  :config
  ;; Gruvbox theme for wgrep
  (custom-set-faces
   '(wgrep-face ((t (:background "#504945" :foreground "#ebdbb2"))))
   '(wgrep-file-face ((t (:foreground "#83a598" :weight bold))))
   '(wgrep-reject-face ((t (:foreground "#fb4934" :weight bold))))
   '(wgrep-done-face ((t (:foreground "#b8bb26" :weight bold))))))
#+end_src

** Actions and Completion-at-Point
*** Embark - Context-aware actions

#+begin_src emacs-lisp
(use-package embark
  :ensure t
  :after (vertico consult)
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  ;; Use Embark for prefix help
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (custom-set-faces
   '(embark-keybinding ((t (:foreground "#fe8019" :weight bold))))
   '(embark-collect-marked ((t (:background "#504945" :foreground "#ebdbb2"))))
   '(embark-collect-group-title ((t (:foreground "#d3869b" :weight bold))))
   '(embark-collect-group-separator ((t (:foreground "#7c6f64")))))
  
  ;; which-key integration
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (when (fboundp 'which-key--hide-popup-ignore-command)
            (which-key--hide-popup-ignore-command))
        (when (fboundp 'which-key--show-keymap)
          (which-key--show-keymap
           (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if prefix (format " with <%s>" prefix) "")))
           (if prefix
               (pcase (lookup-key keymap (kbd prefix))
                 ((and (pred keymapp) km) km)
                 (_ (key-binding (kbd prefix))))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding)))))))))
  (when (fboundp 'which-key--show-keymap)
    (setq embark-indicators
          '(embark-which-key-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))))
#+end_src

*** Embark-Consult Integration

#+begin_src emacs-lisp
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
#+end_src

*** Cape - Completion at point extensions

#+begin_src emacs-lisp
;; (use-package cape
;;   :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
;;   :init
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev)
;;   (add-hook 'completion-at-point-functions #'cape-file)
;;   (add-hook 'completion-at-point-functions #'cape-elisp-block)
;;   (add-hook 'completion-at-point-functions #'cape-tex))
#+end_src

#+begin_src emacs-lisp
(use-package cape
  :after corfu
  :bind (("C-c p p" . completion-at-point)
         ("C-c p t" . complete-tag)
         ("C-c p d" . cape-dabbrev)
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex))
  :init
  ;; Add useful defaults to completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :config
  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
#+end_src

** Custom Functions

#+begin_src emacs-lisp
;;; Custom search functions
(defun my/consult-line-symbol-at-point ()
  "Run consult-line with symbol at point as initial input."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (consult-line symbol)))

(defun my/consult-ripgrep-symbol-at-point ()
  "Run consult-ripgrep with symbol at point as initial input."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (consult-ripgrep nil symbol))))

;;; Helper function for migrating from Company
(defun my/cape-company-to-capf (backend)
  "Convert company BACKEND to cape completion-at-point-function."
  (when (and (fboundp backend) (fboundp 'cape-company-to-capf))
    (cape-company-to-capf backend)))
#+end_src

* UI and Theme
** Fonts
#+begin_src emacs-lisp
(defun efs/set-font-faces ()
  (message "Setting faces!")
  (set-face-attribute 'default nil
		      :font "JetBrainsMono Nerd Font"
		      :height 145
		      :weight 'medium)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
		      :font "JetBrainsMono Nerd Font"
		      :height 145
		      :weight 'medium)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
		      :font "JetBrainsMono Nerd Font"
		      :height 145
		      :weight 'medium))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (efs/set-font-faces))))
  (efs/set-font-faces))

;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.02)
#+end_src
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
** Nerd Icons

#+begin_src emacs-lisp
(use-package nerd-icons
  :if (display-graphic-p)
  :custom
  (nerd-icons-font-family "JetBrainsMono Nerd Font")
  (nerd-icons-color-icons t)
  (nerd-icons-scale-factor 1.0)
  (nerd-icons-default-file-color "#ebdbb2")
  (nerd-icons-default-dir-color "#83a598")
  :config
  ;; Color mappings for different file types
  (add-to-list 'nerd-icons-extension-icon-alist
               '("org" nerd-icons-sucicon "nf-custom-orgmode" :face (:foreground "#b8bb26")))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("el" nerd-icons-sucicon "nf-custom-emacs" :face (:foreground "#d3869b")))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("py" nerd-icons-devicon "nf-dev-python" :face (:foreground "#8ec07c")))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("js" nerd-icons-devicon "nf-dev-javascript" :face (:foreground "#fabd2f")))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("json" nerd-icons-devicon "nf-dev-javascript" :face (:foreground "#fe8019")))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("md" nerd-icons-octicon "nf-oct-markdown" :face (:foreground "#8ec07c")))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("yaml" nerd-icons-octicon "nf-oct-gear" :face (:foreground "#fb4934")))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("yml" nerd-icons-octicon "nf-oct-gear" :face (:foreground "#fb4934"))))
#+end_src

** Nerd Icons Completion
#+begin_src emacs-lisp
(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
#+end_src

** Nerd Icons Corfu
#+begin_src emacs-lisp
(use-package nerd-icons-corfu
  :ensure t
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  ;; Gruvbox colors for corfu icons
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face (:foreground "#8ec07c"))
          (boolean :style "cod" :icon "symbol_boolean" :face (:foreground "#b8bb26"))
          (class :style "cod" :icon "symbol_class" :face (:foreground "#d3869b"))
          (color :style "cod" :icon "symbol_color" :face (:foreground "#fe8019"))
          (command :style "cod" :icon "terminal" :face (:foreground "#83a598"))
          (constant :style "cod" :icon "symbol_constant" :face (:foreground "#fabd2f"))
          (constructor :style "cod" :icon "triangle_right" :face (:foreground "#8ec07c"))
          (enummember :style "cod" :icon "symbol_enum_member" :face (:foreground "#b8bb26"))
          (enum-member :style "cod" :icon "symbol_enum_member" :face (:foreground "#b8bb26"))
          (enum :style "cod" :icon "symbol_enum" :face (:foreground "#d3869b"))
          (event :style "cod" :icon "symbol_event" :face (:foreground "#fb4934"))
          (field :style "cod" :icon "symbol_field" :face (:foreground "#8ec07c"))
          (file :style "cod" :icon "symbol_file" :face (:foreground "#ebdbb2"))
          (folder :style "cod" :icon "folder" :face (:foreground "#83a598"))
          (interface :style "cod" :icon "symbol_interface" :face (:foreground "#d3869b"))
          (keyword :style "cod" :icon "symbol_keyword" :face (:foreground "#fe8019"))
          (macro :style "cod" :icon "symbol_misc" :face (:foreground "#fabd2f"))
          (magic :style "cod" :icon "wand" :face (:foreground "#d3869b"))
          (method :style "cod" :icon "symbol_method" :face (:foreground "#83a598"))
          (function :style "cod" :icon "symbol_method" :face (:foreground "#83a598"))
          (module :style "cod" :icon "file_submodule" :face (:foreground "#b8bb26"))
          (numeric :style "cod" :icon "symbol_numeric" :face (:foreground "#fe8019"))
          (operator :style "cod" :icon "symbol_operator" :face (:foreground "#fb4934"))
          (param :style "cod" :icon "symbol_parameter" :face (:foreground "#8ec07c"))
          (property :style "cod" :icon "symbol_property" :face (:foreground "#b8bb26"))
          (reference :style "cod" :icon "references" :face (:foreground "#8ec07c"))
          (snippet :style "cod" :icon "symbol_snippet" :face (:foreground "#fabd2f"))
          (string :style "cod" :icon "symbol_string" :face (:foreground "#b8bb26"))
          (struct :style "cod" :icon "symbol_structure" :face (:foreground "#d3869b"))
          (text :style "cod" :icon "symbol_key" :face (:foreground "#ebdbb2"))
          (typeparameter :style "cod" :icon "list_unordered" :face (:foreground "#8ec07c"))
          (type-parameter :style "cod" :icon "list_unordered" :face (:foreground "#8ec07c"))
          (unit :style "cod" :icon "symbol_ruler" :face (:foreground "#b8bb26"))
          (value :style "cod" :icon "symbol_field" :face (:foreground "#ebdbb2"))
          (variable :style "cod" :icon "symbol_variable" :face (:foreground "#8ec07c"))
          (t :style "cod" :icon "code" :face (:foreground "#ebdbb2")))))
#+end_src

** Nerd Icons Dired
#+begin_src emacs-lisp
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))
#+end_src
** Doom Themes
#+begin_src emacs-lisp
(use-package doom-themes
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-treemacs-theme "doom-miramare") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-miramare t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))
#+end_src

** Doom Modeline
#+begin_src emacs-lisp
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 28
	doom-modeline-bar-width 3
	doom-modeline-icon (display-graphic-p)
	doom-modeline-major-mode-icon t
	doom-modeline-major-mode-color-icon t
	doom-modeline-buffer-file-name-style 'truncate-upto-project
	doom-modeline-buffer-state-icon t
	doom-modeline-buffer-modification-icon t
	doom-modeline-buffer-file-name-style 'relative-from-project
	doom-modeline-minor-modes nil
	doom-modeline-enable-word-count nil
	doom-modeline-buffer-encoding t
	doom-modeline-indent-info nil
	doom-modeline-project-detection 'auto
	doom-modeline-lsp t
	doom-modeline-checker-simple-format t
	doom-modeline-vcs-max-length 12
	doom-modeline-env-version t
	doom-modeline-irc-stylize 'identity
	doom-modeline-github-timer nil
	doom-modeline-gnus-timer nil))
#+end_src

** Solaire Mode 
#+begin_src emacs-lisp
;; Solaire Mode Configuration for Emacs 30
;; Provides visual distinction between "real" buffers and popups/sidebars

(use-package solaire-mode
  :ensure t
  :config
  ;; Enable solaire-mode globally
  (solaire-global-mode +1)
  
  ;; Integration with corfu popups
  (with-eval-after-load 'corfu
    ;; Apply solaire styling to corfu's completion buffer
    (advice-add 'corfu--make-buffer :after
                (lambda (candidates &rest _)
                  (when-let ((buffer (get-buffer " *corfu*")))
                    (with-current-buffer buffer
                      (solaire-mode +1)))))
    
    ;; Ensure corfu popup inherits proper background
    (setq corfu-auto-delay 0.2
          corfu-auto-prefix 2))
  
  ;; Integration with vertico minibuffer completion
  (with-eval-after-load 'vertico
    (advice-add 'vertico--display-candidates :after
                (lambda (&rest _)
                  (when (minibufferp)
                    (with-selected-window (minibuffer-window)
                      (solaire-mode +1))))))
  
  ;; Enable solaire-mode for ediff control panels
  (with-eval-after-load 'ediff
    (advice-add 'ediff-setup-control-buffer :after
                (lambda (&rest _)
                  (solaire-mode +1))))
  
  ;; Integration with org-mode source blocks
  (with-eval-after-load 'org
    (add-hook 'org-src-mode-hook #'solaire-mode))
  
  ;; Integration with which-key popups
  (with-eval-after-load 'which-key
    (advice-add 'which-key--show-buffer-side-window :after
                (lambda (&rest _)
                  (when-let ((buffer (get-buffer which-key--buffer)))
                    (with-current-buffer buffer
                      (solaire-mode +1))))))
  
  ;; Apply to help and info buffers
  (dolist (mode '(help-mode-hook info-mode-hook))
    (add-hook mode #'solaire-mode))
  
  )
#+end_src

** Which Key
#+begin_src emacs-lisp
(use-package which-key
  :init
  (which-key-mode 1)
  :config
  ;; Core timing settings
  (setq which-key-idle-delay 0.05
	which-key-idle-secondary-delay 0.00)
 
  ;; Display configuration
  (setq which-key-min-display-lines 6
	which-key-max-display-columns nil
	which-key-max-description-length 32
	which-key-allow-imprecise-window-fit t
	which-key-separator " → "
	which-key-unicode-correction 3)
  
  ;; Popup and display behavior
  (setq which-key-popup-type 'minibuffer
	which-key-show-prefix 'echo
	which-key-show-remaining-keys t
	which-key-show-early-on-C-h t
	which-key-enable-extended-define-key t)
  
  ;; Sorting and organization
  (setq which-key-sort-order 'which-key-key-order-alpha
	which-key-sort-uppercase-first nil
	which-key-side-window-max-width 0.33
	which-key-side-window-max-height 0.25)
  
  ;; Performance and compatibility settings
  (setq which-key-compute-remaps nil
	which-key-use-C-h-commands t
	which-key-show-operator-state-maps t
	which-key-preserve-window-configuration t)

  ;; Gruvbox theme for which-key
  (custom-set-faces
   '(which-key-key-face ((t (:foreground "#83a598" :weight bold))))
   '(which-key-description-face ((t (:foreground "#ebdbb2"))))
   '(which-key-group-description-face ((t (:foreground "#d3869b"))))
   '(which-key-command-description-face ((t (:foreground "#b8bb26"))))
   '(which-key-local-map-description-face ((t (:foreground "#8ec07c"))))
   '(which-key-separator-face ((t (:foreground "#7c6f64"))))
   '(which-key-note-face ((t (:foreground "#7c6f64"))))
   '(which-key-note-face ((t (:foreground "#7c6f64"))))))
#+end_src

** Dashboard
#+begin_src emacs-lisp
;; use-package with package.el:
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  ;; show dashboard in daemon
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)
                          (registers . 5)))
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  dashboard-insert-navigator
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  dashboard-insert-items
                                  dashboard-insert-newline
                                  dashboard-insert-footer))
  (setq dashboard-display-icons-p t
	dashboard-icon-type 'nerd-icons     
	dashboard-set-heading-icons t
	dashboard-set-file-icons t)
  (setq dashboard-startup-banner 'logo))
#+end_src

** Ligatures

#+begin_src emacs-lisp
;;; Unicode and Font Configuration
(use-package emacs
  :ensure nil
  :config
  ;; Enable Unicode support
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  
  ;; Ensure proper Unicode handling in daemon mode
  (when (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (set-language-environment "UTF-8")
                (set-default-coding-systems 'utf-8))))
  
  ;; Font fallback for Unicode characters
  (set-fontset-font t 'unicode-bmp "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'unicode-sip "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'mathematical "Noto Color Emoji" nil 'append))

;;; Prettify Symbols Mode Configuration
(use-package prog-mode
  :ensure nil
  :config
  ;; Base prettify symbols for programming modes
  (defvar base-prettify-symbols-alist
    '(;; Logical operators
      ("&&" . ?∧)
      ("||" . ?∨)
      ("not" . ?¬)
      
      ;; Relational operators
      ("==" . ?≡)
      ("!=" . ?≠)
      ("<=" . ?≤)
      (">=" . ?≥)
      
      ;; Arrows
      ("->" . ?→)
      ("=>" . ?⇒)
      ("<-" . ?←)
      ("<->" . ?↔)
      ("<=>" . ?⇔)
      
      ;; Mathematical
      ("sum" . ?∑)
      ("product" . ?∏)
      ("sqrt" . ?√)
      ("infinity" . ?∞)
      ("alpha" . ?α)
      ("beta" . ?β)
      ("gamma" . ?γ)
      ("delta" . ?δ)
      ("lambda" . ?λ)
      ("pi" . ?π)
      ("sigma" . ?σ)
      ("theta" . ?θ)
      ("omega" . ?ω)
      
      ;; Others
      ("..." . ?…)
      ("null" . ?∅)
      ("true" . ?⊤)
      ("false" . ?⊥))
    "Base prettify symbols alist for all modes."))

;;; Text-mode specific configuration
(use-package text-mode
  :ensure nil
  :hook (text-mode . (lambda ()
                       (setq-local prettify-symbols-alist
                                   (append base-prettify-symbols-alist
                                           text-prettify-symbols-alist))
                       (prettify-symbols-mode 1)))
  :config
  ;; Text-mode specific prettification
  (defvar text-prettify-symbols-alist
    '(;; Common text symbols
      ("(c)" . ?©)
      ("(r)" . ?®)
      ("(tm)" . ?™)
      ("+-" . ?±)
      ("1/2" . ?½)
      ("1/3" . ?⅓)
      ("1/4" . ?¼)
      ("2/3" . ?⅔)
      ("3/4" . ?¾)
      ("1/8" . ?⅛)
      ("3/8" . ?⅜)
      ("5/8" . ?⅝)
      ("7/8" . ?⅞)
      ("---" . ?—)
      ("--" . ?–)
      ("..." . ?…)
      ("<<" . ?«)
      (">>" . ?»)
      ("<->" . ?↔)
      ("->" . ?→)
      ("<-" . ?←)
      ("=>" . ?⇒)
      ("<=>" . ?⇔)
      ("degree" . ?°)
      ("celsius" . ?℃)
      ("fahrenheit" . ?℉))
    "Text-mode specific prettify symbols."))

;;; LaTeX-mode specific configuration
(use-package tex-mode
  :ensure nil
  :hook (latex-mode . (lambda ()
                        (setq-local prettify-symbols-alist
                                    (append base-prettify-symbols-alist
                                            latex-prettify-symbols-alist))
                        (prettify-symbols-mode 1)))
  :config
  ;; LaTeX-mode specific prettification
  (defvar latex-prettify-symbols-alist
    '(;; Greek letters (lowercase)
      ("\\alpha" . ?α)
      ("\\beta" . ?β)
      ("\\gamma" . ?γ)
      ("\\delta" . ?δ)
      ("\\epsilon" . ?ε)
      ("\\varepsilon" . ?ε)
      ("\\zeta" . ?ζ)
      ("\\eta" . ?η)
      ("\\theta" . ?θ)
      ("\\vartheta" . ?ϑ)
      ("\\iota" . ?ι)
      ("\\kappa" . ?κ)
      ("\\lambda" . ?λ)
      ("\\mu" . ?μ)
      ("\\nu" . ?ν)
      ("\\xi" . ?ξ)
      ("\\pi" . ?π)
      ("\\varpi" . ?ϖ)
      ("\\rho" . ?ρ)
      ("\\varrho" . ?ϱ)
      ("\\sigma" . ?σ)
      ("\\varsigma" . ?ς)
      ("\\tau" . ?τ)
      ("\\upsilon" . ?υ)
      ("\\phi" . ?φ)
      ("\\varphi" . ?ϕ)
      ("\\chi" . ?χ)
      ("\\psi" . ?ψ)
      ("\\omega" . ?ω)
      
      ;; Greek letters (uppercase)
      ("\\Gamma" . ?Γ)
      ("\\Delta" . ?Δ)
      ("\\Theta" . ?Θ)
      ("\\Lambda" . ?Λ)
      ("\\Xi" . ?Ξ)
      ("\\Pi" . ?Π)
      ("\\Sigma" . ?Σ)
      ("\\Upsilon" . ?Υ)
      ("\\Phi" . ?Φ)
      ("\\Psi" . ?Ψ)
      ("\\Omega" . ?Ω)
      
      ;; Mathematical operators
      ("\\sum" . ?∑)
      ("\\prod" . ?∏)
      ("\\coprod" . ?∐)
      ("\\int" . ?∫)
      ("\\iint" . ?∬)
      ("\\iiint" . ?∭)
      ("\\oint" . ?∮)
      ("\\infty" . ?∞)
      ("\\partial" . ?∂)
      ("\\nabla" . ?∇)
      ("\\pm" . ?±)
      ("\\mp" . ?∓)
      ("\\times" . ?×)
      ("\\div" . ?÷)
      ("\\cdot" . ?·)
      ("\\bullet" . ?•)
      ("\\circ" . ?∘)
      ("\\oplus" . ?⊕)
      ("\\ominus" . ?⊖)
      ("\\otimes" . ?⊗)
      ("\\oslash" . ?⊘)
      ("\\odot" . ?⊙)
      ("\\bigcirc" . ?◯)
      ("\\dagger" . ?†)
      ("\\ddagger" . ?‡)
      ("\\star" . ?⋆)
      ("\\ast" . ?∗)
      
      ;; Relations
      ("\\leq" . ?≤)
      ("\\geq" . ?≥)
      ("\\equiv" . ?≡)
      ("\\models" . ?⊨)
      ("\\prec" . ?≺)
      ("\\succ" . ?≻)
      ("\\sim" . ?∼)
      ("\\perp" . ?⊥)
      ("\\preceq" . ?≼)
      ("\\succeq" . ?≽)
      ("\\simeq" . ?≃)
      ("\\mid" . ?∣)
      ("\\ll" . ?≪)
      ("\\gg" . ?≫)
      ("\\asymp" . ?≍)
      ("\\parallel" . ?∥)
      ("\\subset" . ?⊂)
      ("\\supset" . ?⊃)
      ("\\approx" . ?≈)
      ("\\subseteq" . ?⊆)
      ("\\supseteq" . ?⊇)
      ("\\cong" . ?≅)
      ("\\neq" . ?≠)
      ("\\in" . ?∈)
      ("\\ni" . ?∋)
      ("\\propto" . ?∝)
      ("\\vdash" . ?⊢)
      ("\\dashv" . ?⊣)
      ("\\notin" . ?∉)
      
      ;; Arrows
      ("\\leftarrow" . ?←)
      ("\\gets" . ?←)
      ("\\rightarrow" . ?→)
      ("\\to" . ?→)
      ("\\leftrightarrow" . ?↔)
      ("\\Leftarrow" . ?⇐)
      ("\\Rightarrow" . ?⇒)
      ("\\Leftrightarrow" . ?⇔)
      ("\\mapsto" . ?↦)
      ("\\hookleftarrow" . ?↩)
      ("\\hookrightarrow" . ?↪)
      ("\\uparrow" . ?↑)
      ("\\downarrow" . ?↓)
      ("\\updownarrow" . ?↕)
      ("\\Uparrow" . ?⇑)
      ("\\Downarrow" . ?⇓)
      ("\\Updownarrow" . ?⇕)
      ("\\nearrow" . ?↗)
      ("\\searrow" . ?↘)
      ("\\swarrow" . ?↙)
      ("\\nwarrow" . ?↖)
      
      ;; Logic symbols
      ("\\land" . ?∧)
      ("\\lor" . ?∨)
      ("\\lnot" . ?¬)
      ("\\neg" . ?¬)
      ("\\top" . ?⊤)
      ("\\bot" . ?⊥)
      ("\\exists" . ?∃)
      ("\\forall" . ?∀)
      ("\\nexists" . ?∄)
      
      ;; Set theory
      ("\\emptyset" . ?∅)
      ("\\varnothing" . ?∅)
      ("\\cap" . ?∩)
      ("\\cup" . ?∪)
      ("\\bigcap" . ?⋂)
      ("\\bigcup" . ?⋃)
      ("\\setminus" . ?∖)
      
      ;; Miscellaneous
      ("\\angle" . ?∠)
      ("\\wp" . ?℘)
      ("\\Re" . ?ℜ)
      ("\\Im" . ?ℑ)
      ("\\aleph" . ?ℵ)
      ("\\hbar" . ?ℏ)
      ("\\ell" . ?ℓ)
      ("\\partial" . ?∂)
      ("\\clubsuit" . ?♣)
      ("\\diamondsuit" . ?♢)
      ("\\heartsuit" . ?♡)
      ("\\spadesuit" . ?♠)
      ("\\sharp" . ?♯)
      ("\\flat" . ?♭)
      ("\\natural" . ?♮)
      ("\\surd" . ?√)
      ("\\triangle" . ?△)
      ("\\checkmark" . ?✓))
    "LaTeX-mode specific prettify symbols."))

;;; Auto-composition mode for true ligatures (when supported)
(use-package composite
  :ensure nil
  :config
  ;; Enable auto-composition mode globally
  (global-auto-composition-mode 1))

;;; Performance optimizations
(use-package emacs
  :ensure nil
  :config
  ;; Optimize composition performance
  (setq-default bidi-display-reordering 'left-to-right)
  (setq bidi-paragraph-direction 'left-to-right)
  
  ;; Optimize font rendering
  (setq inhibit-compacting-font-caches t)
  (setq font-lock-maximum-decoration t)
  
  ;; Ensure smooth scrolling with ligatures
  (setq scroll-conservatively 10000)
  (setq scroll-preserve-screen-position t)
  (setq scroll-margin 0)
  (setq-default cursor-type 'box)
  (setq x-stretch-cursor t))

;;; Helper functions
(defun toggle-prettify-symbols ()
  "Toggle prettify symbols mode in current buffer."
  (interactive)
  (if (bound-and-true-p prettify-symbols-mode)
      (progn
        (prettify-symbols-mode -1)
        (message "Prettify symbols disabled"))
    (progn
      (prettify-symbols-mode 1)
      (message "Prettify symbols enabled"))))

(defun reload-prettify-symbols ()
  "Reload prettify symbols configuration for current buffer."
  (interactive)
  (when (derived-mode-p 'org-mode 'text-mode 'latex-mode)
    (font-lock-refresh-defaults)
    (when (bound-and-true-p prettify-symbols-mode)
      (prettify-symbols-mode -1)
      (prettify-symbols-mode 1))
    (message "Prettify symbols configuration reloaded")))

;; Global keybindings
(global-set-key (kbd "C-c l t") 'toggle-prettify-symbols)
(global-set-key (kbd "C-c l r") 'reload-prettify-symbols)
#+end_src

** Hide Mode Line
#+begin_src emacs-lisp
(use-package hide-mode-line)
(add-hook 'completion-list-mode-hook #'hide-mode-line-mode)
(add-hook 'vterm-mode-hook #'hide-mode-line-mode)
(add-hook 'org-agenda-mode-hook #'hide-mode-line-mode)
#+end_src

* Emacs Behaviors and Enhancements
** Minibuffer Enhancements

#+begin_src emacs-lisp
(use-package minibuffer
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)
  :config
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; (add-hook 'minibuffer-setup-hook #'(lambda () (setq-local completion-at-point-functions
  ;;                                                         (list #'cape-history #'cape-file #'cape-dabbrev))))
  ;; Gruvbox theme for minibuffer
  (custom-set-faces
   '(minibuffer-prompt ((t (:foreground "#83a598" :weight bold))))
   '(completions-annotations ((t (:foreground "#a89984" :italic t))))
   '(completions-first-difference ((t (:foreground "#fabd2f" :weight bold))))
   '(completions-first-difference ((t (:foreground "#fabd2f" :weight bold))))))
#+end_src

** Savehist - persist history over Emacs restarts

#+begin_src emacs-lisp
(use-package savehist
  :ensure nil ; built-in
  :init (savehist-mode 1)
  :custom
  (history-length 25)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables
   '(kill-ring
     search-ring
     regexp-search-ring
     last-kbd-macro
     kmacro-ring
     shell-command-history
     extended-command-history)))
#+end_src

** Recentf - track recently opened files

#+begin_src emacs-lisp
(use-package recentf
  :ensure nil ; built-in
  :init (recentf-mode 1)
  :custom
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  (recentf-exclude '("\\elpa" "\\straight" "/tmp/" "/ssh:" "/sudo:" "COMMIT_EDITMSG")))
#+end_src

** Abbrev mode enhancements

#+begin_src emacs-lisp
(use-package abbrev
  :ensure nil
  :diminish abbrev-mode
  :custom
  (save-abbrevs 'silently)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
  (add-hook 'text-mode-hook #'abbrev-mode)
  (add-hook 'prog-mode-hook #'abbrev-mode))
#+end_src

** Dabbrev (Dynamic Abbreviation)

#+begin_src emacs-lisp
(use-package dabbrev
  :ensure nil
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode) 
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil)
  (dabbrev-upcase-means-case-search t))
#+end_src

** Zooming In/Out

#+begin_src emacs-lisp
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
#+end_src

** Winner Mode

#+begin_src emacs-lisp
(use-package winner
  :ensure nil
  :init
  (winner-mode 1)
  :bind
  (("C-c <left>" . winner-undo)
   ("C-c <right>" . winner-redo))
  :config
  ;; Optional: Set the maximum number of window configurations to remember
  (setq winner-ring-size 100)
  
  ;; Optional: Don't record winner history for certain buffer types
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*")))
#+end_src

** Zap-to-Char
zzz-to-char and zzz-up-to-char which work like the built-ins zap-to-char and zap-up-to-char, but allow the user to quickly select the exact character they want to zzz to. 
#+begin_src emacs-lisp
(use-package zzz-to-char
  :bind (("M-z" . zzz-up-to-char)
         ("M-Z" . zzz-to-char))
  :config
  ;; Set the timeout for character input (default is 1 second)
  (setq zzz-to-char-reach 80))
#+end_src

** Word Wrap
#+begin_src emacs-lisp 
(global-visual-line-mode 1)
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'prog-mode-hook 'auto-fill-mode)
#+end_src

** Bookmark
#+begin_src emacs-lisp
(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-save-flag 1)) 
#+end_src

** Move Text
#+begin_src emacs-lisp
(use-package move-text
  :commands (move-text-up move-text-down)
  :init
  (move-text-default-bindings)
  :config
  (defun my/indent-region-advice (&rest ignored)
    "Auto-indent moved text for better formatting."
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  
  (advice-add 'move-text-up :after 'my/indent-region-advice)
  (advice-add 'move-text-down :after 'my/indent-region-advice))
#+end_src

** Vterm
*** Core Setup
#+begin_src emacs-lisp
(use-package vterm
  :defer t
  :init
  ;; Core settings
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 10000
        vterm-shell "/usr/bin/zsh"
        vterm-timer-delay 0.01)
  
  ;; Environment variables for zsh integration
  (setq vterm-environment
        '("TERM=xterm-256color"
          "EMACS_VTERM=t"))
  
  :config
  ;; Enable find-file and message commands from vterm
  (setq vterm-eval-cmds
        '(("find-file" find-file)
          ("message" message)))
  
  ;; Buffer setup hook
  (defun my/vterm-setup ()
    "Configure vterm buffer settings."
    (setq-local truncate-lines nil
                scroll-conservatively 101
                scroll-margin 0))
  
  (add-hook 'vterm-mode-hook #'my/vterm-setup)
  (add-hook 'vterm-mode-hook #'hide-mode-line-mode))
#+end_src

*** Vterm Toggle
#+begin_src emacs-lisp
(use-package vterm-toggle
  :after vterm
  :init
  ;; Configure vterm-toggle behavior
  (setq vterm-toggle-fullscreen-p nil
        vterm-toggle-scope 'project)
  
  :config
  ;; Display vterm at bottom with proper sizing
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  
  ;; Project integration functions
  (defun my/vterm-project-root ()
    "Get project root directory."
    (if (fboundp 'project-root)
        (when-let ((project (project-current)))
          (project-root project))
      default-directory))
  
  (defun my/vterm-toggle-project ()
    "Toggle vterm in project root."
    (interactive)
    (let* ((project-root (my/vterm-project-root))
           (default-directory project-root))
      (vterm-toggle)))
  
  (defun my/vterm-new-project ()
    "Create new vterm buffer in project root."
    (interactive)
    (let* ((project-root (my/vterm-project-root))
           (project-name (file-name-nondirectory 
                         (directory-file-name project-root)))
           (buffer-name (format "*vterm-%s*" project-name))
           (default-directory project-root))
      (vterm buffer-name)))
  
  ;; Utility functions
  (defun my/vterm-send-clear ()
    "Clear vterm buffer."
    (interactive)
    (vterm-send-string "clear")
    (vterm-send-return))
  
  (defun my/vterm-send-cd-project ()
    "Change directory to project root in vterm."
    (interactive)
    (when-let ((project-root (my/vterm-project-root)))
      (vterm-send-string (format "cd %s" (shell-quote-argument project-root)))
      (vterm-send-return))))
#+end_src

*** Zsh Integration
#+begin_src emacs-lisp
(defun my/create-zsh-vterm-config ()
  "Create zsh configuration for vterm integration."
  (interactive)
  (let ((config-content "# Vterm integration for zsh
if [[ \"$EMACS_VTERM\" == \"t\" ]]; then
    # Directory tracking
    vterm_printf() {
        if [ -n \"$TMUX\" ] && ([ \"${TERM%%-*}\" = \"tmux\" ] || [ \"${TERM%%-*}\" = \"screen\" ]); then
            printf \"\\ePtmux;\\e\\e]%s\\007\\e\\\\\" \"$1\"
        elif [ \"${TERM%%-*}\" = \"screen\" ]; then
            printf \"\\eP\\e]%s\\007\\e\\\\\" \"$1\"
        else
            printf \"\\e]%s\\e\\\\\" \"$1\"
        fi
    }
    
    vterm_prompt_end() {
        vterm_printf \"51;A$(whoami)@$(hostname):$(pwd)\"
    }
    
    # Add to prompt
    setopt PROMPT_SUBST
    PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
    
    # Clear function
    vterm_clear() {
        vterm_printf \"51;Evterm-clear-scrollback\"
        clear
    }
    
    # Directory sync on cd
    chpwd() {
        vterm_printf \"51;A$(whoami)@$(hostname):$(pwd)\"
    }
    
    # Alias for clear
    alias clear='vterm_clear'
fi
"))
    (write-region config-content nil (expand-file-name "~/.zshrc.vterm"))
    (message "Zsh vterm config created at ~/.zshrc.vterm")
    (message "Add 'source ~/.zshrc.vterm 2>/dev/null || true' to your ~/.zshrc")))

;; Create the zsh config file
(when (and (executable-find "zsh") 
           (not (file-exists-p "~/.zshrc.vterm")))
  (my/create-zsh-vterm-config))
#+end_src

** Anzu
#+begin_src emacs-lisp
(use-package anzu
  :init
  (global-anzu-mode)
  :config  
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-replace-threshold 50)
   '(anzu-replace-to-string-separator " => "))
  
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))
#+end_src

** Smartparens
#+begin_src emacs-lisp
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))
#+end_src

* Programming
** Treesit
*** Basic tree-sitter configuration

#+begin_src emacs-lisp
(use-package treesit
  :ensure nil 
  :when (treesit-available-p)
  :config
  (setq treesit-font-lock-level 4) ; Maximum syntax highlighting
  
  ;; Performance optimizations for tree-sitter
  (setq treesit-max-buffer-size (* 1024 1024)) ; 1MB limit
  
  ;; Function to toggle tree-sitter debugging
  (defun my/treesit-debug-toggle ()
    "Toggle tree-sitter debugging."
    (interactive)
    (if (bound-and-true-p treesit--indent-verbose)
        (progn
          (setq treesit--indent-verbose nil)
          (message "Tree-sitter debugging disabled"))
      (setq treesit--indent-verbose t)
      (message "Tree-sitter debugging enabled")))
  
  ;; Function to show tree-sitter information
  (defun my/treesit-info ()
    "Show tree-sitter information for current buffer."
    (interactive)
    (if (treesit-parser-list)
        (let ((parsers (treesit-parser-list)))
          (message "Tree-sitter parsers: %s"
                   (mapconcat (lambda (parser)
                               (symbol-name (treesit-parser-language parser)))
                             parsers ", ")))
      (message "No tree-sitter parsers in current buffer")))
  
  ;; Keybindings
  (global-set-key (kbd "C-c t d") #'my/treesit-debug-toggle)
  (global-set-key (kbd "C-c t i") #'my/treesit-info))
#+end_src

*** Automatic tree-sitter mode management

#+begin_src emacs-lisp
(use-package treesit-auto
  :when (treesit-available-p)
  :config
  ;; Configure which modes to auto-enable
  (setq treesit-auto-langs '(bash c cpp css json python rust toml yaml))
  
  ;; Install and configure tree-sitter modes automatically
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
#+end_src

*** Language-specific configurations
treesit-auto handles mode associations automatically
#+begin_src emacs-lisp
;; Python setup
(defun my/python-ts-mode-setup ()
  "Setup for Python tree-sitter mode."
  (setq-local indent-tabs-mode nil)
  (setq-local python-indent-offset 4)
  (setq-local tab-width 4))

;; C/C++ setup
(defun my/c-ts-mode-setup ()
  "Setup for C tree-sitter mode."
  (setq-local c-ts-mode-indent-offset 4)
  (setq-local tab-width 4))

(defun my/c++-ts-mode-setup ()
  "Setup for C++ tree-sitter mode."
  (setq-local c-ts-mode-indent-offset 4)
  (setq-local tab-width 4))

(defun my/json-ts-mode-setup ()
  "Setup for JSON tree-sitter mode."
  (setq-local js-indent-level 2)
  (setq-local tab-width 2))

(defun my/yaml-ts-mode-setup ()
  "Setup for YAML tree-sitter mode."
  (setq-local yaml-indent-offset 2)
  (setq-local tab-width 2))

(defun my/css-ts-mode-setup ()
  "Setup for CSS tree-sitter mode."
  (setq-local css-indent-offset 2)
  (setq-local tab-width 2))

(defun my/bash-ts-mode-setup ()
  "Setup for Bash tree-sitter mode."
  (setq-local sh-basic-offset 2)
  (setq-local tab-width 2))

;; Apply hooks for language-specific setups
(add-hook 'python-ts-mode-hook #'my/python-ts-mode-setup)
(add-hook 'c-ts-mode-hook #'my/c-ts-mode-setup)
(add-hook 'c++-ts-mode-hook #'my/c++-ts-mode-setup)
(add-hook 'json-ts-mode-hook #'my/json-ts-mode-setup)
(add-hook 'yaml-ts-mode-hook #'my/yaml-ts-mode-setup)
(add-hook 'css-ts-mode-hook #'my/css-ts-mode-setup)
(add-hook 'html-ts-mode-hook #'my/html-ts-mode-setup)
(add-hook 'bash-ts-mode-hook #'my/bash-ts-mode-setup)
#+end_src

#+begin_src emacs-lisp
(use-package treesit-fold
  :when (treesit-available-p)
  :hook ((prog-mode . treesit-fold-mode))
  :bind (("C-c C-f" . treesit-fold-toggle)
         ("C-c C-o" . treesit-fold-open-all)
         ("C-c C-c" . treesit-fold-close-all)))
#+end_src

*** Electric Pair Mode

#+begin_src emacs-lisp
;; Tree-sitter based electric pair inhibition
(defun my/treesit-in-string-or-comment-p ()
  "Check if point is inside a string or comment using tree-sitter.
Returns t if inside string or comment, nil otherwise."
  (when (and (treesit-available-p) 
             (treesit-parser-list))
    (condition-case nil
      (let* ((node (treesit-node-at (point)))
             (node-type (when node (treesit-node-type node)))
             (parent-node (when node (treesit-node-parent node)))
             (parent-type (when parent-node (treesit-node-type parent-node))))
        (when node  ; Only proceed if we have a valid node
          (or
           ;; Check current node type for strings and comments
           (and node-type
                (or (string-match-p "\\(?:^\\|_\\)string\\(?:$\\|_\\)" node-type)
                    (string-match-p "\\(?:^\\|_\\)comment\\(?:$\\|_\\)" node-type)
                    (string-match-p "\\(?:^\\|_\\)literal\\(?:$\\|_\\)" node-type)
                    (string-match-p "\\(?:^\\|_\\)quoted\\(?:$\\|_\\)" node-type)))
           ;; Check parent node type (sometimes strings are nested)
           (and parent-type
                (or (string-match-p "\\(?:^\\|_\\)string\\(?:$\\|_\\)" parent-type)
                    (string-match-p "\\(?:^\\|_\\)comment\\(?:$\\|_\\)" parent-type)))
           ;; Language-specific checks
           (my/treesit-language-specific-string-comment-check node))))
      (error nil))))  ; Return nil on any error

(defun my/treesit-language-specific-string-comment-check (node)
  "Language-specific checks for strings and comments.
NODE is the tree-sitter node at point."
  (when node
    (let ((node-type (treesit-node-type node))
          (lang (when (treesit-parser-list)
                  (treesit-parser-language (car (treesit-parser-list))))))
      (pcase lang
        ;; Python-specific node types
        ('python
         (member node-type '("string" "comment" "string_content" "interpolation" 
                            "f_string" "raw_string_literal")))
        ;; JavaScript/TypeScript
        ((or 'javascript 'typescript)
         (member node-type '("string" "comment" "template_string" "regex" 
                            "string_fragment" "template_literal")))
        ;; C/C++
        ((or 'c 'cpp)
         (member node-type '("string_literal" "comment" "char_literal" 
                            "raw_string_literal" "string_content")))
        ;; Rust
        ('rust
         (member node-type '("string_literal" "comment" "char_literal" 
                            "raw_string_literal" "string_content")))
        ;; JSON
        ('json
         (member node-type '("string" "string_content")))
        ;; CSS
        ('css
         (member node-type '("string_value" "comment" "plain_value")))
        ;; HTML
        ('html
         (member node-type '("text" "comment" "attribute_value" "quoted_attribute_value")))
        ;; Bash
        ('bash
         (member node-type '("string" "comment" "raw_string" "string_expansion"
                            "command_substitution" "string_content")))
        ;; YAML
        ('yaml
         (member node-type '("string_scalar" "comment" "plain_scalar" 
                            "single_quote_scalar" "double_quote_scalar")))
        ;; Default fallback
        (_ nil)))))

(defun my/electric-pair-inhibit-predicate (char)
  "Predicate function to inhibit electric pairing in strings and comments.
CHAR is the character being inserted."
  (or
   ;; Use tree-sitter to check context
   (my/treesit-in-string-or-comment-p)
   ;; Fallback to default inhibition logic
   (electric-pair-default-inhibit char)))

;; Function to enable tree-sitter based electric pair inhibition
(defun my/enable-treesit-electric-pair-inhibition ()
  "Enable tree-sitter based electric pair inhibition."
  (interactive)
  (when (treesit-available-p)
    (setq-local electric-pair-inhibit-predicate #'my/electric-pair-inhibit-predicate)
    (message "Tree-sitter electric pair inhibition enabled")))

;; Function to disable tree-sitter based electric pair inhibition
(defun my/disable-treesit-electric-pair-inhibition ()
  "Disable tree-sitter based electric pair inhibition."
  (interactive)
  (setq-local electric-pair-inhibit-predicate #'electric-pair-default-inhibit)
  (message "Tree-sitter electric pair inhibition disabled"))

;; Utility function to show current context information
(defun my/treesit-show-context ()
  "Show tree-sitter context information at point."
  (interactive)
  (if (and (treesit-available-p) (treesit-parser-list))
      (let* ((node (treesit-node-at (point)))
             (node-type (when node (treesit-node-type node)))
             (parent (when node (treesit-node-parent node)))
             (parent-type (when parent (treesit-node-type parent)))
             (in-string-comment (my/treesit-in-string-or-comment-p)))
        (message "Node: %s | Parent: %s | In string/comment: %s"
                 (or node-type "nil")
                 (or parent-type "nil")
                 (if in-string-comment "YES" "NO")))
    (message "Tree-sitter not available or no parsers active")))

;; Hook to automatically enable for tree-sitter modes
(defun my/setup-treesit-electric-pair ()
  "Setup tree-sitter electric pair inhibition for current buffer."
  (when (and (treesit-available-p)
             (treesit-parser-list)
             electric-pair-mode)
    (my/enable-treesit-electric-pair-inhibition)))

;; Add to your existing language setup hooks
(add-hook 'python-ts-mode-hook #'my/setup-treesit-electric-pair)
(add-hook 'c-ts-mode-hook #'my/setup-treesit-electric-pair)
(add-hook 'c++-ts-mode-hook #'my/setup-treesit-electric-pair)
(add-hook 'json-ts-mode-hook #'my/setup-treesit-electric-pair)
(add-hook 'yaml-ts-mode-hook #'my/setup-treesit-electric-pair)
(add-hook 'css-ts-mode-hook #'my/setup-treesit-electric-pair)
(add-hook 'html-ts-mode-hook #'my/setup-treesit-electric-pair)
(add-hook 'bash-ts-mode-hook #'my/setup-treesit-electric-pair)
(add-hook 'javascript-ts-mode-hook #'my/setup-treesit-electric-pair)
(add-hook 'typescript-ts-mode-hook #'my/setup-treesit-electric-pair)
(add-hook 'rust-ts-mode-hook #'my/setup-treesit-electric-pair)

;; Keybindings (add to your existing treesit keybindings)
(global-set-key (kbd "C-c t c") #'my/treesit-show-context)
(global-set-key (kbd "C-c t e") #'my/enable-treesit-electric-pair-inhibition)
(global-set-key (kbd "C-c t E") #'my/disable-treesit-electric-pair-inhibition)
#+end_src

*** Combobulate
#+begin_src emacs-lisp
(use-package combobulate
  :when (treesit-available-p)
  :hook ((python-ts-mode
          js-ts-mode
          typescript-ts-mode
          tsx-ts-mode
          css-ts-mode
          yaml-ts-mode
          json-ts-mode) . combobulate-mode)
  :bind (:map combobulate-key-map
              ("C-c o u" . combobulate-splice-up)
              ("C-c o d" . combobulate-splice-down)
              ("C-c o c" . combobulate-clone-node-dwim)
              ("C-c o k" . combobulate-kill-node-dwim)
              ("C-c o f" . combobulate-navigate-next)
              ("C-c o b" . combobulate-navigate-previous))
  :load-path ("~/.config/emacs/lisp/combobulate"))
#+end_src

** LSP
*** Eglot
=Add eldoc setup separately=
#+begin_src emacs-lisp 
(use-package eglot
  :ensure nil
  :hook ((c-ts-mode c++-ts-mode python-ts-mode bash-ts-mode json-ts-mode yaml-mode) . eglot-ensure)
  :custom
  ;; Performance optimizations
  (eglot-events-buffer-size 0)  ; Disable event logging for performance
  (eglot-sync-connect nil)      ; Don't block on connection
  (eglot-connect-timeout 10)
  (eglot-autoshutdown t)        ; Shutdown server when last buffer is killed
  (eglot-send-changes-idle-time 0.1)
  
  ;; UI preferences
  (eglot-report-progress nil)   ; Don't spam minibuffer with progress
  (eglot-confirm-server-initiated-edits nil)
  
  :config
  ;; Enhanced server configurations
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode) . ("clangd"
                                            "--background-index"
                                            "--clang-tidy"
                                            "--completion-style=detailed"
                                            "--header-insertion=iwyu"
                                            "--header-insertion-decorators")))
  
  ;; Python language server configuration
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyright-langserver" "--stdio")))
  
  ;; Workspace configuration for better performance
  (defun eglot/configure-workspace ()
    "Configure workspace settings for better performance."
    (when (eglot-current-server)
      (eglot--signal-textDocument/didChangeConfiguration
       (eglot-current-server)
       (list :settings (eglot-workspace-configuration (eglot-current-server))))))
  
  (add-hook 'eglot-managed-mode-hook #'eglot/configure-workspace)
   
  ;; Doom Modeline integration
  (with-eval-after-load 'doom-modeline
    ;; Define custom eglot segment for doom-modeline
    (doom-modeline-def-segment eglot-status
      "Display eglot LSP server status."
      (when (and (bound-and-true-p eglot--managed-mode)
                 (eglot-current-server))
        (let* ((server (eglot-current-server))
               (nick (eglot-project-nickname server))
               (running-p (eglot-running-p server))
               (face (if running-p 'doom-modeline-lsp-success 'doom-modeline-lsp-error))
               (icon (if running-p
                        (doom-modeline-icon 'codicon "nf-cod-server" "◉" "◉" :face face)
                      (doom-modeline-icon 'codicon "nf-cod-server_error" "◌" "◌" :face face))))
          (concat
           (doom-modeline-display-icon icon)
           (propertize (format " %s" nick) 'face face)))))
    
    ;; Override existing LSP segment when eglot is active
    (defun doom-modeline--eglot-or-lsp ()
      "Show eglot status if active, otherwise show LSP status."
      (if (and (bound-and-true-p eglot--managed-mode)
               (eglot-current-server))
          (doom-modeline-segment--eglot-status)
        (doom-modeline-segment--lsp)))
    
    ;; Replace the default LSP segment
    (advice-add 'doom-modeline-segment--lsp :override #'doom-modeline--eglot-or-lsp)))
#+end_src

*** Eglot Booster
#+begin_src emacs-lisp
(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster"
       :branch "master")
  :after eglot
  :config
  (eglot-booster-mode 1))
#+end_src

** Eldoc
*** Core Configuration

#+begin_src emacs-lisp
(use-package eldoc
  :ensure nil
  :custom
  ;; Eldoc behavior
  (eldoc-idle-delay 0.2)
  (eldoc-print-after-edit t)
  (eldoc-echo-area-use-multiline-p 3)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  
  :config
  ;; Global eldoc mode
  (global-eldoc-mode 1)
  
  ;; Gruvbox dark theme faces
  (defface eldoc-highlight-function-argument
    '((t (:foreground "#fabd2f" :weight bold)))
    "Face for highlighting function arguments in eldoc."
    :group 'eldoc)
  
  ;; Hook to ensure eldoc works properly with eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local eldoc-documentation-functions
                          (cons #'eglot-signature-eldoc-function
                                (cons #'eglot-hover-eldoc-function
                                      (remove #'eglot-signature-eldoc-function
                                              (remove #'eglot-hover-eldoc-function
                                                      eldoc-documentation-functions))))))))
#+end_src

*** Eldoc Box

#+begin_src emacs-lisp
(use-package eldoc-box
  :after eldoc
  :custom
  ;; Box appearance
  (eldoc-box-max-pixel-width 800)
  (eldoc-box-max-pixel-height 400)
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-only-multi-line t)
  (eldoc-box-cleanup-interval 1.0)
  (eldoc-box-offset '(16 . 16))
  
  ;; Position and behavior
  (eldoc-box-position-function #'eldoc-box--default-upper-corner-position-function)
  (eldoc-box-fringe-use-same-bg t)
  
  :config
  ;; Gruvbox dark theme faces
  (defface eldoc-box-border
    '((t (:background "#504945")))
    "Face for eldoc-box border."
    :group 'eldoc-box)
  
  (defface eldoc-box-body
    '((t (:background "#32302f" :foreground "#ebdbb2")))
    "Face for eldoc-box body."
    :group 'eldoc-box)
  
  (defface eldoc-box-markdown-separator
    '((t (:foreground "#665c54" :height 1.0)))
    "Face for eldoc-box markdown separator."
    :group 'eldoc-box)
  
  ;; Custom eldoc-box frame parameters for gruvbox theme
  (setq eldoc-box-frame-parameters
        '((left . -1)
          (top . -1)
          (width . 0)
          (height . 0)
          (no-accept-focus . t)
          (no-focus-on-map . t)
          (min-width . 0)
          (min-height . 0)
          (internal-border-width . 2)
          (vertical-scroll-bars . nil)
          (horizontal-scroll-bars . nil)
          (left-fringe . 8)
          (right-fringe . 8)
          (menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (line-spacing . 0.1)
          (unsplittable . t)
          (undecorated . t)
          (visibility . nil)
          (mouse-wheel-frame . nil)
          (no-other-frame . t)
          (cursor-type . nil)
          (inhibit-double-buffering . t)
          (drag-internal-border . t)
          (no-special-glyphs . t)
          (background-color . "#32302f")
          (foreground-color . "#ebdbb2")))
  
  ;; Enable hover mode in programming modes
  (add-hook 'prog-mode-hook #'eldoc-box-hover-at-point-mode)
  
  ;; Keybindings
  :bind (("C-h ." . eldoc-box-help-at-point)))
#+end_src

** Flymake
*** Core Configuration

#+begin_src emacs-lisp
(use-package flymake
  :ensure nil
  :diminish flymake-mode
  :hook (eglot-managed-mode . flymake-mode)
  :custom
  ;; Performance optimizations
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t)
  (flymake-suppress-zero-counters t)
  (flymake-wrap-around nil)
  
  :config
  ;; Gruvbox dark theme faces
  (custom-set-faces
   '(flymake-error ((t (:underline (:color "#fb4934" :style wave)))))
   '(flymake-warning ((t (:underline (:color "#fabd2f" :style wave)))))
   '(flymake-note ((t (:underline (:color "#83a598" :style wave))))))
  
  ;; Eldoc integration for flymake diagnostics
  (defun flymake-eldoc-function (callback)
    "Eldoc function for flymake diagnostics."
    (when-let* ((diags (flymake-diagnostics (point)))
                (diag (car diags)))
      (funcall callback
               (format "[%s] %s"
                       (propertize
                        (pcase (flymake-diagnostic-type diag)
                          ('error "ERROR")
                          ('warning "WARN")
                          ('note "NOTE")
                          (_ "INFO"))
                        'face
                        (pcase (flymake-diagnostic-type diag)
                          ('error '(:foreground "#fb4934" :weight bold))
                          ('warning '(:foreground "#fabd2f" :weight bold))
                          ('note '(:foreground "#83a598" :weight bold))
                          (_ '(:foreground "#ebdbb2" :weight bold))))
                       (flymake-diagnostic-text diag)))))
  
  (add-hook 'flymake-mode-hook
            (lambda ()
              (add-hook 'eldoc-documentation-functions
                        #'flymake-eldoc-function nil t)))
  
  :bind (("C-c f n" . flymake-goto-next-error)
         ("C-c f p" . flymake-goto-prev-error)
         ("C-c f l" . flymake-show-buffer-diagnostics)
         ("C-c f d" . flymake-show-project-diagnostics)))
#+end_src

*** Doom Modeline Integration

#+begin_src emacs-lisp
(with-eval-after-load 'doom-modeline
  ;; Configure doom-modeline for detailed flymake display
  (setq doom-modeline-checker-simple-format nil)
  
  ;; Gruvbox faces for doom-modeline checker
  (custom-set-faces
   '(doom-modeline-checker-error ((t (:foreground "#fb4934" :weight bold))))
   '(doom-modeline-checker-warning ((t (:foreground "#fabd2f" :weight bold))))
   '(doom-modeline-checker-info ((t (:foreground "#83a598" :weight bold))))
   '(doom-modeline-checker-success ((t (:foreground "#b8bb26" :weight bold))))))
#+end_src

*** Flymake Diagnostics At Point

#+begin_src emacs-lisp
(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :custom
  (flymake-diagnostic-at-point-timer-delay 0.5)
  (flymake-diagnostic-at-point-error-prefix "➤ ")
  (flymake-diagnostic-at-point-warning-prefix "⚠ ")
  
  :config
  ;; Custom minibuffer display with gruvbox theming
  (defun flymake-diagnostic-at-point-display-minibuffer (text)
    "Display flymake diagnostic TEXT in minibuffer with gruvbox colors."
    (when (and text (stringp text) (> (length text) 0))
      (let* ((diags (flymake-diagnostics (point)))
             (diag (car diags))
             (diagnostic-type (when diag (flymake-diagnostic-type diag))))
        (message "%s"
                 (propertize (string-trim text)
                             'face
                             (pcase diagnostic-type
                               ('error '(:foreground "#fb4934" :background "#3c1f1e"))
                               ('warning '(:foreground "#fabd2f" :background "#473d29"))
                               ('note '(:foreground "#83a598" :background "#0d1011"))
                               (_ '(:foreground "#ebdbb2" :background "#32302f"))))))))
  
  (setq flymake-diagnostic-at-point-display-diagnostic-function
        'flymake-diagnostic-at-point-display-minibuffer))
#+end_src

** Dape
*** Core Setup
#+begin_src emacs-lisp
(use-package dape
  :config
  ;; Basic settings
  (setq dape-key-prefix "\C-x\C-a"
        dape-buffer-window-arrangement 'right
        dape-inlay-hints t
        dape-read-memory-default-count 32)

  ;; Enable eldoc integration
  (add-hook 'dape-mode-hook #'eldoc-mode)

  ;; Gruvbox dark theme faces
  (custom-set-faces
   '(dape-breakpoint-face
     ((t (:background "#cc241d" :foreground "#fbf1c7" :weight bold))))
   '(dape-exception-face
     ((t (:background "#fb4934" :foreground "#fbf1c7" :weight bold))))
   '(dape-source-line-stopped-face
     ((t (:background "#458588" :foreground "#fbf1c7" :weight bold))))
   '(dape-repl-prompt-face
     ((t (:foreground "#b8bb26" :weight bold))))
   '(dape-ui-locals-name
     ((t (:foreground "#8ec07c" :weight bold))))
   '(dape-ui-locals-value
     ((t (:foreground "#fe8019"))))
   '(dape-ui-locals-type
     ((t (:foreground "#d3869b"))))
   '(dape-ui-breakpoints-file
     ((t (:foreground "#83a598"))))
   '(dape-ui-breakpoints-line
     ((t (:foreground "#fabd2f"))))
   '(dape-ui-memory-address
     ((t (:foreground "#fabd2f"))))
   '(dape-ui-memory-value
     ((t (:foreground "#fe8019")))))

  ;; Flymake integration - disable during debugging
  (add-hook 'dape-mode-hook
            (lambda ()
              (when (bound-and-true-p flymake-mode)
                (setq-local dape--flymake-was-enabled t)
                (flymake-mode -1))))
  
  (add-hook 'dape-mode-exit-hook
            (lambda ()
              (when (and (boundp 'dape--flymake-was-enabled)
                         dape--flymake-was-enabled)
                (flymake-mode 1))))

  ;; Eldoc integration for debugging context
  (defun dape-eldoc-function (callback &rest _)
    "Provide debugging context to eldoc."
    (when (and (boundp 'dape--process) 
               dape--process 
               (dape--stopped-threads dape--process))
      (when-let ((info (ignore-errors (dape--variable-at-point))))
        (funcall callback (format "%s: %s" (car info) (cdr info))))))
  
  (add-hook 'dape-mode-hook
            (lambda ()
              (add-hook 'eldoc-documentation-functions #'dape-eldoc-function nil t)))

  ;; Corfu integration for REPL completion
  (with-eval-after-load 'corfu
    (add-hook 'dape-repl-mode-hook
              (lambda ()
                (setq-local corfu-auto t)
                (corfu-mode 1))))

  ;; Cape integration for enhanced REPL completion
  (with-eval-after-load 'cape
    (add-hook 'dape-repl-mode-hook
              (lambda ()
                (setq-local completion-at-point-functions
                           (list (cape-capf-super
                                  #'dape-repl-completion-at-point
                                  #'cape-file))))))

  ;; Tree-sitter integration for function detection
  (with-eval-after-load 'treesit
    (defun dape-treesit-current-function ()
      "Get current function name using tree-sitter."
      (when (treesit-parser-list)
        (let ((node (treesit-node-at (point))))
          (while (and node
                      (not (member (treesit-node-type node)
                                   '("function_definition" "method_definition"
                                     "function_declaration" "method_declaration"))))
            (setq node (treesit-node-parent node)))
          (when node
            (let ((name-node (treesit-node-child-by-field-name node "name")))
              (when name-node
                (treesit-node-text name-node)))))))
    
    (add-hook 'dape-mode-hook
              (lambda ()
                (when (treesit-parser-list)
                  (setq-local which-func-functions '(dape-treesit-current-function))))))

  ;; Project integration for workspace detection
  (with-eval-after-load 'project
    (defun dape-cwd ()
      "Get current working directory for debugging."
      (if-let ((project (project-current)))
          (project-root project)
        default-directory)))

  ;; ;; Language Configurations
  ;; Python debugging with debugpy
  (add-to-list 'dape-configs
               `(debugpy
                 modes (python-mode python-ts-mode)
                 command "python"
                 command-args ("-m" "debugpy.adapter")
                 :request "launch"
                 :type "executable"
                 :cwd dape-cwd
                 :program dape-find-file-buffer-default
                 :console "integratedTerminal"
                 :justMyCode :json-false
                 :showReturnValue t))

  ;; C/C++ debugging with GDB
  (add-to-list 'dape-configs
               `(gdb
                 modes (c-mode c-ts-mode c++-mode c++-ts-mode)
                 command "gdb"
                 command-args ("--interpreter=dap")
                 :request "launch"
                 :type "gdb"
                 :cwd dape-cwd
                 :program (lambda () 
                           (expand-file-name
                            (read-file-name "Program: " 
                                          (or (dape-cwd) default-directory))))
                 :args []))

  ;; UI and Window Management
  ;; Enhanced variable display formatting
  (setq dape-ui-variable-format-function
        (lambda (name value type)
          (format "%s %s = %s"
                  (propertize type 'face 'dape-ui-locals-type)
                  (propertize name 'face 'dape-ui-locals-name)
                  (propertize value 'face 'dape-ui-locals-value))))

  ;; Auto-open debug info window
  (add-hook 'dape-start-hook
            (lambda ()
              (save-selected-window (dape-info))))

  ;; Cleanup debug buffers on termination
  (add-hook 'dape-termination-hook
            (lambda ()
              (dolist (buffer-name '("*dape-info*" "*dape-repl*"))
                (when-let ((buffer (get-buffer buffer-name)))
                  (kill-buffer buffer)))))

  ;; Keybindings
  :bind (;; Core debugging commands
         ("C-x C-a C-d" . dape)
         ("C-x C-a C-k" . dape-kill)
         ("C-x C-a C-q" . dape-quit)
         ("C-x C-a C-R" . dape-restart)
         
         ;; Breakpoint management
         ("C-x C-a C-b" . dape-breakpoint-toggle)
         ("C-x C-a C-l" . dape-breakpoint-log)
         ("C-x C-a C-e" . dape-breakpoint-expression)
         ("C-x C-a C-r" . dape-breakpoint-remove-all)
         
         ;; Stepping commands
         ("C-x C-a C-n" . dape-next)
         ("C-x C-a C-s" . dape-step-in)
         ("C-x C-a C-o" . dape-step-out)
         ("C-x C-a C-c" . dape-continue)
         ("C-x C-a C-p" . dape-pause)
         
         ;; Stack navigation
         ("C-x C-a C-u" . dape-stack-select-up)
         ("C-x C-a C-D" . dape-stack-select-down)
         
         ;; Information and utilities
         ("C-x C-a C-i" . dape-info)
         ("C-x C-a C-S" . dape-repl)
         ("C-x C-a C-w" . dape-watch-dwim)
         ("C-x C-a C-m" . dape-read-memory)))
#+end_src

*** Helper Functions
#+begin_src emacs-lisp
;; Helper Functions
(defun dape-python-with-args ()
  "Debug current Python script with custom arguments."
  (interactive)
  (let* ((args (split-string (read-string "Arguments: ") " " t))
         (dape-configs 
          (cons `(debugpy-args
                  modes (python-mode python-ts-mode)
                  command "python"
                  command-args ("-m" "debugpy.adapter")
                  :request "launch"
                  :type "executable"
                  :cwd ,(or (dape-cwd) default-directory)
                  :program ,(buffer-file-name)
                  :args ,args
                  :console "integratedTerminal"
                  :justMyCode :json-false
                  :showReturnValue t)
                dape-configs)))
    (dape 'debugpy-args)))

(defun dape-gdb-with-args ()
  "Debug C/C++ executable with custom arguments."
  (interactive)
  (let* ((executable (read-file-name "Executable: " 
                                    (or (dape-cwd) default-directory)))
         (args (split-string (read-string "Arguments: ") " " t))
         (dape-configs 
          (cons `(gdb-args
                  modes (c-mode c-ts-mode c++-mode c++-ts-mode)
                  command "gdb"
                  command-args ("--interpreter=dap")
                  :request "launch"
                  :type "gdb"
                  :cwd ,(or (dape-cwd) default-directory)
                  :program ,executable
                  :args ,args)
                dape-configs)))
    (dape 'gdb-args)))

;; Bind helper functions
(with-eval-after-load 'dape
  (define-key dape-mode-map (kbd "C-x C-a C-A") #'dape-python-with-args)
  (define-key dape-mode-map (kbd "C-x C-a C-G") #'dape-gdb-with-args))
#+end_src
** Formatting
*** Core Configuration

#+begin_src emacs-lisp
(use-package apheleia
  :hook ((prog-mode . apheleia-mode)
         (text-mode . apheleia-mode)
         (conf-mode . apheleia-mode))
  :custom
  ;; Performance and behavior
  (apheleia-remote-algorithm 'cancel)
  (apheleia-log-only-errors t)
  (apheleia-hide-log-buffers t)
  (apheleia-max-alignment-size 1000)
  (apheleia-formatters-respect-indent-level t)
  (apheleia-mode-lighter " Φ")
  
  :config
  ;; Enhanced formatter configurations
  (setq apheleia-formatters
        (append apheleia-formatters
                `((clang-format . ("clang-format"
                                   "--style={BasedOnStyle: LLVM, IndentWidth: 4, ColumnLimit: 100, AllowShortFunctionsOnASingleLine: Empty, BreakBeforeBraces: Attach, IndentCaseLabels: true, AlignTrailingComments: true, SpaceBeforeParens: ControlStatements}"
                                   "--assume-filename" ,(lambda () (or (buffer-file-name) "stdin"))))
                  
                  (black . ("black" "--quiet" "--line-length" "88" "--stdin-filename"
                           ,(lambda () (or (buffer-file-name) "stdin")) "-"))
                  
                  (isort . ("isort" "--quiet" "--stdout" "--profile" "black"
                           "--filename" ,(lambda () (or (buffer-file-name) "stdin")) "-"))
                  
                  (shfmt . ("shfmt" "-i" "2" "-ci"))
                  
                  ;; Add ruff formatter for Python if available
                  ,@(when (executable-find "ruff")
                      `((ruff . ("ruff" "format" "--stdin-filename" 
                                ,(lambda () (or (buffer-file-name) "stdin")) "-"))))
                  
                  ;; Prettier for various web formats
                  ,@(when (executable-find "prettier")
                      `((prettier-yaml . ("prettier" "--parser" "yaml" "--stdin-filepath"
                                         ,(lambda () (or (buffer-file-name) "stdin"))))
                        (prettier-json . ("prettier" "--parser" "json" "--stdin-filepath"
                                         ,(lambda () (or (buffer-file-name) "stdin"))))
                        (prettier-css . ("prettier" "--parser" "css" "--stdin-filepath"
                                        ,(lambda () (or (buffer-file-name) "stdin"))))
                        (prettier-html . ("prettier" "--parser" "html" "--stdin-filepath"
                                         ,(lambda () (or (buffer-file-name) "stdin"))))))
                  
                  ;; SQL formatter if available
                  ,@(when (executable-find "sqlformat")
                      `((sqlformat . ("sqlformat" "--reindent" "--keywords" "upper" 
                                     "--identifiers" "lower" "-"))))
                  
                  ;; LaTeX formatter if available
                  ,@(when (executable-find "latexindent")
                      `((latexindent . ("latexindent" "--local" "--silent")))))))
  
  ;; Mode associations with intelligent formatter selection
  (setq apheleia-mode-alist
        (append apheleia-mode-alist
                `((c-ts-mode . clang-format)
                  (c++-ts-mode . clang-format)
                  (c-mode . clang-format)
                  (c++-mode . clang-format)
                  
                  ;; Python formatting - use ruff if available, otherwise black + isort
                  ,@(if (executable-find "ruff")
                        `((python-ts-mode . ruff)
                          (python-mode . ruff))
                      `((python-ts-mode . (isort black))
                        (python-mode . (isort black))))
                  
                  (bash-ts-mode . shfmt)
                  (sh-mode . shfmt)
                  
                  ;; Web formats with prettier
                  ,@(when (executable-find "prettier")
                      `((yaml-mode . prettier-yaml)
                        (yaml-ts-mode . prettier-yaml)
                        (json-mode . prettier-json)
                        (json-ts-mode . prettier-json)
                        (css-mode . prettier-css)
                        (css-ts-mode . prettier-css)
                        (html-mode . prettier-html)
                        (web-mode . prettier-html)))
                  
                  ;; SQL formatting
                  ,@(when (executable-find "sqlformat")
                      `((sql-mode . sqlformat)))
                  
                  ;; LaTeX formatting
                  ,@(when (executable-find "latexindent")
                      `((latex-mode . latexindent)
                        (LaTeX-mode . latexindent))))))
  
  ;; Enhanced save hook with better error handling
  (defun apheleia-format-on-save-safe ()
    "Safe format on save that handles errors gracefully."
    (when (and apheleia-mode
               (buffer-modified-p)
               (not buffer-read-only)
               (not (bound-and-true-p apheleia-temporarily-disabled)))
      (condition-case err
          (apheleia-format-buffer)
        (error
         (message "Apheleia formatting failed: %s" (error-message-string err))
         ;; Don't prevent saving due to formatting errors
         nil))))
  
  ;; Replace default save hook with safer version
  (remove-hook 'before-save-hook #'apheleia-format-buffer)
  (add-hook 'before-save-hook #'apheleia-format-on-save-safe)
  
  :bind (("C-c a f" . apheleia-format-buffer)
         ("C-c a t" . apheleia-mode)
         ("C-c a r" . apheleia-restart)))
#+end_src

*** Project-Specific Configuration
#+begin_src emacs-lisp
(with-eval-after-load 'apheleia
  (defvar apheleia-project-formatters-alist
    '(;; Python projects with ruff configuration
      (("pyproject.toml" "ruff.toml" ".ruff.toml")
       (python-ts-mode . ruff)
       (python-mode . ruff))
      
      ;; Python projects with black configuration
      (("pyproject.toml" "setup.cfg" ".black")
       (python-ts-mode . (isort black))
       (python-mode . (isort black)))
      
      ;; C/C++ projects with clang-format config
      ((".clang-format" "_clang-format")
       (c-ts-mode . clang-format)
       (c++-ts-mode . clang-format)
       (c-mode . clang-format)
       (c++-mode . clang-format))
      
      ;; Web projects with prettier config
      ((".prettierrc" ".prettierrc.json" ".prettierrc.js" "prettier.config.js")
       (json-mode . prettier-json)
       (yaml-mode . prettier-yaml)
       (css-mode . prettier-css)
       (html-mode . prettier-html)))
    "Alist of project root patterns to formatter configurations.")
  
  (defun apheleia-find-project-root ()
    "Find the project root directory."
    (or (when (fboundp 'project-root)
          (when-let ((project (project-current)))
            (project-root project)))
        (locate-dominating-file default-directory ".git")
        (locate-dominating-file default-directory ".projectile")
        (locate-dominating-file default-directory "pyproject.toml")
        (locate-dominating-file default-directory "package.json")
        default-directory))
  
  (defun apheleia-formatter-available-p (formatter)
    "Check if FORMATTER is available on the system."
    (cond
     ((symbolp formatter)
      (let ((formatter-def (alist-get formatter apheleia-formatters)))
        (cond
         ((functionp formatter-def) t) ; Function formatters are always available
         ((listp formatter-def)
          (let ((command (car formatter-def)))
            (if (stringp command)
                (executable-find command)
              t)))
         (t nil))))
     ((listp formatter)
      (cl-every #'apheleia-formatter-available-p formatter))
     (t t)))
  
  (defun apheleia-detect-project-formatters ()
    "Detect project-specific formatters based on project files."
    (when-let* ((project-root (apheleia-find-project-root))
                (config (cl-find-if
                         (lambda (entry)
                           (let ((patterns (car entry)))
                             (cl-some (lambda (pattern)
                                        (file-exists-p (expand-file-name pattern project-root)))
                                      patterns)))
                         apheleia-project-formatters-alist)))
      (let ((formatters (cdr config)))
        (when-let ((formatter (alist-get major-mode formatters)))
          ;; Check if formatter is available before setting it
          (when (apheleia-formatter-available-p formatter)
            (setq-local apheleia-mode-alist
                        (cons (cons major-mode formatter)
                              (cl-remove major-mode apheleia-mode-alist :key #'car))))))))
  
  (add-hook 'apheleia-mode-hook #'apheleia-detect-project-formatters))
#+end_src

*** Enhanced Integration and Utilities
#+begin_src emacs-lisp
(with-eval-after-load 'apheleia
  ;; Function to check formatter availability
  (defun apheleia-check-formatters ()
    "Check which formatters are available on the system."
    (interactive)
    (let ((available-formatters '())
          (missing-formatters '()))
      (dolist (formatter apheleia-formatters)
        (let ((name (car formatter))
              (definition (cdr formatter)))
          (if (apheleia-formatter-available-p name)
              (push name available-formatters)
            (push name missing-formatters))))
      (with-help-window "*Apheleia Formatters*"
        (princ "Available formatters:\n")
        (dolist (formatter (sort available-formatters #'string<))
          (princ (format "  ✓ %s\n" formatter)))
        (when missing-formatters
          (princ "\nMissing formatters:\n")
          (dolist (formatter (sort missing-formatters #'string<))
            (princ (format "  ✗ %s\n" formatter)))))))
  
  ;; Function to show current formatter for buffer
  (defun apheleia-show-current-formatter ()
    "Show the current formatter(s) for this buffer."
    (interactive)
    (if (bound-and-true-p apheleia-mode)
        (condition-case nil
            (let ((formatters (apheleia--get-formatters)))
              (if formatters
                  (message "Current formatter(s): %s"
                           (mapconcat (lambda (f) 
                                        (if (symbolp f) (symbol-name f) (format "%s" f)))
                                      formatters ", "))
                (message "No formatter configured for %s" major-mode)))
          (error (message "Error getting formatter information")))
      (message "Apheleia mode not enabled")))
  
  ;; Function to temporarily disable formatting
  (defvar-local apheleia-temporarily-disabled nil
    "Whether apheleia is temporarily disabled for this buffer.")
  
  (defun apheleia-toggle-temporary-disable ()
    "Temporarily disable/enable apheleia for current buffer."
    (interactive)
    (setq apheleia-temporarily-disabled (not apheleia-temporarily-disabled))
    (message "Apheleia %s for this buffer"
             (if apheleia-temporarily-disabled "disabled" "enabled")))
  
  ;; Success message display
  (defun apheleia-show-success-message ()
    "Show success message after formatting."
    (condition-case nil
        (let ((formatters (apheleia--get-formatters)))
          (when formatters
            (let ((formatter-names (mapcar (lambda (f) 
                                             (if (symbolp f) (symbol-name f) (format "%s" f)))
                                           formatters)))
              (message "✓ Formatted with %s" 
                       (string-join formatter-names ", ")))))
      (error nil)))
  
  ;; Hook for successful formatting
  (add-hook 'apheleia-post-format-hook #'apheleia-show-success-message)
  
  ;; Enhanced Eglot integration
  (defun apheleia-eglot-integration ()
    "Ensure apheleia coordinates with eglot."
    (when (and apheleia-mode
               (bound-and-true-p eglot--managed-mode))
      ;; Add hook to trigger diagnostics refresh after formatting
      (add-hook 'apheleia-post-format-hook
                (lambda () 
                  (when (and (buffer-live-p (current-buffer))
                             (bound-and-true-p eglot--managed-mode))
                    (run-with-timer 0.5 nil 
                                    (lambda ()
                                      (when (buffer-live-p (current-buffer))
                                        (eglot-code-actions nil nil 'refresh-only))))))
                nil t)))
  
  (add-hook 'eglot-managed-mode-hook #'apheleia-eglot-integration)
  
  ;; Additional keybindings
  (when (bound-and-true-p apheleia-mode-map)
    (define-key apheleia-mode-map (kbd "C-c a c") #'apheleia-check-formatters)
    (define-key apheleia-mode-map (kbd "C-c a s") #'apheleia-show-current-formatter)
    (define-key apheleia-mode-map (kbd "C-c a d") #'apheleia-toggle-temporary-disable)))
#+end_src

*** Performance Optimizations
#+begin_src emacs-lisp
(with-eval-after-load 'apheleia
  ;; Cache formatter availability to avoid repeated executable-find calls
  (defvar apheleia-formatter-cache (make-hash-table :test 'eq)
    "Cache for formatter availability.")
  
  (defvar apheleia-cache-expiry-time 300  ; 5 minutes
    "Time in seconds after which to expire formatter cache.")
  
  (defvar apheleia-cache-timestamp (make-hash-table :test 'eq)
    "Timestamps for cache entries.")
  
  (defun apheleia-formatter-available-cached-p (formatter)
    "Check if FORMATTER is available, using cache with expiry."
    (let* ((current-time (float-time))
           (cached-time (gethash formatter apheleia-cache-timestamp 0))
           (cached-result (gethash formatter apheleia-formatter-cache 'not-found)))
      (if (and (not (eq cached-result 'not-found))
               (< (- current-time cached-time) apheleia-cache-expiry-time))
          cached-result
        (let ((available (apheleia-formatter-available-p formatter)))
          (puthash formatter available apheleia-formatter-cache)
          (puthash formatter current-time apheleia-cache-timestamp)
          available))))
  
  ;; Clear cache when formatters are modified
  (defun apheleia-clear-formatter-cache ()
    "Clear the formatter availability cache."
    (interactive)
    (clrhash apheleia-formatter-cache)
    (clrhash apheleia-cache-timestamp)
    (message "Apheleia formatter cache cleared"))
  
  ;; Advice to use cached version for better performance
  (advice-add 'apheleia-formatter-available-p :override #'apheleia-formatter-available-cached-p))
#+end_src

*** Optional Modeline Integration
#+begin_src emacs-lisp
(with-eval-after-load 'apheleia
  (defun apheleia-modeline-segment ()
    "Return apheleia status for modeline."
    (when (bound-and-true-p apheleia-mode)
      (let ((formatters (ignore-errors (apheleia--get-formatters))))
        (if formatters
            (propertize " Φ" 
                        'face '(:foreground "#b8bb26" :weight bold)
                        'help-echo (format "Apheleia: %s" 
                                          (mapconcat #'symbol-name formatters ", ")))
          (propertize " Φ" 
                      'face '(:foreground "#928374")
                      'help-echo "Apheleia: no formatter")))))
  
  ;; Function to add to your modeline format if desired
  (defun apheleia-enable-modeline ()
    "Add apheleia status to modeline."
    (interactive)
    (unless (member '(:eval (apheleia-modeline-segment)) mode-line-format)
      (setq mode-line-format
            (append mode-line-format '((:eval (apheleia-modeline-segment)))))))
  
  ;; Uncomment to enable modeline integration automatically
  ;; (add-hook 'apheleia-mode-hook #'apheleia-enable-modeline)
  )
#+end_src

** Code Folding
*** Hideshow
Emacs' built-in code folding system with enhanced overlay display.
#+begin_src emacs-lisp
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :custom
  (hs-hide-comments-when-hiding-all t)
  (hs-allow-nesting t)
  
  :config
  ;; Gruvbox dark theme faces
  (custom-set-faces
   '(hs-face ((t (:background "#504945" :foreground "#a89984" 
                  :box (:line-width 1 :color "#665c54")))))
   '(hs-fringe-face ((t (:foreground "#fb4934")))))
  
  ;; Enhanced overlay display with line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts for folded regions."
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " ... (%d lines)" nlines)))
        (overlay-put ov 'display (propertize info 'face 'hs-face)))))
  
  (setq hs-set-up-overlay 'hs-display-code-line-counts)
  
  :bind (("C-c h t" . hs-toggle-hiding)
         ("C-c h h" . hs-hide-block)
         ("C-c h s" . hs-show-block)
         ("C-c h H" . hs-hide-all)
         ("C-c h S" . hs-show-all)))
#+end_src

*** Treesit-fold Integration
Modern tree-sitter based folding with enhanced summaries.

#+begin_src emacs-lisp
(use-package treesit-fold
  :after treesit
  :hook ((c-ts-mode c++-ts-mode python-ts-mode json-ts-mode 
          bash-ts-mode typescript-ts-mode) . treesit-fold-mode)
  :custom
  (treesit-fold-summary-show t)
  (treesit-fold-replacement "...")
  
  :config
  ;; Gruvbox dark theme faces
  (custom-set-faces
   '(treesit-fold-replacement-face 
     ((t (:foreground "#a89984" :background "#504945" 
          :box (:line-width 1 :color "#665c54") :weight bold))))
   '(treesit-fold-fringe-face ((t (:foreground "#fb4934")))))
  
  ;; Enhanced summary with line counts
  (defun treesit-fold-summary-with-count (range)
    "Show summary with line count for folded region."
    (let* ((start (car range))
           (end (cdr range))
           (lines (count-lines start end))
           (summary (treesit-fold--get-summary start end)))
      (format "%s (%d lines)" (or summary "...") lines)))
  
  (setq treesit-fold-summary-function #'treesit-fold-summary-with-count)
  
  :bind (("C-c f t" . treesit-fold-toggle)
         ("C-c f h" . treesit-fold-close)
         ("C-c f s" . treesit-fold-open)
         ("C-c f H" . treesit-fold-close-all)
         ("C-c f S" . treesit-fold-open-all)))
#+end_src

*** LSP Integration
Integration with Language Server Protocol folding ranges via eglot.
#+begin_src emacs-lisp
(defun lsp-folding-range-support-p ()
  "Check if current LSP server supports folding ranges."
  (and (bound-and-true-p eglot--managed-mode)
       (eglot-current-server)
       (eglot--server-capable :foldingRangeProvider)))

(defun lsp-get-folding-ranges ()
  "Get folding ranges from LSP server."
  (when (lsp-folding-range-support-p)
    (condition-case nil
        (jsonrpc-request (eglot-current-server)
                         :textDocument/foldingRange
                         (list :textDocument (eglot--TextDocumentIdentifier)))
      (error nil))))

(defun lsp-apply-folding-ranges ()
  "Apply folding ranges from LSP server using hideshow."
  (interactive)
  (when-let ((ranges (lsp-get-folding-ranges)))
    (save-excursion
      (dolist (range ranges)
        (let* ((start-line (1+ (gethash "startLine" range)))
               (end-line (1+ (gethash "endLine" range))))
          (goto-char (point-min))
          (forward-line (1- start-line))
          (let ((start-pos (point)))
            (forward-line (- end-line start-line))
            (when (> (count-lines start-pos (point)) 1)
              (hs-hide-block-at-point start-pos))))))))

(defun hs-hide-block-at-point (pos)
  "Hide block at specific position."
  (save-excursion
    (goto-char pos)
    (when (hs-find-block-beginning)
      (hs-hide-block))))

;; Add LSP folding keybinding when eglot is active
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (lsp-folding-range-support-p)
                (local-set-key (kbd "C-c e f") #'lsp-apply-folding-ranges)))))
#+end_src

*** Visual Enhancements
Custom fringe bitmaps for fold indicators with Gruvbox theming.
#+begin_src emacs-lisp
(use-package fringe
  :ensure nil
  :config
  ;; Gruvbox dark theme fringe
  (custom-set-faces
   '(fringe ((t (:background "#282828" :foreground "#a89984")))))
  
  ;; Enhanced fold indicators
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'fold-indicator
      [#b11111111
       #b10000001
       #b10000001
       #b10000001
       #b10000001
       #b10000001
       #b10000001
       #b11111111]
      nil 8 'center)
    
    (define-fringe-bitmap 'unfold-indicator
      [#b11111111
       #b10000001
       #b10111101
       #b10111101
       #b10111101
       #b10111101
       #b10000001
       #b11111111]
      nil 8 'center))
  
  ;; Set fringe width
  (fringe-mode '(8 . 8)))
#+end_src

*** Unified Interface
Unified commands that automatically choose the best folding method available.
#+begin_src emacs-lisp
(defun unified-fold-toggle ()
  "Toggle folding using the best available method."
  (interactive)
  (cond
   ;; Prefer treesit-fold for tree-sitter modes
   ((and (bound-and-true-p treesit-fold-mode)
         (fboundp 'treesit-fold-toggle))
    (treesit-fold-toggle))
   ;; Fall back to hideshow
   ((bound-and-true-p hs-minor-mode)
    (hs-toggle-hiding))
   (t (message "No folding available"))))

(defun unified-fold-hide-all ()
  "Hide all folds using the best available method."
  (interactive)
  (cond
   ((and (bound-and-true-p treesit-fold-mode)
         (fboundp 'treesit-fold-close-all))
    (treesit-fold-close-all))
   ((bound-and-true-p hs-minor-mode)
    (hs-hide-all))
   (t (message "No folding available"))))

(defun unified-fold-show-all ()
  "Show all folds using the best available method."
  (interactive)
  (cond
   ((and (bound-and-true-p treesit-fold-mode)
         (fboundp 'treesit-fold-open-all))
    (treesit-fold-open-all))
   ((bound-and-true-p hs-minor-mode)
    (hs-show-all))
   (t (message "No folding available"))))

;; Global unified keybindings
(global-set-key (kbd "C-c TAB") #'unified-fold-toggle)
(global-set-key (kbd "C-c C-h") #'unified-fold-hide-all)
(global-set-key (kbd "C-c C-s") #'unified-fold-show-all)
#+end_src

*** State Management
#+begin_src emacs-lisp
(defvar-local folding-state-cache nil
  "Cache for folding state to avoid expensive calculations.")

(defvar folding-state-update-timer nil
  "Timer for updating folding state.")

(defun folding-count-folds ()
  "Count total and active folds in current buffer."
  (let ((total-folds 0)
        (active-folds 0))
    ;; Count hideshow folds
    (when (bound-and-true-p hs-minor-mode)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (hs-overlay-at (point))
            (setq total-folds (1+ total-folds))
            (when (overlay-get (car (hs-overlay-at (point))) 'invisible)
              (setq active-folds (1+ active-folds))))
          (forward-line 1))))
    
    ;; Count treesit-fold folds
    (when (bound-and-true-p treesit-fold-mode)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'treesit-fold)
          (setq total-folds (1+ total-folds))
          (when (overlay-get ov 'invisible)
            (setq active-folds (1+ active-folds))))))
    
    (cons active-folds total-folds)))

(defun folding-update-state-cache ()
  "Update folding state cache."
  (when (or (bound-and-true-p hs-minor-mode)
            (bound-and-true-p treesit-fold-mode))
    (setq folding-state-cache (folding-count-folds))))

(defun folding-schedule-update ()
  "Schedule folding state update."
  (when folding-state-update-timer
    (cancel-timer folding-state-update-timer))
  (setq folding-state-update-timer
        (run-with-idle-timer 0.5 nil #'folding-update-state-cache)))

;; Update cache on fold changes
(advice-add 'hs-toggle-hiding :after (lambda (&rest _) (folding-schedule-update)))
(advice-add 'hs-hide-all :after (lambda (&rest _) (folding-schedule-update)))
(advice-add 'hs-show-all :after (lambda (&rest _) (folding-schedule-update)))

(when (fboundp 'treesit-fold-toggle)
  (advice-add 'treesit-fold-toggle :after (lambda (&rest _) (folding-schedule-update)))
  (advice-add 'treesit-fold-close-all :after (lambda (&rest _) (folding-schedule-update)))
  (advice-add 'treesit-fold-open-all :after (lambda (&rest _) (folding-schedule-update))))
#+end_src

*** Doom Modeline Integration
Custom doom-modeline segment to display folding status.
#+begin_src emacs-lisp
(with-eval-after-load 'doom-modeline
  ;; Define folding segment
  (doom-modeline-def-segment folding
    "Folding state indicator for doom-modeline."
    (when (and (or (bound-and-true-p hs-minor-mode)
                   (bound-and-true-p treesit-fold-mode))
               folding-state-cache)
      (let* ((active (car folding-state-cache))
             (total (cdr folding-state-cache))
             (icon (if (> active 0) "▼" "▶"))
             (face (if (> active 0) 'doom-modeline-info 'doom-modeline-buffer-minor-mode)))
        (concat
         (doom-modeline-spc)
         (propertize icon 'face face)
         (when (> total 0)
           (propertize (format "%d/%d" active total) 'face face))))))

  ;; Custom doom modeline with folding (fixed checker segment)
  (doom-modeline-def-modeline 'folding-modeline
    '(bar workspace-name window-number modals matches follow buffer-info 
      remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus 
      github debug repl lsp minor-modes folding input-method indent-info 
      buffer-encoding major-mode process vcs time))

  ;; Function to toggle folding modeline
  (defun toggle-folding-modeline ()
    "Toggle between default and folding-enabled doom modeline."
    (interactive)
    (if (eq doom-modeline-current-modeline 'folding-modeline)
        (doom-modeline-set-modeline 'main)
      (doom-modeline-set-modeline 'folding-modeline))
    (force-mode-line-update))

  ;; Auto-enable folding modeline in programming modes
  (defun maybe-enable-folding-modeline ()
    "Enable folding modeline in programming buffers."
    (when (and (derived-mode-p 'prog-mode)
               (or (bound-and-true-p hs-minor-mode)
                   (bound-and-true-p treesit-fold-mode)))
      (doom-modeline-set-modeline 'folding-modeline)
      (folding-update-state-cache)))

  (add-hook 'prog-mode-hook #'maybe-enable-folding-modeline)
  (add-hook 'hs-minor-mode-hook #'maybe-enable-folding-modeline)
  (add-hook 'treesit-fold-mode-hook #'maybe-enable-folding-modeline)

  ;; Keybinding to toggle folding modeline
  (global-set-key (kbd "C-c m f") #'toggle-folding-modeline))
#+end_src

*** State Persistence
Persistent folding states across Emacs sessions.
#+begin_src emacs-lisp
(defvar folding-save-file (expand-file-name "folding-states.el" user-emacs-directory)
  "File to save folding states.")

(defun folding-save-state ()
  "Save current buffer's folding state."
  (interactive)
  (when (and buffer-file-name
             (or (bound-and-true-p hs-minor-mode)
                 (bound-and-true-p treesit-fold-mode)))
    (let ((states '())
          (file buffer-file-name))
      ;; Collect hideshow states
      (when (bound-and-true-p hs-minor-mode)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when-let ((ov (car (hs-overlay-at (point)))))
              (when (overlay-get ov 'invisible)
                (push (list 'hs (overlay-start ov) (overlay-end ov)) states)))
            (forward-line 1))))
      
      ;; Collect treesit-fold states
      (when (bound-and-true-p treesit-fold-mode)
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (and (overlay-get ov 'treesit-fold)
                     (overlay-get ov 'invisible))
            (push (list 'treesit (overlay-start ov) (overlay-end ov)) states))))
      
      ;; Save to file
      (when states
        (let ((all-states (if (file-exists-p folding-save-file)
                              (with-temp-buffer
                                (insert-file-contents folding-save-file)
                                (condition-case nil
                                    (read (current-buffer))
                                  (error '())))
                            '())))
          (setf (alist-get file all-states nil nil #'string=) states)
          (with-temp-file folding-save-file
            (prin1 all-states (current-buffer))))))))

(defun folding-restore-state ()
  "Restore folding state for current buffer."
  (interactive)
  (when (and buffer-file-name
             (file-exists-p folding-save-file))
    (condition-case nil
        (let* ((all-states (with-temp-buffer
                             (insert-file-contents folding-save-file)
                             (read (current-buffer))))
               (states (alist-get buffer-file-name all-states nil nil #'string=)))
          (dolist (state states)
            (let ((type (car state))
                  (start (cadr state))
                  (end (caddr state)))
              (when (and (>= start (point-min)) (<= end (point-max)))
                (cond
                 ((eq type 'hs)
                  (when (bound-and-true-p hs-minor-mode)
                    (save-excursion
                      (goto-char start)
                      (hs-hide-block))))
                 ((eq type 'treesit)
                  (when (bound-and-true-p treesit-fold-mode)
                    (save-excursion
                      (goto-char start)
                      (treesit-fold-close)))))))))
      (error (message "Failed to restore folding state")))))

;; Auto-save/restore hooks
(add-hook 'kill-buffer-hook #'folding-save-state)
(add-hook 'find-file-hook 
          (lambda () 
            (run-with-idle-timer 1 nil #'folding-restore-state)))

;; Manual save/restore keybindings
(global-set-key (kbd "C-c f w") #'folding-save-state)
(global-set-key (kbd "C-c f r") #'folding-restore-state)
#+end_src

*** Additional Conveniences
Extra convenience functions for folding management.
#+begin_src emacs-lisp
(defun folding-status ()
  "Show current folding status in minibuffer."
  (interactive)
  (if-let ((cache folding-state-cache))
      (message "Folding: %d/%d active" (car cache) (cdr cache))
    (message "No folding information available")))

(global-set-key (kbd "C-c f i") #'folding-status)

;; Initialize folding state on startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer 2 nil
                                 (lambda ()
                                   (dolist (buf (buffer-list))
                                     (with-current-buffer buf
                                       (when (derived-mode-p 'prog-mode)
                                         (folding-update-state-cache))))))))

(provide 'code-folding)
;;; code-folding.el ends here
#+end_src

*** Keybinding Reference
**** Hideshow Bindings
| Keybinding | Function           | Description              |
|------------|--------------------|--------------------------|
| =C-c h t=  | hs-toggle-hiding   | Toggle current fold      |
| =C-c h h=  | hs-hide-block      | Hide current block       |
| =C-c h s=  | hs-show-block      | Show current block       |
| =C-c h H=  | hs-hide-all        | Hide all blocks          |
| =C-c h S=  | hs-show-all        | Show all blocks          |

**** Treesit-fold Bindings
| Keybinding | Function              | Description              |
|------------|-----------------------|--------------------------|
| =C-c f t=  | treesit-fold-toggle   | Toggle current fold      |
| =C-c f h=  | treesit-fold-close    | Close current fold       |
| =C-c f s=  | treesit-fold-open     | Open current fold        |
| =C-c f H=  | treesit-fold-close-all| Close all folds          |
| =C-c f S=  | treesit-fold-open-all | Open all folds           |

**** Unified Interface
| Keybinding   | Function              | Description              |
|--------------|-----------------------|--------------------------|
| =C-c TAB=    | unified-fold-toggle   | Smart fold toggle        |
| =C-c C-h=    | unified-fold-hide-all | Smart hide all           |
| =C-c C-s=    | unified-fold-show-all | Smart show all           |

**** Additional Functions
| Keybinding | Function                 | Description                    |
|------------|--------------------------|--------------------------------|
| =C-c e f=  | lsp-apply-folding-ranges | Apply LSP folding ranges       |
| =C-c m f=  | toggle-folding-modeline  | Toggle folding modeline        |
| =C-c f w=  | folding-save-state       | Save folding state             |
| =C-c f r=  | folding-restore-state    | Restore folding state          |
| =C-c f i=  | folding-status           | Show folding info              |

* Workflow Management
** Perpective
*** Core Configuration
#+begin_src emacs-lisp
(use-package perspective
  :init
  (persp-mode)
  :custom
  (persp-mode-prefix-key (kbd "C-c p"))
  :config
  
  ;; Auto-save perspective state on exit
  (add-hook 'kill-emacs-hook #'persp-state-save)
  
  ;; Display buffer configuration
  (setq display-buffer-base-action
        '((display-buffer-reuse-window display-buffer-same-window)
          (reusable-frames . t))
        even-window-sizes nil)
  ;; Set custom variables before loading
  (setq persp-sort 'access
        persp-show-modestring t
        persp-modestring-short t
        persp-state-default-file (expand-file-name "perspective-session" user-emacs-directory)
        persp-interactive-completion-function #'completing-read
        persp-purge-initial-persp-on-save nil
        persp-frame-global-perspective-name "GLOBAL"))
#+end_src

*** Core Helper Functions
#+begin_src emacs-lisp
(defun my/persp-switch-to-buffer-dwim ()
  "Switch to buffer in current perspective, or all buffers with prefix."
  (interactive)
  (if current-prefix-arg
      (call-interactively #'switch-to-buffer)
    (call-interactively #'persp-switch-to-buffer*)))

(defun my/persp-kill-buffer-dwim ()
  "Kill buffer in current perspective, or any buffer with prefix."
  (interactive)
  (if current-prefix-arg
      (call-interactively #'kill-buffer)
    (call-interactively #'persp-kill-buffer*)))

(defun my/persp-kill-current ()
  "Kill current perspective with confirmation."
  (interactive)
  (let ((current-persp (persp-current-name)))
    (when (and current-persp
               (not (string= current-persp persp-initial-frame-name))
               (yes-or-no-p (format "Kill perspective '%s'? " current-persp)))
      (persp-kill current-persp))))

(defun my/persp-list-perspectives ()
  "List all perspectives with enhanced formatting."
  (interactive)
  (let ((perspectives (persp-names))
        (current (persp-current-name)))
    (if perspectives
        (message "Perspectives: %s" 
                 (mapconcat (lambda (p)
                             (if (equal p current)
                                 (propertize p 'face 'bold)
                               p))
                           perspectives 
                           " | "))
      (message "No perspectives available"))))

(defun my/persp-switch-by-number (num)
  "Switch to perspective by number."
  (interactive "nPerspective number: ")
  (let ((perspectives (persp-names)))
    (if (and perspectives (>= num 1) (<= num (length perspectives)))
        (persp-switch (nth (1- num) perspectives))
      (message "Invalid perspective number: %d" num))))

(defun my/persp-reset-layout ()
  "Reset current perspective to single window."
  (interactive)
  (delete-other-windows)
  (let ((scratch-buffer (format "*scratch-%s*" (persp-current-name))))
    (switch-to-buffer (get-buffer-create scratch-buffer))))

(defun my/persp-save-session-as ()
  "Save perspective session to specified file."
  (interactive)
  (let ((file (read-file-name "Save perspective session to: "
                             user-emacs-directory
                             "perspective-session")))
    (persp-state-save file)))

(defun my/persp-load-session ()
  "Load perspective session from file."
  (interactive)
  (let ((file (read-file-name "Load perspective session from: "
                             user-emacs-directory
                             "perspective-session"
                             t)))
    (when (file-exists-p file)
      (persp-state-load file))))

(defun my/persp-cleanup-empty ()
  "Clean up empty perspectives (except main/initial)."
  (interactive)
  (dolist (persp-name (persp-names))
    (when (and (not (string= persp-name persp-initial-frame-name))
               (not (string= persp-name "main"))
               (null (persp-buffers persp-name)))
      (persp-kill persp-name)))
  (message "Cleaned up empty perspectives"))

(defun my/setup-initial-perspectives ()
  "Set up initial perspectives for common workflows."
  (interactive)
  (let ((original-persp (persp-current-name)))
    (persp-switch "config")
    (persp-switch "notes")
    (persp-switch "coding")
    (persp-switch (or original-persp "main"))))

(defun my/perspective-save-and-kill-emacs ()
  "Save perspective state and kill Emacs."
  (interactive)
  (persp-state-save)
  (save-buffers-kill-emacs))
#+end_src

*** Global Keybindings
#+begin_src emacs-lisp
(global-set-key (kbd "C-x C-c") #'my/perspective-save-and-kill-emacs)
(global-set-key (kbd "C-c p s") #'persp-switch)
(global-set-key (kbd "C-c p k") #'my/persp-kill-current)
(global-set-key (kbd "C-c p n") #'persp-next)
(global-set-key (kbd "C-c p p") #'persp-prev)
(global-set-key (kbd "C-c p r") #'persp-rename)
(global-set-key (kbd "C-c p l") #'my/persp-list-perspectives)
(global-set-key (kbd "C-c p b") #'my/persp-switch-to-buffer-dwim)
#+end_src

*** Project and Ibuffer Integration
#+begin_src emacs-lisp
;; Project integration
(defun my/setup-project-integration ()
  "Setup project integration if project.el is available"
  (when (featurep 'project)
    (defun my/persp-switch-to-project ()
      "Switch to project perspective or create one."
      (interactive)
      (if-let ((project (project-current)))
	  (let* ((project-root (project-root project))
		 (project-name (file-name-nondirectory (directory-file-name project-root))))
            (persp-switch project-name))
	(message "Not in a project")))))

(defun my/setup-project-integration ()
  "Setup project integration if project.el is available."
  (when (featurep 'project)
    (defun my/persp-switch-to-project ()
      "Switch to project perspective or create one."
      (interactive)
      (if-let ((project (project-current)))
          (let* ((project-root (project-root project))
                 (project-name (file-name-nondirectory) 
                               (directory-file-name project-root)))
            (persp-switch project-name))
        (message "Not in a project")))
    
    (defun my/auto-create-project-perspective ()
      "Automatically create perspective for current project."
      (when-let ((project (project-current)))
        (let* ((project-root (project-root project))
               (project-name (file-name-nondirectory) 
                             (directory-file-name project-root)))
          (unless (member project-name (persp-names))
            (persp-switch project-name)))))
    
    (add-hook 'project-find-file-hook #'my/auto-create-project-perspective)
    (global-set-key (kbd "C-c p P") #'my/persp-switch-to-project)))

;; Ibuffer integration (safer implementation)
(defun my/setup-ibuffer-integration ()
  "Setup ibuffer integration if available."
  (when (featurep 'ibuffer)
    (defun my/persp-ibuffer-dwim ()
      "Open ibuffer filtered by perspective, or all buffers with prefix."
      (interactive)
      (if current-prefix-arg
          (ibuffer t)
        (persp-ibuffer)))
    
    (defun my/persp-ibuffer-hook ()
      "Hook function for ibuffer perspective integration."
      (condition-case err
          (when (fboundp 'persp-ibuffer-set-filter-groups)
            (persp-ibuffer-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic)))
        (error (message "Error in persp-ibuffer-hook: %s" err))))
    
    (add-hook 'ibuffer-hook #'my/persp-ibuffer-hook)
    (global-set-key (kbd "C-c p i") #'my/persp-ibuffer-dwim)))

;; Load integrations after initialization
(add-hook 'after-init-hook
          (lambda ()
            (my/setup-project-integration)
            (my/setup-ibuffer-integration)))
#+end_src

*** Which Key Integration
#+begin_src emacs-lisp
(with-eval-after-load 'which-key
  (when (fboundp 'which-key-add-key-based-replacements)
    (which-key-add-key-based-replacements
      "C-c p" "perspective"
      "SPC l" "layouts")))
#+end_src

*** Consult Integration
#+begin_src emacs-lisp
(with-eval-after-load 'consult
  (when (boundp 'persp-consult-source)
    (add-to-list 'consult-buffer-sources persp-consult-source)))
#+end_src

*** Treemacs Integration
#+begin_src emacs-lisp
(with-eval-after-load 'treemacs
  (defun my/persp-treemacs-sync ()
    "Sync treemacs workspace with current perspective."
    (condition-case err
        (when (and (bound-and-true-p treemacs--ready)
                   (not (string= (persp-current-name) persp-initial-frame-name)))
          (let ((workspace-name (format "persp-%s" (persp-current-name))))
            (unless (treemacs-workspace->is-name-taken? workspace-name)
              (treemacs--create-workspace workspace-name)
              (treemacs-switch-workspace workspace-name))))
      (error (message "Error in treemacs sync: %s" err))))
  
  (add-hook 'persp-switch-hook #'my/persp-treemacs-sync))
#+end_src

** Imenu
*** Core Configuration
#+begin_src emacs-lisp
(use-package imenu
  :ensure nil
  :custom
  (imenu-auto-rescan t)
  (imenu-use-popup-menu nil)
  (imenu-eager-completion-buffer t)
  (imenu-space-replacement " ")
  (imenu-level-separator "/")
  (imenu-max-item-length 100)
  (imenu-sort-function 'imenu--sort-by-name)
  
  :config
  ;; Enhanced imenu expressions for various modes
  (defun my/imenu-setup-expressions ()
    "Set up better imenu expressions for different modes."
    (cond
     ;; Emacs Lisp mode
     ((derived-mode-p 'emacs-lisp-mode)
      (setq imenu-generic-expression
            '(("Functions" "^\\s-*(defun\\s-+\\([a-zA-Z0-9-_/]+\\)" 1)
              ("Interactive Functions" "^\\s-*(defun\\s-+\\([a-zA-Z0-9-_/]+\\).*\\(interactive\\)" 1)
              ("Macros" "^\\s-*(defmacro\\s-+\\([a-zA-Z0-9-_/]+\\)" 1)
              ("Variables" "^\\s-*(defvar\\s-+\\([a-zA-Z0-9-_/]+\\)" 1)
              ("Constants" "^\\s-*(defconst\\s-+\\([a-zA-Z0-9-_/]+\\)" 1)
              ("Custom" "^\\s-*(defcustom\\s-+\\([a-zA-Z0-9-_/]+\\)" 1)
              ("Faces" "^\\s-*(defface\\s-+\\([a-zA-Z0-9-_/]+\\)" 1)
              ("Advice" "^\\s-*(defadvice\\s-+\\([a-zA-Z0-9-_/]+\\)" 1)
              ("Use-Package" "^\\s-*(use-package\\s-+\\([a-zA-Z0-9-_/]+\\)" 1))))
     
     ;; Python mode (covers both regular and tree-sitter modes)
     ((derived-mode-p 'python-mode 'python-ts-mode)
      (setq imenu-generic-expression
            '(("Classes" "^\\s-*class\\s-+\\([a-zA-Z0-9_]+\\)" 1)
              ("Functions" "^\\s-*def\\s-+\\([a-zA-Z0-9_]+\\)" 1)
              ("Async Functions" "^\\s-*async\\s-+def\\s-+\\([a-zA-Z0-9_]+\\)" 1)
              ("Methods" "^\\s-*def\\s-+\\([a-zA-Z0-9_]+\\)\\s-*(\\s-*self" 1))))
     
     ;; C/C++ modes (covers both regular and tree-sitter modes)
     ((derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
      (setq imenu-generic-expression
            '(("Functions" "^\\s-*\\(?:static\\s-+\\)?\\(?:inline\\s-+\\)?\\w+\\s-+\\*?\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" 1)
              ("Classes" "^\\s-*class\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
              ("Structs" "^\\s-*struct\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1)
              ("Typedefs" "^\\s-*typedef\\s-+.*\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*;" 1)
              ("Macros" "^\\s-*#define\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1))))
     
     ;; JSON mode (covers both regular and tree-sitter modes)
     ((derived-mode-p 'json-mode 'json-ts-mode)
      (setq imenu-generic-expression
            '(("Keys" "^\\s-*\"\\([^\"]+\\)\"\\s-*:" 1))))
     
     ;; YAML mode (covers both regular and tree-sitter modes)
     ((derived-mode-p 'yaml-mode 'yaml-ts-mode)
      (setq imenu-generic-expression
            '(("Keys" "^\\([a-zA-Z0-9_-]+\\):" 1))))))
  
  ;; Eglot integration for enhanced imenu
  (defun my/imenu-eglot-setup ()
    "Setup imenu with eglot integration when available."
    (when (and (bound-and-true-p eglot--managed-mode)
               (eglot-current-server))
      (setq-local imenu-create-index-function #'my/eglot-imenu-create-index)))
  
  (defun my/eglot-imenu-create-index ()
    "Create imenu index using eglot document symbols."
    (condition-case err
        (when (and (eglot-current-server)
                   (eglot-server-capable :documentSymbolProvider))
          (let* ((symbols (jsonrpc-request (eglot-current-server)
                                          :textDocument/documentSymbol
                                          (list :textDocument (eglot--TextDocumentIdentifier))))
                 (index nil))
            (when symbols
              (dolist (symbol symbols)
                (let* ((name (plist-get symbol :name))
                       (kind (plist-get symbol :kind))
                       (range (plist-get symbol :range))
                       (start (plist-get range :start))
                       (line (1+ (plist-get start :line)))
                       (character (plist-get start :character))
                       (position (save-excursion
                                  (goto-char (point-min))
                                  (forward-line (1- line))
                                  (forward-char character)
                                  (point)))
                       (category (my/lsp-symbol-kind-to-category kind)))
                  (when (and category name)
                    (let ((entry (assoc category index)))
                      (if entry
                          (setcdr entry (cons (cons name position) (cdr entry)))
                        (push (list category (cons name position)) index))))))
              ;; Sort each category
              (dolist (category index)
                (setcdr category (sort (cdr category) 
                                     (lambda (a b) (string< (car a) (car b))))))
              index)))
      (error 
       (message "Eglot imenu error: %s" (error-message-string err))
       (imenu-default-create-index-function))))
  
  (defun my/lsp-symbol-kind-to-category (kind)
    "Convert LSP symbol kind to imenu category."
    (pcase kind
      (1 "Files") (2 "Modules") (3 "Namespaces") (4 "Packages")
      (5 "Classes") (6 "Methods") (7 "Properties") (8 "Fields")
      (9 "Constructors") (10 "Enums") (11 "Interfaces") (12 "Functions")
      (13 "Variables") (14 "Constants") (15 "Strings") (16 "Numbers")
      (17 "Booleans") (18 "Arrays") (19 "Objects") (20 "Keys")
      (21 "Null") (22 "Enum Members") (23 "Structs") (24 "Events")
      (25 "Operators") (26 "Type Parameters") (_ "Other")))
  
  ;; Setup hooks for mode-specific expressions
  (dolist (mode '(emacs-lisp-mode-hook
                  python-mode-hook python-ts-mode-hook
                  c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook
                  json-mode-hook json-ts-mode-hook
                  yaml-mode-hook yaml-ts-mode-hook))
    (add-hook mode #'my/imenu-setup-expressions))
  
  ;; Eglot integration hook
  (add-hook 'eglot-managed-mode-hook #'my/imenu-eglot-setup)
  
  ;; Consult integration configuration
  (with-eval-after-load 'consult
    (setq consult-imenu-config
          '((emacs-lisp-mode :toplevel "Functions"
                            :types ((?f "Functions" font-lock-function-name-face))
                                   (?i "Interactive Functions" font-lock-function-name-face)
                                   (?m "Macros" font-lock-function-name-face)
                                   (?v "Variables" font-lock-variable-name-face)
                                   (?c "Constants" font-lock-constant-face)
                                   (?C "Custom" font-lock-variable-name-face)
                                   (?F "Faces" font-lock-type-face)
                                   (?a "Advice" font-lock-keyword-face)
                                   (?u "Use-Package" font-lock-keyword-face))
            
            ;; Shared configuration for C/C++ modes (DRY principle)
            ,@(let ((c-config '(:toplevel "Functions")
                               :types ((?f "Functions" font-lock-function-name-face))
                                      (?c "Classes" font-lock-type-face)
                                      (?s "Structs" font-lock-type-face)
                                      (?t "Typedefs" font-lock-type-face)
                                      (?d "Macros" font-lock-preprocessor-face)))
                (mapcar (lambda (mode) `(,mode ,@c-config))
                        '(c-mode c++-mode c-ts-mode c++-ts-mode)))
            
            ;; Shared configuration for Python modes
            ,@(let ((py-config '(:toplevel "Classes")
                                :types ((?c "Classes" font-lock-type-face))
                                       (?f "Functions" font-lock-function-name-face)
                                       (?a "Async Functions" font-lock-function-name-face)
                                       (?m "Methods" font-lock-function-name-face)))
                (mapcar (lambda (mode) `(,mode ,@py-config))
                        '(python-mode python-ts-mode)))))))
#+end_src

*** Tree-sitter Integration
#+begin_src emacs-lisp
(with-eval-after-load 'treesit
  (defun my/treesit-imenu-setup ()
    "Setup tree-sitter enhanced imenu when available."
    (when (and (treesit-parser-list)
               (not (bound-and-true-p eglot--managed-mode)))
      (setq-local imenu-create-index-function #'treesit-simple-imenu)))
  
  (add-hook 'prog-mode-hook #'my/treesit-imenu-setup))
#+end_src

*** Utility Functions
#+begin_src emacs-lisp
(defun my/imenu-reset-to-default ()
  "Reset imenu to use default create index function."
  (interactive)
  (setq-local imenu-create-index-function #'imenu-default-create-index-function)
  (setq imenu--index-alist nil)
  (message "Imenu reset to default behavior"))

(defun my/toggle-eglot-imenu ()
  "Toggle between eglot-enhanced imenu and default imenu."
  (interactive)
  (if (and (bound-and-true-p eglot--managed-mode)
           (eq imenu-create-index-function #'my/eglot-imenu-create-index))
      (progn
        (setq-local imenu-create-index-function #'imenu-default-create-index-function)
        (message "Switched to default imenu"))
    (progn
      (my/imenu-eglot-setup)
      (message "Switched to eglot-enhanced imenu")))
  (setq imenu--index-alist nil))
#+end_src

** Project
*** Core Setup
#+begin_src emacs-lisp
(use-package project
  :ensure nil
  :init
  ;; ;; Project discovery settings
  ;; (setq project-find-functions
  ;;       '(project-try-vc
  ;;         project-try-local))
  
  ;; Project file listing optimizations
  (setq project-vc-ignores '("target/" "build/" "dist/" ".git/"))
  (setq project-vc-extra-root-markers '(".projectile" ".project"))
  
  ;; Buffer switching behavior
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-dired "Dired")
          (project-vc-dir "VC-Dir")
          (project-shell "Shell")
          (project-compile "Compile")
          (project-switch-to-buffer "Switch buffer")
          (project-kill-buffers "Kill project buffers")))
  
  ;; Kill buffer behavior
  (setq project-kill-buffer-conditions
        '(buffer-file-name
          (major-mode . compilation-mode)
          (major-mode . shell-mode)
          (major-mode . eshell-mode)
          (major-mode . vterm-mode)
          (derived-mode . special-mode)))
  
  ;; Performance optimizations
  (setq project-list-remote-repositories nil)
  (setq project-vc-merge-submodules nil)
  
  :config
  ;; Enhanced project switching with perspective integration
  (defun my/project-switch-with-perspective (project-root)
    "Switch to project and create/switch to corresponding perspective."
    (interactive (list (project-prompt-project-dir)))
    (let* ((project-name (file-name-nondirectory (directory-file-name project-root)))
           (perspective-name (format "Project: %s" project-name)))
      ;; Switch to or create perspective
      (when (fboundp 'persp-switch)
        (persp-switch perspective-name))
      ;; Switch to project
      (project-switch-project project-root)
      ;; Update treemacs if available
      (when (and (fboundp 'treemacs-add-and-display-current-project-exclusively)
                 (project-current))
        (treemacs-add-and-display-current-project-exclusively))))
  
  ;; Project buffer management with ibuffer integration
  (defun my/project-ibuffer ()
    "Open ibuffer filtered to current project buffers."
    (interactive)
    (if-let ((project (project-current)))
        (let ((project-root (project-root project)))
          (ibuffer nil "*Project Buffers*"
                   `((filename . ,(regexp-quote project-root)))))
      (message "Not in a project")))
  
  ;; Enhanced project find file
  (defun my/project-find-file-dwim ()
    "Find file in project with enhanced completion."
    (interactive)
    (if (project-current)
        (call-interactively #'project-find-file)
      (call-interactively #'find-file)))
  
  ;; Project-aware buffer switching
  (defun my/project-switch-buffer ()
    "Switch to buffer within current project."
    (interactive)
    (if (project-current)
        (call-interactively #'project-switch-to-buffer)
      (call-interactively #'switch-to-buffer)))
  
  ;; Project compilation with directory awareness
  (defun my/project-compile ()
    "Compile project from project root."
    (interactive)
    (if-let ((project (project-current)))
        (let ((default-directory (project-root project)))
          (call-interactively #'compile))
      (call-interactively #'compile)))
  
  ;; Project terminal/shell
  (defun my/project-shell ()
    "Open shell in project root."
    (interactive)
    (if-let ((project (project-current)))
	(let* ((project-root (project-root project))
               (project-name (file-name-nondirectory (directory-file-name project-root)))
               (default-directory project-root))
          (cond
           ((fboundp 'vterm) (vterm (format "*vterm-%s*" project-name)))
           ((fboundp 'eat) (eat (format "*eat-%s*" project-name)))
         (t (shell (format "*shell-%s*" project-name)))))
      (cond
       ((fboundp 'vterm) (vterm))
       ((fboundp 'eat) (eat))
       (t (shell))))) 
  
  ;; Project magit integration
  (defun my/project-magit ()
    "Open magit for current project."
    (interactive)
    (if-let ((project (project-current)))
        (if (fboundp 'magit-status)
            (magit-status (project-root project))
          (message "Magit not available"))
      (if (fboundp 'magit-status)
          (call-interactively #'magit-status)
        (message "Magit not available"))))
  
  ;; Project treemacs integration
  (defun my/project-treemacs ()
    "Focus treemacs on current project."
    (interactive)
    (if-let ((project (project-current)))
        (if (fboundp 'treemacs-select-window)
            (progn
              (treemacs-select-window)
              (when (fboundp 'treemacs-add-and-display-current-project-exclusively)
                (treemacs-add-and-display-current-project-exclusively)))
          (message "Treemacs not available"))
      (if (fboundp 'treemacs)
          (treemacs)
        (message "Treemacs not available"))))
  
  ;; Project bookmark integration
  (defun my/project-bookmark ()
    "Set bookmark for current project."
    (interactive)
    (if-let ((project (project-current)))
	(let* ((project-root (project-root project))
               (project-name (file-name-nondirectory (directory-file-name project-root)))
               (bookmark-name (format "Project: %s" project-name)))
          (bookmark-set bookmark-name))
      (call-interactively #'bookmark-set)))
  
  ;; Enhanced project discovery
  (defun my/project-discover-and-switch ()
    "Discover projects and switch to one."
    (interactive)
    (let* ((known-projects (project-known-project-roots))
           (discovered-projects (append
                                (my/find-projects-in-directory "~/projects/" 2)
                                (my/find-projects-in-directory "~/work/" 2)
                                (my/find-projects-in-directory "~/code/" 2)
                                (my/find-projects-in-directory "~/dev/" 2)))
           (all-projects (delete-dups (append known-projects discovered-projects))))
      (if all-projects
          (let ((selected (completing-read "Switch to project: " all-projects)))
            (my/project-switch-with-perspective selected))
        (message "No projects found"))))
  
  (defun my/find-projects-in-directory (dir max-depth)
    "Find projects in DIR up to MAX-DEPTH levels deep."
    (when (and (file-directory-p dir) (> max-depth 0))
      (let ((projects '()))
        (dolist (subdir (directory-files dir t "^[^.]" t))
          (when (file-directory-p subdir)
            (if (my/is-project-directory-p subdir)
                (push subdir projects)
              (setq projects (append projects 
                                   (my/find-projects-in-directory subdir (1- max-depth)))))))
        projects)))
  
  (defun my/is-project-directory-p (dir)
    "Check if DIR is a project directory."
    (or (file-exists-p (expand-file-name ".git" dir))
        (file-exists-p (expand-file-name ".projectile" dir))
        (file-exists-p (expand-file-name "pyproject.toml" dir))
        (file-exists-p (expand-file-name "requirements.txt" dir))
        (file-exists-p (expand-file-name "Pipfile" dir))
        (file-exists-p (expand-file-name "Makefile" dir))
        (file-exists-p (expand-file-name "CMakeLists.txt" dir))))
  
  ;; Project cleanup utilities
  (defun my/project-cleanup-buffers ()
    "Kill buffers not related to current project."
    (interactive)
    (if-let ((project (project-current)))
        (let ((project-buffers (project-buffers project))
              (killed-count 0))
          (dolist (buffer (buffer-list))
            (unless (or (member buffer project-buffers)
                        (string-prefix-p " " (buffer-name buffer))
                        (string-prefix-p "*" (buffer-name buffer)))
              (when (buffer-live-p buffer)
                (kill-buffer buffer)
                (setq killed-count (1+ killed-count)))))
          (message "Killed %d non-project buffers" killed-count))
      (message "Not in a project")))
  
  (defun my/project-kill-all-buffers ()
    "Kill all buffers in current project."
    (interactive)
    (if-let ((project (project-current)))
        (let ((killed-count 0))
          (dolist (buffer (project-buffers project))
            (when (buffer-live-p buffer)
              (kill-buffer buffer)
              (setq killed-count (1+ killed-count))))
          (message "Killed %d project buffers" killed-count))
      (message "Not in a project")))
  
  ;; Project search and replace functions
  (defun my/project-search-dwim ()
    "Search in project with smart defaults."
    (interactive)
    (if (project-current)
        (if (use-region-p)
            (project-find-regexp (buffer-substring-no-properties 
                                 (region-beginning) (region-end)))
          (call-interactively #'project-find-regexp))
      (call-interactively #'grep)))
  
  (defun my/project-replace-dwim ()
    "Replace in project with smart defaults."
    (interactive)
    (if (project-current)
        (if (use-region-p)
            (let ((search-term (buffer-substring-no-properties 
                               (region-beginning) (region-end))))
              (project-query-replace-regexp 
               search-term 
               (read-string (format "Replace '%s' with: " search-term))))
          (call-interactively #'project-query-replace-regexp))
      (call-interactively #'query-replace-regexp)))
  
  ;; Project file operations
  (defun my/project-copy-file-path ()
    "Copy the current file's path relative to project root."
    (interactive)
    (if-let* ((project (project-current))
              (file (buffer-file-name))
              (project-root (project-root project)))
        (let ((relative-path (file-relative-name file project-root)))
          (kill-new relative-path)
          (message "Copied: %s" relative-path))
      (message "Not in a project or not visiting a file")))
  
  (defun my/project-copy-file-path-absolute ()
    "Copy the current file's absolute path."
    (interactive)
    (if-let ((file (buffer-file-name)))
        (progn
          (kill-new file)
          (message "Copied: %s" file))
      (message "Not visiting a file")))
  
  (defun my/project-find-other-file ()
    "Find related file (header/source, test/implementation, etc.)."
    (interactive)
    (if-let ((project (project-current)))
        (let* ((current-file (buffer-file-name))
               (file-name (file-name-nondirectory current-file))
               (file-base (file-name-sans-extension file-name))
               (file-ext (file-name-extension file-name))
               (project-files (project-files project))
               (other-candidates (my/get-related-file-candidates file-base file-ext))
               (found-files '()))
          (dolist (candidate other-candidates)
            (when-let ((found (cl-find-if (lambda (f) (string-suffix-p candidate f)) project-files)))
              (push found found-files)))
          (if found-files
              (find-file (if (= (length found-files) 1)
                            (car found-files)
                          (completing-read "Choose file: " found-files)))
            (message "No related file found")))
      (message "Not in a project")))
  
  (defun my/get-related-file-candidates (file-base file-ext)
    "Get list of related file candidates for FILE-BASE with FILE-EXT."
    (cond
     ;; C/C++ header/source files
     ((string= file-ext "c") 
      (list (concat file-base ".h")))
     ((string= file-ext "h") 
      (list (concat file-base ".c")
            (concat file-base ".cpp")
            (concat file-base ".cc")
            (concat file-base ".cxx")))
     ((member file-ext '("cpp" "cc" "cxx"))
      (list (concat file-base ".h")
            (concat file-base ".hpp")
            (concat file-base ".hxx")))
     ((member file-ext '("hpp" "hxx"))
      (list (concat file-base ".cpp")
            (concat file-base ".cc")
            (concat file-base ".cxx")))
     ;; Test files
     ((string-match-p "_test\\|Test\\|_spec\\|Spec" file-base)
      (let ((clean-base (replace-regexp-in-string "_test\\|Test\\|_spec\\|Spec" "" file-base)))
        (list (concat clean-base "." file-ext))))
     ;; Regular files to test files
     (t (list (concat file-base "_test." file-ext)
              (concat file-base "Test." file-ext)
              (concat file-base "_spec." file-ext)
              (concat file-base "Spec." file-ext)))))
  
  ;; Basic project task runner
  (defun my/project-run-task ()
    "Run project task based on project type."
    (interactive)
    (if-let ((project (project-current)))
        (let ((default-directory (project-root project)))
          (cond
           ((file-exists-p "Makefile") (my/project-make-run))
           ((file-exists-p "pyproject.toml") (my/project-python-run))
           (t (call-interactively #'compile))))
      (message "Not in a project")))
  
  (defun my/project-make-run ()
    "Run make target in current project."
    (let ((targets (my/get-make-targets)))
      (if targets
          (let ((target (completing-read "Make target: " targets)))
            (compile (format "make %s" target)))
        (compile "make"))))
  
  (defun my/get-make-targets ()
    "Get available make targets from Makefile."
    (when (file-exists-p "Makefile")
      (with-temp-buffer
        (insert-file-contents "Makefile")
        (let ((targets '()))
          (while (re-search-forward "^\\([a-zA-Z0-9_-]+\\):" nil t)
            (push (match-string 1) targets))
          (nreverse targets)))))
  
  (defun my/project-python-run ()
    "Run python command in current project."
    (let ((command (completing-read "Python command: " 
                                   '("python -m pytest" "python setup.py test" 
                                     "python -m flake8" "python -m black ."))))
      (compile command))))
#+end_src

*** Enhanced project discovery for specific project types
#+begin_src emacs-lisp
(with-eval-after-load 'project
  ;; Project type detection functions
  (defun project-try-python (dir)
    "Try to find a Python project at DIR."
    (when-let ((root (or (locate-dominating-file dir "pyproject.toml")
                         (locate-dominating-file dir "setup.py")
                         (locate-dominating-file dir "requirements.txt"))))
      (cons 'python root)))
  
  (defun project-try-make (dir)
    "Try to find a project with Makefile at DIR."
    (when-let ((root (locate-dominating-file dir "Makefile")))
      (cons 'make root)))
  
  (defun project-try-cmake (dir)
    "Try to find a CMake project at DIR."
    (when-let ((root (locate-dominating-file dir "CMakeLists.txt")))
      (cons 'cmake root)))
  
  ;; Add project type detection functions
  (cl-pushnew #'project-try-python project-find-functions)
  (cl-pushnew #'project-try-make project-find-functions)
  (cl-pushnew #'project-try-cmake project-find-functions))
#+end_src
*** iBuffer Integration
#+begin_src emacs-lisp
(with-eval-after-load 'ibuffer
  (defun my/ibuffer-project-group ()
    "Generate ibuffer groups based on projects."
    (let ((groups '()))
      ;; Current project group
      (when-let ((current-project (project-current)))
        (let ((current-root (project-root current-project)))
          (push `("Current Project" 
                  (predicate . (lambda ()
                                 (when-let ((buf-file (buffer-file-name)))
                                  (string-prefix-p ,current-root buf-file)))))
                groups)))
      ;; Other projects group
      (push `("Other Projects" 
              (predicate . (lambda ()
                             (and (buffer-file-name)
                                 (project-current nil (file-name-directory (buffer-file-name)))
                                 (not (when-let ((current-project (project-current)))
					(string-prefix-p (project-root current-project)
							(buffer-file-name))))))))
            groups)
      ;; Non-project buffers
      (push `("Non-Project" 
              (predicate . (lambda ()
                             (not (and (buffer-file-name)
                                      (project-current nil (file-name-directory (buffer-file-name))))))))
            groups)
      (nreverse groups))))
#+end_src

*** Perspective Integration
#+begin_src emacs-lisp
(with-eval-after-load 'perspective
  (defun my/project-perspective-hook ()
    "Create perspective when switching projects."
    (when-let ((project (project-current)))
      (let ((project-name (file-name-nondirectory 
                          (directory-file-name (project-root project)))))
        (unless (member project-name (persp-names))
          (persp-switch project-name)))))
  
  (add-hook 'project-switch-project-hook #'my/project-perspective-hook))
#+end_src

*** Treemacs Integration
#+begin_src emacs-lisp
(with-eval-after-load 'treemacs
  (defun my/project-treemacs-hook ()
    "Update treemacs when switching projects."
    (when (and (fboundp 'treemacs-current-workspace)
               (project-current))
      (treemacs-add-and-display-current-project-exclusively)))
  
  (add-hook 'project-switch-project-hook #'my/project-treemacs-hook))
#+end_src
** Ibuffer
#+begin_src emacs-lisp
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  ;; Customize nerd-icons-ibuffer settings
  (setq nerd-icons-ibuffer-icon-size 1.0
        nerd-icons-ibuffer-color-icon t
        nerd-icons-ibuffer-icon-v-adjust 0.0))

(use-package ibuffer
  :ensure nil
  :commands (ibuffer ibuffer-other-window)
  :bind (("C-x C-b" . ibuffer))
  :init
  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-display-summary nil
        ibuffer-use-header-line t
        ibuffer-default-sorting-mode 'alphabetic
        ibuffer-always-show-last-buffer nil
        ibuffer-use-other-window nil
        ibuffer-truncate-lines t
        ibuffer-auto-mode t
        ibuffer-old-time 24
        ibuffer-default-directory default-directory)
  
  :config
  ;; Custom faces using Gruvbox dark theme hex codes
  (custom-set-faces
   '(ibuffer-filter-group-name ((t (:foreground "#fb4934" :weight bold :height 1.1))))
   '(ibuffer-title-face ((t (:foreground "#fabd2f" :weight bold :height 1.2))))
   '(ibuffer-marked-face ((t (:foreground "#b8bb26" :weight bold))))
   '(ibuffer-deletion-face ((t (:foreground "#fb4934" :weight bold))))
   '(ibuffer-help-buffer-face ((t (:foreground "#83a598"))))
   '(ibuffer-compressed-file-face ((t (:foreground "#d3869b"))))
   '(ibuffer-special-buffer-face ((t (:foreground "#fe8019"))))
   '(ibuffer-directory-face ((t (:foreground "#83a598" :weight bold))))
   '(ibuffer-read-only-face ((t (:foreground "#8ec07c"))))
   '(ibuffer-locked-buffer-face ((t (:foreground "#fb4934"))))
   '(ibuffer-size-header-face ((t (:foreground "#d5c4a1"))))
   '(ibuffer-mode-header-face ((t (:foreground "#d5c4a1"))))
   '(ibuffer-name-header-face ((t (:foreground "#d5c4a1"))))
   '(ibuffer-filename-header-face ((t (:foreground "#d5c4a1")))))

  ;; Custom ibuffer formats - using standard columns
  (setq ibuffer-formats
        '((mark modified read-only locked
                " " (name 28 28 :left :elide)
                " " (size 9 -1 :right)
                " " (mode+ 14 14 :left :elide)
                " " filename-and-process)
          (mark modified read-only locked
                " " (name 28 28 :left :elide)
                " " (size 9 -1 :right)
                " " (mode+ 14 14 :left :elide))))

  ;; Enhanced mode display with colors
  (define-ibuffer-column mode+
    (:name "Mode" :inline t)
    (let ((mode-name (format-mode-line mode-name)))
      (cond
       ((string-match-p "Python\\|python" mode-name)
        (propertize mode-name 'face '(:foreground "#fabd2f" :weight bold)))
       ((string-match-p "C\\|c-mode\\|c++" mode-name)
        (propertize mode-name 'face '(:foreground "#83a598" :weight bold)))
       ((string-match-p "CSS\\|SCSS\\|css\\|scss" mode-name)
        (propertize mode-name 'face '(:foreground "#d3869b" :weight bold)))
       ((string-match-p "LaTeX\\|TeX\\|latex\\|tex" mode-name)
        (propertize mode-name 'face '(:foreground "#8ec07c" :weight bold)))
       ((string-match-p "Text\\|text" mode-name)
        (propertize mode-name 'face '(:foreground "#fe8019" :weight bold)))
       ((string-match-p "Magit\\|Git\\|magit\\|git" mode-name)
        (propertize mode-name 'face '(:foreground "#fb4934" :weight bold)))
       ((string-match-p "Shell\\|Term\\|shell\\|term\\|eshell" mode-name)
        (propertize mode-name 'face '(:foreground "#fabd2f" :weight bold)))
       ((string-match-p "Dired\\|dired" mode-name)
        (propertize mode-name 'face '(:foreground "#83a598" :weight bold)))
       ((string-match-p "Help\\|Info\\|help\\|info" mode-name)
        (propertize mode-name 'face '(:foreground "#d3869b" :weight bold)))
       ((string-match-p "Treemacs\\|treemacs" mode-name)
        (propertize mode-name 'face '(:foreground "#8ec07c" :weight bold)))
       (t (propertize mode-name 'face '(:foreground "#d5c4a1"))))))

  ;; Helper functions for dynamic filter groups
  (defun ibuffer-current-project-root ()
    "Get current project root if available."
    (when (and (fboundp 'project-current) (project-current))
      (project-root (project-current))))

  (defun ibuffer-buffer-in-current-project-p (buffer)
    "Check if buffer belongs to current project."
    (let ((current-root (ibuffer-current-project-root))
          (buffer-file (buffer-file-name buffer)))
      (and current-root buffer-file
           (string-prefix-p current-root buffer-file))))

  (defun ibuffer-buffer-in-other-project-p (buffer)
    "Check if buffer belongs to a different project."
    (let ((current-root (ibuffer-current-project-root))
          (buffer-file (buffer-file-name buffer)))
      (and buffer-file
           (when-let ((buf-project (project-current nil buffer-file)))
             (let ((buf-root (project-root buf-project)))
               (and buf-root
                    (or (not current-root)
                        (not (string= buf-root current-root)))))))))

  (defun ibuffer-buffer-in-current-perspective-p (buffer)
    "Check if buffer is in current perspective."
    (and (fboundp 'persp-current-name)
         (fboundp 'persp-buffer-in-other-p)
         (not (persp-buffer-in-other-p buffer))))

  ;; Static filter groups definition
  (defun ibuffer-bufler-style-filter-groups ()
    "Generate dynamic filter groups based on current context."
    (let ((groups '()))
      
      ;; Current project group (if in a project)
      (when (ibuffer-current-project-root)
        (let ((project-name (file-name-nondirectory 
                            (directory-file-name (ibuffer-current-project-root)))))
          (push `(,(format "📁 Current Project: %s" project-name)
                  (predicate . (ibuffer-buffer-in-current-project-p (current-buffer))))
                groups)))
      
      ;; Current perspective group (if not main perspective)
      (when (and (fboundp 'persp-current-name) 
                 (not (string= (persp-current-name) "main")))
        (push `(,(format "👁️  Perspective: %s" (persp-current-name))
                (predicate . (ibuffer-buffer-in-current-perspective-p (current-buffer))))
              groups))
      
      ;; Static groups
      (append (nreverse groups)
              '(;; Treemacs
                ("🌲 Treemacs"
                 (or (name . "\\*Treemacs.*\\*")
                     (mode . treemacs-mode)))
                
                ;; Version Control
                ("🔀 Version Control"
                 (or (name . "\\*magit")
                     (name . "\\*git")
                     (name . "\\*vc")
                     (name . "COMMIT_EDITMSG")
                     (mode . magit-status-mode)
                     (mode . magit-log-mode)
                     (mode . magit-diff-mode)
                     (mode . magit-revision-mode)
                     (mode . magit-process-mode)
                     (mode . git-commit-mode)))
                
                ;; Other project files
                ("📂 Other Projects"
                 (predicate . (ibuffer-buffer-in-other-project-p (current-buffer))))
                
                ;; Programming files by mode
                ("🐍 Python"
                 (or (mode . python-mode)
                     (mode . python-ts-mode)
                     (name . "\\.py$")))
                
                ("⚙️  C/C++"
                 (or (mode . c-mode)
                     (mode . c++-mode)
                     (mode . c-ts-mode)
                     (mode . c++-ts-mode)
                     (name . "\\.[ch]$")
                     (name . "\\.[ch]pp$")
                     (name . "\\.[ch]xx$")
                     (name . "\\.cc$")))
                
                ("🎨 CSS/SCSS"
                 (or (mode . css-mode)
                     (mode . scss-mode)
                     (mode . sass-mode)
                     (mode . css-ts-mode)
                     (name . "\\.css$")
                     (name . "\\.scss$")
                     (name . "\\.sass$")))
                
                ("📄 LaTeX/TeX"
                 (or (mode . latex-mode)
                     (mode . tex-mode)
                     (mode . LaTeX-mode)
                     (mode . TeX-mode)
                     (name . "\\.tex$")
                     (name . "\\.latex$")
                     (name . "\\.sty$")
                     (name . "\\.cls$")))
                
                ("📝 Text Files"
                 (or (mode . text-mode)
                     (mode . markdown-mode)
                     (mode . org-mode)
                     (name . "\\.txt$")
                     (name . "\\.md$")
                     (name . "\\.org$")
                     (name . "README$")))
                
                ;; Interactive shells and terminals
                ("🖥️  Shells & Terminals"
                 (or (mode . shell-mode)
                     (mode . eshell-mode)
                     (mode . term-mode)
                     (mode . ansi-term-mode)
                     (mode . vterm-mode)
                     (name . "\\*shell\\*")
                     (name . "\\*terminal\\*")
                     (name . "\\*eshell\\*")
                     (name . "\\*ansi-term\\*")
                     (name . "\\*vterm\\*")))
                
                ;; Help and documentation
                ("❓ Help & Documentation"
                 (or (name . "\\*Help\\*")
                     (name . "\\*info\\*")
                     (name . "\\*Man ")
                     (name . "\\*woman\\*")
                     (name . "\\*Apropos\\*")
                     (name . "\\*eldoc\\*")
                     (mode . help-mode)
                     (mode . info-mode)
                     (mode . Man-mode)
                     (mode . woman-mode)
                     (mode . apropos-mode)))
                
                ;; File management
                ("📁 File Management"
                 (or (mode . dired-mode)
                     (mode . wdired-mode)
                     (name . "\\*Find\\*")
                     (name . "\\*Locate\\*")))
                
                ;; Development tools
                ("🔧 Development Tools"
                 (or (name . "\\*compilation\\*")
                     (name . "\\*Compile-Log\\*")
                     (name . "\\*Flycheck errors\\*")
                     (name . "\\*lsp-log\\*")
                     (name . "\\*eglot-log\\*")
                     (name . "\\*EGLOT.*log\\*")
                     (name . "\\*xref\\*")
                     (mode . compilation-mode)
                     (mode . grep-mode)
                     (mode . occur-mode)))
                
                ;; Special Emacs buffers
                ("⭐ Special Buffers"
                 (or (name . "\\*scratch\\*")
                     (name . "\\*Messages\\*")
                     (name . "\\*Warnings\\*")
                     (name . "\\*Backtrace\\*")
                     (name . "\\*Completions\\*")
                     (name . "\\*dashboard\\*")
                     (name . "\\*startup\\*")
                     (name . "\\*spacemacs\\*")))
                
                ;; Hidden buffers (starting with space)
                ("👻 Hidden Buffers"
                 (name . "^ "))
                
                ;; Other system buffers
                ("🔧 System Buffers"
                 (predicate . (string-match-p "\\*.*\\*" (buffer-name))))))))

  ;; Set up saved filter groups properly
  (setq ibuffer-saved-filter-groups
        `(("Bufler-style" . ,(ibuffer-bufler-style-filter-groups))))

  ;; Set up and apply filter groups
  (defun ibuffer-setup-filter-groups ()
    "Set up and apply the bufler-style filter groups."
    (setq ibuffer-saved-filter-groups
          `(("Bufler-style" . ,(ibuffer-bufler-style-filter-groups))))
    (when (derived-mode-p 'ibuffer-mode)
      (ibuffer-switch-to-saved-filter-groups "Bufler-style")))

  ;; Enhanced header line with context information
  (setq ibuffer-header-line-format
        '(:eval
          (let* ((current-perspective (if (fboundp 'persp-current-name)
                                          (persp-current-name)
                                        "main"))
                 (current-project (if (ibuffer-current-project-root)
                                      (file-name-nondirectory 
                                       (directory-file-name 
                                        (ibuffer-current-project-root)))
                                    "No Project"))
                 (buffer-count (length (buffer-list)))
                 (modified-count (length (cl-remove-if-not 
                                         (lambda (buf) (buffer-modified-p buf))
                                         (buffer-list))))
                 (project-info (if (not (string= current-project "No Project"))
                                   (format "📁 %s" current-project)
                                 "📁 No Project"))
                 (perspective-info (if (not (string= current-perspective "main"))
                                       (format "👁️  %s" current-perspective)
                                     "👁️  main"))
                 (stats-info (format "📈 %d buffers" buffer-count))
                 (modified-info (if (> modified-count 0)
                                    (format "✏️  %d modified" modified-count)
                                  "")))
            (propertize
             (concat " 📊 IBuffer | " perspective-info " | " project-info " | " 
                     stats-info
                     (if (not (string= modified-info ""))
                         (concat " | " modified-info)
                       ""))
             'face '(:foreground "#fabd2f" :weight bold :height 0.9)))))

  ;; Auto-refresh functionality with throttling
  (defvar ibuffer-refresh-timer nil
    "Timer for throttled ibuffer refresh.")

  (defvar ibuffer-last-refresh-time 0
    "Time of last ibuffer refresh.")

  (defun ibuffer-bufler-style-refresh ()
    "Refresh ibuffer with current project and perspective context."
    (interactive)
    (when (and (get-buffer "*Ibuffer*") (buffer-live-p (get-buffer "*Ibuffer*")))
      (with-current-buffer "*Ibuffer*"
        (when (derived-mode-p 'ibuffer-mode)
          (let ((current-line (line-number-at-pos))
                (current-column (current-column)))
            ;; Regenerate and apply filter groups
            (ibuffer-setup-filter-groups)
            (ibuffer-update nil t)
            ;; Restore cursor position
            (goto-char (point-min))
            (forward-line (1- current-line))
            (move-to-column current-column))))))

  (defun ibuffer-throttled-refresh ()
    "Throttled refresh to prevent excessive updates."
    (let ((current-time (current-time)))
      (when (or (not ibuffer-last-refresh-time)
                (> (float-time (time-subtract current-time ibuffer-last-refresh-time)) 1.0))
        (when ibuffer-refresh-timer
          (cancel-timer ibuffer-refresh-timer))
        (setq ibuffer-refresh-timer
              (run-with-timer 0.3 nil 
                              (lambda () 
                                (ibuffer-bufler-style-refresh)
                                (setq ibuffer-last-refresh-time (current-time))))))))

  ;; Setup hooks for automatic refresh
  (add-hook 'ibuffer-mode-hook 
            (lambda ()
              (ibuffer-setup-filter-groups)
              (ibuffer-auto-mode 1)))

  ;; Refresh on perspective changes (with safety check)
  (when (and (fboundp 'persp-switch-hook) (boundp 'persp-switch-hook))
    (add-hook 'persp-switch-hook #'ibuffer-throttled-refresh))

  ;; Refresh on project changes (with safety check)
  (when (and (fboundp 'project-find-file-hook) (boundp 'project-find-file-hook))
    (add-hook 'project-find-file-hook #'ibuffer-throttled-refresh))

  ;; Refresh when buffers are created/killed (but throttled)
  (add-hook 'buffer-list-update-hook 
            (lambda ()
              (when (get-buffer "*Ibuffer*")
                (ibuffer-throttled-refresh))))

  ;; Additional key bindings for enhanced functionality
  (define-key ibuffer-mode-map (kbd "C-c C-r") #'ibuffer-bufler-style-refresh)
  (define-key ibuffer-mode-map (kbd "/ /") #'ibuffer-filter-disable)
  (define-key ibuffer-mode-map (kbd "/ g") #'ibuffer-filters-to-filter-group)
  (define-key ibuffer-mode-map (kbd "/ TAB") #'ibuffer-exchange-filters)

  ;; Toggle sorting mode function
  (defun ibuffer-toggle-sorting-mode ()
    "Toggle between different ibuffer sorting modes."
    (interactive)
    (let ((modes '(alphabetic size major-mode filename/process)))
      (setq ibuffer-default-sorting-mode
            (or (cadr (member ibuffer-default-sorting-mode modes))
                (car modes)))
      (cond
       ((eq ibuffer-default-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))
       ((eq ibuffer-default-sorting-mode 'size)
        (ibuffer-do-sort-by-size))
       ((eq ibuffer-default-sorting-mode 'major-mode)
        (ibuffer-do-sort-by-major-mode))
       ((eq ibuffer-default-sorting-mode 'filename/process)
        (ibuffer-do-sort-by-filename/process)))
      (message "Sorting by: %s" ibuffer-default-sorting-mode)))

  (define-key ibuffer-mode-map (kbd "C-c s") #'ibuffer-toggle-sorting-mode)

  ;; Enhanced filtering functions
  (defun ibuffer-filter-by-project ()
    "Filter buffers by current project."
    (interactive)
    (let ((project-root (ibuffer-current-project-root)))
      (if project-root
          (ibuffer-filter-by-predicate 
           `(ibuffer-buffer-in-current-project-p (current-buffer)))
        (message "Not in a project"))))

  (defun ibuffer-filter-by-perspective ()
    "Filter buffers by current perspective."
    (interactive)
    (if (fboundp 'persp-current-name)
        (ibuffer-filter-by-predicate
         `(ibuffer-buffer-in-current-perspective-p (current-buffer)))
      (message "Perspective mode not available")))

  (define-key ibuffer-mode-map (kbd "/ p") #'ibuffer-filter-by-project)
  (define-key ibuffer-mode-map (kbd "/ v") #'ibuffer-filter-by-perspective))

;; Optional: Set up ibuffer as default buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)
#+end_src
** Treemacs
*** Core Configuration
#+begin_src emacs-lisp
(use-package treemacs
  :after nerd-icons
  :config
  ;;; Custom Helper Functions
  ;;; Defined inside :config to ensure treemacs is loaded first.
  (defun my-treemacs--get-project-root ()
    "Return the current project.el project root, if any."
    (when (fboundp 'project-current)
      (when-let ((project (project-current)))
        (project-root project))))

  (defun my-treemacs/toggle-or-select ()
    "Toggle treemacs, or select its window if it is already open."
    (interactive)
    (let ((treemacs-window (treemacs-get-local-window)))
      (if (and treemacs-window (eq (selected-window) treemacs-window))
          (treemacs-quit)
        (if treemacs-window
            (treemacs-select-window)
          (treemacs)))))

  (defun my-treemacs/find-in-current-project ()
    "Open treemacs, add the current project, and focus it."
    (interactive)
    (when-let ((project-root (my-treemacs--get-project-root)))
      (treemacs-add-and-display-project project-root (file-name-nondirectory (directory-file-name project-root)))))

  (defun my-treemacs/auto-add-project-on-find ()
    "Hook to automatically add the current project to the workspace."
    (when-let ((project-root (my-treemacs--get-project-root)))
      ;; Check if buffer's file is part of the current project before adding
      (when (and (buffer-file-name) 
                 (string-prefix-p project-root (buffer-file-name)))
        (let ((project-name (file-name-nondirectory (directory-file-name project-root))))
          (unless (treemacs-workspace-project-path-p project-root (treemacs-current-workspace))
            (treemacs-add-project-to-workspace project-root project-name))))))

  ;;; Core Settings
  ;;; Custom settings that differ from the default values.
  (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
        treemacs-follow-after-init               t
        treemacs-expand-after-init               t
        treemacs-is-never-other-window           t
        treemacs-no-delete-other-windows         t
        treemacs-project-follow-cleanup          t
        treemacs-position                        'left
        treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
        treemacs-silent-filewatch                t
        treemacs-silent-refresh                  t
        treemacs-width                           35
        treemacs-width-is-initially-locked       t)

  ;; Enable essential modes
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-load-theme "nerd-icons")

  ;; Configure git integration based on system capabilities
  (pcase (cons (executable-find "git") treemacs-python-executable)
    (`(t . t)   (treemacs-git-mode 'deferred))
    (`(t . nil) (treemacs-git-mode 'simple)))

  ;;; Hooks
  (add-hook 'find-file-hook #'my-treemacs/auto-add-project-on-find)
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (visual-line-mode 1))))
;; TOOD use these keybindings directly in general.el
  ;; :bind
  ;; (:map global-map
  ;;       ("M-0"       . treemacs-select-window)
  ;;       ("C-x t 1"   . treemacs-delete-other-windows)
  ;;       ("C-x t t"   . my-treemacs/toggle-or-select)
  ;;       ("C-x t d"   . treemacs-select-directory)
  ;;       ("C-x t B"   . treemacs-bookmark)
  ;;       ("C-x t C-t" . treemacs-find-file)
  ;;       ("C-x t M-t" . treemacs-find-tag)
  ;;       ("C-x t p"   . my-treemacs/find-in-current-project)))
#+end_src

*** Integrations with Other Packages
#+begin_src emacs-lisp
(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-evil 
  :after (treemacs evil))

(use-package treemacs-perspective
  :after (treemacs perspective)
  :config 
  (setq treemacs-scope-type 'Perspectives))
#+end_src

* Org
** Custom Functions
#+begin_src emacs-lisp
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))
#+end_src

** Directory and Path Setup
#+begin_src emacs-lisp
(defvar my/org-directory "~/org/"
  "Base directory for all org files.")

(defvar my/org-roam-directory (expand-file-name "roam/" my/org-directory)
  "Directory for org-roam files.")

(defvar my/org-downloads-directory (expand-file-name "downloads/" my/org-directory)
  "Directory for org-download files.")

(defvar my/org-noter-directory (expand-file-name "noter/" my/org-directory)
  "Directory for org-noter files.")

(defvar my/org-archive-directory (expand-file-name "archive/" my/org-directory)
  "Directory for archived org files.")

;; Create necessary directories
(dolist (dir (list my/org-directory
                   my/org-roam-directory
                   my/org-downloads-directory
                   my/org-noter-directory
                   my/org-archive-directory
                   (expand-file-name "attachments/" my/org-directory)
                   (expand-file-name "projects/" my/org-directory)
                   (expand-file-name "reviews/" my/org-directory)
                   (expand-file-name "backups/" my/org-directory)))
  (unless (file-directory-p dir)
    (make-directory dir t)))
#+end_src

** Core Setup
#+begin_src emacs-lisp
(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         ;; (org-mode . flyspell-mode)
         ;; (org-mode . org-cdlatex-mode)
         (org-mode . abbrev-mode)
         (org-mode . auto-fill-mode))
  :custom
  
  ;; Basic settings
  (org-directory my/org-directory)
  (org-default-notes-file (expand-file-name "inbox.org" my/org-directory))
  (org-agenda-files (list my/org-directory))
  (org-archive-location (concat my/org-archive-directory "%s_archive::"))

  ;; Startup and display
  (org-startup-folded 'overview)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-startup-with-latex-preview t)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-ellipsis " ⤵")
  (org-hide-emphasis-markers t)

    ;; Behavior
  (org-use-property-inheritance t)
  (org-cycle-separator-lines 2)
  ;;(org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (org-insert-heading-respect-content t)
  (org-catch-invisible-edits 'smart)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-yank-adjusted-subtrees t)
  (org-yank-folded-subtrees nil)
  (org-M-RET-may-split-line '((default . t)))
  (org-return-follows-link t)
  (org-mouse-1-follows-link t)
  (org-use-speed-commands t)

    ;; Lists and checkboxes
  (org-list-allow-alphabetical t)
  (org-list-automatic-rules '((bullet . t) (checkbox . t) (indent . t)))
  (org-checkbox-statistics-hook '(org-update-parent-todo-statistics))
  (org-hierarchical-todo-statistics t)

  ;; Source blocks
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)

  ;; Export
  (org-export-in-background t)
  (org-export-with-smart-quotes t)
  (org-export-with-sub-superscripts nil)
  (org-export-with-toc nil)
  (org-export-headline-levels 4)
  (org-export-coding-system 'utf-8)

   ;; Links
  (org-link-elisp-confirm-function nil)

    ;; Performance
  (org-agenda-inhibit-startup t)
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-use-tag-inheritance nil)
  (org-agenda-ignore-properties '(effort appt category))
  (org-fontify-whole-heading-line nil)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-element-cache-persistent t)
  (org-element-use-cache t)

  ;; Tag selection
  (org-fast-tag-selection-single-key t)
  
  :config
  ;; File associations
  (add-to-list 'org-file-apps '("\\.pdf\\'" . default))
  (add-to-list 'org-file-apps '("\\.png\\'" . default))
  (add-to-list 'org-file-apps '("\\.jpg\\'" . default))
  
  ;; Refile configuration
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)
 
  ;; Todo keywords
  (setq org-todo-keywords
        '((sequence "☛ TODO(t)" "⚡ NEXT(n)" "🔄 PROG(p)" "⏳ WAIT(w@/!)" 
                    "|" "✅ DONE(d!)" "❌ CANCELLED(c@)")
          (sequence "📋 PLAN(P)" "🔍 RESEARCH(R)" "📝 DRAFT(D)" 
                    "|" "📤 PUBLISHED(u)" "🗑 TRASH(T)")
          (sequence "🎯 GOAL(G)" "🚀 ACTIVE(A)" "⏸ PAUSED(x)" 
                    "|" "🏆 ACHIEVED(a)" "🚫 DROPPED(X)")))
  
  ;; Todo keyword faces
  (setq org-todo-keyword-faces
        '(("☛ TODO" . (:foreground "#fb4934" :weight bold))
          ("⚡ NEXT" . (:foreground "#fabd2f" :weight bold))
          ("🔄 PROG" . (:foreground "#83a598" :weight bold))
          ("⏳ WAIT" . (:foreground "#d3869b" :weight bold))
          ("✅ DONE" . (:foreground "#b8bb26" :weight bold))
          ("❌ CANCELLED" . (:foreground "#928374" :weight bold))
          ("📋 PLAN" . (:foreground "#8ec07c" :weight bold))
          ("🔍 RESEARCH" . (:foreground "#fe8019" :weight bold))
          ("📝 DRAFT" . (:foreground "#d65d0e" :weight bold))
          ("📤 PUBLISHED" . (:foreground "#689d6a" :weight bold))
          ("🗑 TRASH" . (:foreground "#928374" :weight bold))
          ("🎯 GOAL" . (:foreground "#b16286" :weight bold))
          ("🚀 ACTIVE" . (:foreground "#d79921" :weight bold))
          ("⏸ PAUSED" . (:foreground "#7c6f64" :weight bold))
          ("🏆 ACHIEVED" . (:foreground "#689d6a" :weight bold))
          ("🚫 DROPPED" . (:foreground "#665c54" :weight bold))))
  
  ;; Priority faces
  (setq org-priority-faces 
        '((?A . (:foreground "#fb4934" :weight bold))
          (?B . (:foreground "#fabd2f" :weight bold))
          (?C . (:foreground "#83a598" :weight bold))))
  
  )
#+end_src

** Configure Babel Languages
#+begin_src emacs-lisp
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)))
  
  (push '("conf-unix" . conf-unix) org-src-lang-modes))
#+end_src

** Structure Templates
#+begin_src emacs-lisp
(use-package org-tempo
  :ensure nil
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("ltx" . "src latex"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))
#+end_src

** Org Modules
#+begin_src emacs-lisp
(use-package org-id
  :ensure nil
  :after org
  :custom
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-id-locations-file (expand-file-name ".org-id-locations" my/org-directory))
  :config
  (org-id-update-id-locations))

(use-package org-protocol
  :ensure nil
  :after org)

(use-package org-habit
  :ensure nil
  :after org
  :custom
  (org-habit-graph-column 60)
  (org-habit-preceding-days 21)
  (org-habit-following-days 7)
  (org-habit-show-habits-only-for-today t))

(use-package org-clock
  :ensure nil
  :after org
  :custom
  (org-clock-persist t)
  (org-clock-persist-query-resume nil)
  (org-clock-in-resume t)
  (org-clock-report-include-clocking-task t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-auto-clock-resolution 'when-no-clock-is-running)
  (org-show-notification-handler 'message)
  (org-clock-clocked-in-display 'mode-line)
  :config
  (org-clock-persistence-insinuate))

(use-package org-timer
  :ensure nil
  :after org
  :custom
  (org-timer-default-timer 25))

(use-package org-archive
  :ensure nil
  :after org
  :custom
  (org-archive-default-command 'org-archive-subtree-default-with-confirmation)
  (org-archive-subtree-add-inherited-tags t))

(use-package org-attach
  :ensure nil
  :after org
  :custom
  (org-attach-directory (expand-file-name "attachments/" my/org-directory))
  (org-attach-store-link-p 'attached)
  (org-attach-auto-tag "attachment"))
#+end_src

** Org Agenda
#+begin_src emacs-lisp
(use-package org-agenda
  :ensure nil
  :after org
  :custom
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday nil)
  (org-agenda-start-day "today")
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-include-deadlines t)
  (org-agenda-block-separator ?─)
  (org-agenda-compact-blocks t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-log-mode-items '(closed clock state))
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2))
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000)
                          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string "◀── now ─────────────────────────────────────────────────")
  
  :config
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)
                        (org-agenda-overriding-header "📅 Agenda")))
            (todo "⚡ NEXT" ((org-agenda-overriding-header "⚡ Next Tasks")))
            (tags-todo "project/🚀 ACTIVE" ((org-agenda-overriding-header "🚀 Active Projects")))
            (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "🔥 High Priority")))
            (todo "⏳ WAIT" ((org-agenda-overriding-header "⏳ Waiting On")))
            (tags-todo "+habit" ((org-agenda-overriding-header "🔄 Habits")))
            (stuck "" ((org-agenda-overriding-header "🚫 Stuck Projects")))))
          
          ("n" "Next Tasks"
           ((todo "⚡ NEXT" ((org-agenda-overriding-header "⚡ Next Tasks")))))
          
          ("w" "Work Context" 
           ((tags-todo "@work/⚡ NEXT" ((org-agenda-overriding-header "💼 Work Next")))
            (tags-todo "@work/☛ TODO" ((org-agenda-overriding-header "💼 Work Tasks")))
            (tags-todo "@work+project/🚀 ACTIVE" ((org-agenda-overriding-header "💼 Work Projects")))))
          
          ("h" "Home Context"
           ((tags-todo "@home/⚡ NEXT" ((org-agenda-overriding-header "🏠 Home Next")))
            (tags-todo "@home/☛ TODO" ((org-agenda-overriding-header "🏠 Home Tasks")))))
          
          ("p" "Projects Overview"
           ((tags "project" ((org-agenda-overriding-header "📋 All Projects")))))
          
          ("g" "Goals Review"
           ((tags-todo "goal" ((org-agenda-overriding-header "🎯 Goals")))))
          
          ("r" "Review"
           ((agenda "" ((org-agenda-span 'day) (org-agenda-overriding-header "📅 Today")))
            (todo "✅ DONE" ((org-agenda-overriding-header "✅ Completed Today")
                             (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottoday))))
            (stuck "" ((org-agenda-overriding-header "🚫 Stuck Projects"))))))))

(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "🔥 Overdue" :deadline past)
          (:name "📅 Today" :time-grid t :scheduled today)
          (:name "⚡ Next" :todo "⚡ NEXT")
          (:name "🔴 Important" :priority "A")
          (:name "📋 Projects" :tag "project")
          (:name "🏠 Home" :tag "@home")
          (:name "💼 Work" :tag "@work")
          (:name "⏳ Waiting" :todo "⏳ WAIT")
          (:name "📚 Reading" :tag "read")
          (:name "🎯 Goals" :tag "goal")
          (:name "🔄 Habits" :tag "habit")
          (:discard (:anything t)))))
#+end_src

** Org Capture
#+begin_src emacs-lisp
(use-package org-capture
  :ensure nil
  :after org
  :custom
  (org-capture-templates
   '(("t" "Task" entry (file+headline "~/org/inbox.org" "Tasks")
      "* ☛ TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")
     
     ("n" "Note" entry (file+headline "~/org/inbox.org" "Notes")
      "* %? :note:\n  :PROPERTIES:\n  :CREATED: %U\n  :SOURCE: \n  :END:\n")
     
     ("j" "Journal" entry (file+datetree "~/org/journal.org")
      "* %U %?\n")
     
     ("m" "Meeting" entry (file+headline "~/org/inbox.org" "Meetings")
      "* Meeting: %? :meeting:\n  :PROPERTIES:\n  :CREATED: %U\n  :ATTENDEES: \n  :END:\n** Agenda\n** Notes\n** Action Items\n")
     
     ("p" "Project" entry (file+headline "~/org/projects.org" "Projects")
      "* 📋 PLAN %? :project:\n  :PROPERTIES:\n  :CREATED: %U\n  :GOAL: \n  :DEADLINE: \n  :END:\n** Goals\n** Tasks\n*** ☛ TODO Define project scope\n** Resources\n** Notes\n")
     
     ("b" "Book" entry (file+headline "~/org/reading.org" "Reading List")
      "* %? :book:read:\n  :PROPERTIES:\n  :CREATED: %U\n  :AUTHOR: \n  :GENRE: \n  :PAGES: \n  :STARTED: \n  :FINISHED: \n  :RATING: \n  :END:\n** Summary\n** Key Takeaways\n** Quotes\n")
     
     ("h" "Habit" entry (file+headline "~/org/habits.org" "Habits")
      "* ☛ TODO %? :habit:\n  SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d>>\")\n  :PROPERTIES:\n  :CREATED: %U\n  :STYLE: habit\n  :END:\n")
     
     ("i" "Idea" entry (file+headline "~/org/ideas.org" "Ideas")
      "* %? :idea:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")
     
     ("l" "Link" entry (file+headline "~/org/links.org" "Links")
      "* %? :link:\n  :PROPERTIES:\n  :CREATED: %U\n  :URL: \n  :END:\n")
     
     ("w" "Work Task" entry (file+headline "~/org/work.org" "Work Tasks")
      "* ☛ TODO %? :@work:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")
     
     ("g" "Goal" entry (file+headline "~/org/goals.org" "Goals")
      "* 🎯 GOAL %? :goal:\n  DEADLINE: %(org-read-date nil nil \"+1y\")\n  :PROPERTIES:\n  :CREATED: %U\n  :TYPE: \n  :END:\n** Why this goal?\n** Success criteria\n** Action steps\n*** ☛ TODO Break down into smaller tasks\n** Resources needed\n** Potential obstacles\n** Progress tracking\n"))))

#+end_src

** Org Roam
*** Core Configuration
#+begin_src emacs-lisp
(use-package org-roam
  :after org
  :custom
  (org-roam-directory my/org-roam-directory)
  (org-roam-db-location (expand-file-name "org-roam.db" my/org-directory))
  (org-roam-completion-everywhere t)
  (org-roam-verbose nil)
  
  ;; Enhanced node display template with timestamps
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:20}" 'face 'org-tag)
           " "
           (propertize "${hierarchy:30}" 'face 'font-lock-comment-face)
           " "
           (propertize "${file:15}" 'face 'font-lock-keyword-face)))
  
  ;; Graph visualization with Gruvbox colors
  (org-roam-graph-executable "dot")
  (org-roam-graph-extra-config 
   '(("overlap" . "scale")
     ("splines" . "true")
     ("rankdir" . "LR")
     ("concentrate" . "true")
     ("bgcolor" . "#282828")      ; Gruvbox dark background
     ("fontcolor" . "#ebdbb2")    ; Gruvbox light foreground
     ("fontname" . "monospace")
     ("fontsize" . "10")
     ("ratio" . "compress")
     ("ranksep" . "0.5")
     ("nodesep" . "0.3")))
 
  :config
  ;; Enable automatic database synchronization
  (org-roam-db-autosync-mode 1)
  
  ;; Enhanced capture templates with timestamps
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+created: %U\n#+modified: %U\n#+filetags:\n\n")
           :unnarrowed t)
          
          ("n" "note" plain "* Overview\n\n%?\n\n* Key Points\n\n* Related Notes\n\n* References\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+created: %U\n#+modified: %U\n#+filetags: note\n\n")
           :unnarrowed t)
          
          ("p" "project" plain "* Project Overview\n\n%?\n\n* Objectives\n\n* Timeline\n\n* Milestones\n\n* Tasks\n\n** TODO Define project requirements\n** TODO Create project plan\n** TODO Set up project structure\n\n* Resources\n\n* Stakeholders\n\n* Notes\n\n* Status Updates\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+created: %U\n#+modified: %U\n#+filetags: project\n\n")
           :unnarrowed t)
          
          ("k" "book" plain "* Book Information\n\n- Author: \n- Published: \n- Pages: \n- ISBN: \n- Genre: \n- Rating: /5\n- Status: \n- Date Started: \n- Date Finished: \n\n* Summary\n\n%?\n\n* Key Insights\n\n* Important Quotes\n\n* Chapter Notes\n\n* Personal Reflections\n\n* Action Items\n\n* Related Books\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+created: %U\n#+modified: %U\n#+filetags: book literature\n\n")
           :unnarrowed t)
          
          ("a" "article" plain "* Article Information\n\n- Author(s): \n- Published: \n- Source: \n- URL: \n- Type: \n- Reading Date: %U\n\n* Summary\n\n%?\n\n* Key Points\n\n* Methodology (if applicable)\n\n* Findings\n\n* Personal Comments\n\n* Quotes\n\n* Follow-up Reading\n\n* Related Articles\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+created: %U\n#+modified: %U\n#+filetags: article research\n\n")
           :unnarrowed t)
          
          ("r" "reference" plain "* Overview\n\n%?\n\n* Definition\n\n* Key Characteristics\n\n* Usage\n\n* Examples\n\n* Best Practices\n\n* Common Pitfalls\n\n* Related Topics\n\n* External Resources\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+created: %U\n#+modified: %U\n#+filetags: reference\n\n")
           :unnarrowed t)
          
          ("c" "concept" plain "* Definition\n\n%?\n\n* Core Principles\n\n* Key Components\n\n* Examples\n\n* Applications\n\n* Use Cases\n\n* Advantages\n\n* Limitations\n\n* Related Concepts\n\n* Further Reading\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+created: %U\n#+modified: %U\n#+filetags: concept\n\n")
           :unnarrowed t)
          
          ("m" "meeting" plain "* Meeting Information\n\n- Date: %U\n- Duration: \n- Location: \n- Meeting Type: \n- Organizer: \n\n* Attendees\n\n%?\n\n* Agenda\n\n* Discussion Points\n\n* Key Decisions\n\n* Action Items\n\n** TODO Review meeting notes\n** TODO Follow up on action items\n\n* Next Steps\n\n* Meeting Notes\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+created: %U\n#+modified: %U\n#+filetags: meeting\n\n")
           :unnarrowed t)
          
          ("j" "idea" plain "* The Idea\n\n%?\n\n* Background\n\n* Problem it Solves\n\n* Potential Solutions\n\n* Implementation Ideas\n\n* Resources Required\n\n* Timeline\n\n* Challenges\n\n* Success Criteria\n\n* Next Actions\n\n* Related Ideas\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+created: %U\n#+modified: %U\n#+filetags: idea brainstorm\n\n")
           :unnarrowed t)
          
          ("t" "tutorial" plain "* Tutorial Overview\n\n%?\n\n* Prerequisites\n\n* Learning Objectives\n\n* Steps\n\n** Step 1\n\n** Step 2\n\n** Step 3\n\n* Code Examples\n\n* Common Issues\n\n* Tips and Tricks\n\n* Additional Resources\n\n* Related Tutorials\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+created: %U\n#+modified: %U\n#+filetags: tutorial learning\n\n")
           :unnarrowed t)
          
          ("w" "workflow" plain "* Workflow Description\n\n%?\n\n* Purpose\n\n* Triggers\n\n* Steps\n\n** Step 1: \n** Step 2: \n** Step 3: \n\n* Tools Required\n\n* Expected Outcomes\n\n* Optimization Notes\n\n* Troubleshooting\n\n* Related Workflows\n\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+created: %U\n#+modified: %U\n#+filetags: workflow process\n\n")
           :unnarrowed t)
          
          ("I" "inbox" plain "* %?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+created: %U\n#+modified: %U\n#+filetags: inbox\n\n")
           :unnarrowed t))))
#+end_src

*** Org Roam Timestamps
#+begin_src emacs-lisp
(use-package org-roam-timestamps
  :after org-roam
  :config
  (org-roam-timestamps-mode 1)
  :custom
  (org-roam-timestamps-parent-file t)
  (org-roam-timestamps-remember-timestamps t)
  (org-roam-timestamps-minimum-gap 3600)) ; 1 hour minimum gap
#+end_src

*** Org Roam Query
#+begin_src emacs-lisp
(use-package org-roam-ql
  :after org-roam
  :config
  ;; Custom QL predicates for enhanced searching
  (setq org-roam-ql-default-org-roam-buffer-display-function
        #'org-roam-buffer-display-dedicated))

(use-package org-roam-ql-ql
  :after (org-roam org-roam-ql)
  :config
  ;; Enhanced QL query interface
  (setq org-roam-ql-ql-default-completions
        '("tags" "title" "content" "file" "links" "backlinks" "refs")))
#+end_src

*** Embark Integration
#+begin_src emacs-lisp
(use-package embark-org-roam
  :after (embark org-roam)
  :config
  ;; Add org-roam actions to embark
  (add-to-list 'embark-keymap-alist '(org-roam-ref . embark-org-roam-ref-map))
  
  ;; Custom embark actions
  (defun my/embark-org-roam-node-open-with (node)
    "Open org-roam NODE with external application."
    (find-file-other-window (org-roam-node-file node)))
  
  (defun my/embark-org-roam-node-copy-id (node)
    "Copy org-roam NODE id to kill ring."
    (kill-new (org-roam-node-id node))
    (message "Copied node ID: %s" (org-roam-node-id node)))) 
#+end_src

*** Org Roam UI
#+begin_src emacs-lisp
(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  (org-roam-ui-port 35901))
#+end_src

*** Consult Integration
#+begin_src emacs-lisp
(use-package consult-org-roam
  :after (consult org-roam)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r)
  (consult-org-roam-buffer-after-buffers t)
  :config
  (when (fboundp 'consult-org-roam-mode)
    (consult-org-roam-mode 1)))
#+end_src

*** Utility Functions
#+begin_src emacs-lisp
(defun my/org-roam-filter-by-tag (tag-name)
  "Create filter function for nodes with TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  "Return list of note files with TAG-NAME."
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-find-by-tag (tag-name)
  "Find and open nodes with TAG-NAME."
  (org-roam-node-find nil nil (my/org-roam-filter-by-tag tag-name)))

(defun my/org-roam-find-project ()
  "Find and open project nodes."
  (interactive)
  (my/org-roam-find-by-tag "project"))

(defun my/org-roam-find-book ()
  "Find and open book nodes."
  (interactive)
  (my/org-roam-find-by-tag "book"))

(defun my/org-roam-find-concept ()
  "Find and open concept nodes."
  (interactive)
  (my/org-roam-find-by-tag "concept"))

(defun my/org-roam-find-meeting ()
  "Find and open meeting nodes."
  (interactive)
  (my/org-roam-find-by-tag "meeting"))

(defun my/org-roam-find-idea ()
  "Find and open idea nodes."
  (interactive)
  (my/org-roam-find-by-tag "idea"))

(defun my/org-roam-find-inbox ()
  "Find and open inbox nodes."
  (interactive)
  (my/org-roam-find-by-tag "inbox"))

(defun my/org-roam-capture-inbox ()
  "Capture a new note in inbox for later processing."
  (interactive)
  (org-roam-capture :node (org-roam-node-create)
                    :templates '(("i" "inbox" plain "* %?"
                                  :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                     "#+title: ${title}\n#+date: %U\n#+created: %U\n#+modified: %U\n#+filetags: inbox\n\n")
                                  :unnarrowed t))))

(defun my/org-roam-node-insert-immediate (arg &rest args)
  "Insert org-roam node immediately without opening capture buffer."
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-tag-new ()
  "Add a new tag to current node."
  (interactive)
  (org-roam-tag-add (list (read-string "Tag: "))))

(defun my/org-roam-update-org-id-locations ()
  "Update org-id locations for org-roam."
  (interactive)
  (org-id-update-id-locations (org-roam-list-files)))

(defun my/org-roam-maintenance ()
  "Run comprehensive org-roam maintenance tasks."
  (interactive)
  (message "Running org-roam maintenance...")
  (org-roam-db-sync)
  (my/org-roam-update-org-id-locations)
  (when (fboundp 'org-roam-timestamps-update-all)
    (org-roam-timestamps-update-all))
  (message "Org-roam maintenance completed."))

#+end_src

*** Advanced Query Functions
#+begin_src emacs-lisp
(defun my/org-roam-ql-search-recent ()
  "Search for recently modified nodes using org-roam-ql."
  (interactive)
  (org-roam-ql-search
   "(and (file-modified-time >) (days-ago 7))"))

(defun my/org-roam-ql-search-orphans ()
  "Find orphaned nodes (no backlinks) using org-roam-ql."
  (interactive)
  (org-roam-ql-search
   "(not (backlinks))"))

(defun my/org-roam-ql-search-highly-connected ()
  "Find highly connected nodes using org-roam-ql."
  (interactive)
  (org-roam-ql-search
   "(> (length (backlinks)) 5)"))

(defun my/org-roam-ql-search-todos ()
  "Find nodes with TODO items using org-roam-ql."
  (interactive)
  (org-roam-ql-search
   "(todo)"))
#+end_src

*** Timestamp Update Hooks
#+begin_src emacs-lisp
(defun my/org-roam-update-modified-timestamp ()
  "Update modified timestamp in org-roam files."
  (when (org-roam-file-p)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+modified:" nil t)
        (delete-region (point) (line-end-position))
        (insert (format " %s" (format-time-string "[%Y-%m-%d %a %H:%M]")))))))

(add-hook 'before-save-hook #'my/org-roam-update-modified-timestamp)
#+end_src

*** Buffer Display Configuration
#+begin_src emacs-lisp
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.35)
               (window-height . fit-window-to-buffer)))

(add-to-list 'display-buffer-alist
             '("\\*org-roam-ql\\*"
               (display-buffer-in-direction)
               (direction . below)
               (window-height . 0.3)))
#+end_src

*** Roam Buffer Sections
#+begin_src emacs-lisp
(with-eval-after-load 'org-roam
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section)))
#+end_src

*** Completion Configuration
#+begin_src emacs-lisp
(add-hook 'org-roam-find-file-hook
          (lambda ()
            (when (org-roam-file-p)
              (setq-local completion-at-point-functions
                          (append completion-at-point-functions
                                  (list #'org-roam-completion-at-point))))))
#+end_src

*** Performance Optimization
#+begin_src emacs-lisp
(setq org-roam-db-gc-threshold most-positive-fixnum
      org-roam-db-update-on-save t)

(setq org-roam-ql-default-org-roam-buffer-display-function
      #'org-roam-buffer-display-dedicated)
#+end_src

** ☛ TODO Org Download
=Update screenshot method=
#+begin_src emacs-lisp
(use-package org-download
  :after org
  :hook (dired-mode . org-download-enable)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir my/org-downloads-directory)
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-download-screenshot-method "flameshot gui --raw > %s")
  (org-download-annotate-function (lambda (_link) "#+ATTR_ORG: :width 300\n"))
  (org-download-image-attr-list '("#+ATTR_HTML: :width 80% :align center"
                                  "#+ATTR_ORG: :width 300")))
#+end_src

** Org Noter
#+begin_src emacs-lisp
(use-package org-noter
  :after org
  :custom
  (org-noter-notes-search-path (list my/org-noter-directory))
  (org-noter-separate-notes-from-heading t)
  (org-noter-always-create-document-property t)
  (org-noter-insert-note-no-questions t)
  (org-noter-use-indirect-buffer t)
  (org-noter-kill-frame-at-session-end nil)
  (org-noter-auto-save-last-location t)
  (org-noter-default-heading-title "Notes for page $p$")
  (org-noter-hide-other t)
  (org-noter-closest-tipping-point 0.3))
#+end_src

** Markdown to Org
#+begin_src emacs-lisp
(defun md-to-org (md-file &optional org-file)
  "Convert a Markdown file to Org mode format using pandoc.
MD-FILE is the input Markdown file path.
ORG-FILE is the optional output Org file path. If not provided,
it will use the same name as MD-FILE but with .org extension."
  (interactive "fMarkdown file: ")
  (let* ((input-file (expand-file-name md-file))
         (output-file (or org-file
                         (concat (file-name-sans-extension input-file) ".org")))
         (pandoc-cmd (format "pandoc -f markdown -t org %s -o %s"
                            (shell-quote-argument input-file)
                            (shell-quote-argument output-file))))
    (unless (executable-find "pandoc")
      (error "Pandoc not found. Please install pandoc to use this function"))
    (unless (file-exists-p input-file)
      (error "Input file does not exist: %s" input-file))
    (when (file-exists-p output-file)
      (unless (y-or-n-p "Output file exists. Overwrite? ")
        (error "Conversion cancelled")))
    (message "Converting %s to %s..." input-file output-file)
    (let ((result (shell-command pandoc-cmd)))
      (if (= result 0)
          (progn
            (message "Successfully converted to %s" output-file)
            (when (y-or-n-p "Open the converted file? ")
              (find-file output-file)))
        (error "Pandoc conversion failed with exit code %d" result)))))

(defun md-to-org-current-buffer ()
  "Convert the current Markdown buffer to Org mode format.
Saves the current buffer first if modified, then converts it."
  (interactive)
  (unless (buffer-file-name)
    (error "Buffer is not associated with a file"))
  (when (buffer-modified-p)
    (if (y-or-n-p "Buffer modified. Save before converting? ")
        (save-buffer)
      (error "Please save the buffer before converting")))
  (md-to-org (buffer-file-name)))

(defun md-to-org-region (start end)
  "Convert the selected region from Markdown to Org mode format using pandoc.
START and END define the region boundaries.
The converted text replaces the original region."
  (interactive "r")
  (unless (executable-find "pandoc")
    (error "Pandoc not found. Please install pandoc to use this function"))
  (unless (use-region-p)
    (error "No region selected"))
  (when (= start end)
    (error "Empty region selected"))
  
  (let* ((md-text (buffer-substring-no-properties start end))
         (temp-md-file (make-temp-file "md-to-org-" nil ".md"))
         (temp-org-file (make-temp-file "md-to-org-" nil ".org"))
         (pandoc-cmd (format "pandoc -f markdown -t org %s -o %s"
                            (shell-quote-argument temp-md-file)
                            (shell-quote-argument temp-org-file))))
    (unwind-protect
        (progn
          ;; Write markdown text to temp file with proper encoding
          (with-temp-file temp-md-file
            (insert md-text))
          ;; Convert using pandoc
          (let ((result (shell-command pandoc-cmd)))
            (if (= result 0)
                (if (file-exists-p temp-org-file)
                    (let ((org-text (with-temp-buffer
                                     (insert-file-contents temp-org-file)
                                     (buffer-string))))
                      ;; Remove trailing newline if original text didn't have one
                      (when (and (> (length org-text) 0)
                                 (not (string-suffix-p "\n" md-text))
                                 (string-suffix-p "\n" org-text))
                        (setq org-text (substring org-text 0 -1)))
                      ;; Replace the region
                      (save-excursion
                        (delete-region start end)
                        (goto-char start)
                        (insert org-text))
                      (message "Region converted from Markdown to Org mode"))
                  (error "Pandoc output file not created"))
              (error "Pandoc conversion failed with exit code %d" result))))
      ;; Clean up temp files
      (when (file-exists-p temp-md-file)
        (delete-file temp-md-file))
      (when (file-exists-p temp-org-file)
        (delete-file temp-org-file)))))

(defun md-to-org-region-to-new-buffer (start end)
  "Convert the selected region from Markdown to Org mode and show in new buffer.
START and END define the region boundaries.
Creates a new buffer with the converted content instead of replacing the region."
  (interactive "r")
  (unless (executable-find "pandoc")
    (error "Pandoc not found. Please install pandoc to use this function"))
  (unless (use-region-p)
    (error "No region selected"))
  (when (= start end)
    (error "Empty region selected"))
  
  (let* ((md-text (buffer-substring-no-properties start end))
         (temp-md-file (make-temp-file "md-to-org-" nil ".md"))
         (temp-org-file (make-temp-file "md-to-org-" nil ".org"))
         (pandoc-cmd (format "pandoc -f markdown -t org %s -o %s"
                            (shell-quote-argument temp-md-file)
                            (shell-quote-argument temp-org-file))))
    (unwind-protect
        (progn
          ;; Write markdown text to temp file
          (with-temp-file temp-md-file
            (insert md-text))
          ;; Convert using pandoc
          (let ((result (shell-command pandoc-cmd)))
            (if (= result 0)
                (if (file-exists-p temp-org-file)
                    (let ((org-text (with-temp-buffer
                                     (insert-file-contents temp-org-file)
                                     (buffer-string))))
                      ;; Create new buffer with converted content
                      (with-current-buffer (get-buffer-create "*Markdown to Org Conversion*")
                        (erase-buffer)
                        (insert org-text)
                        (org-mode)
                        (goto-char (point-min))
                        (display-buffer (current-buffer)))
                      (message "Converted text displayed in new buffer"))
                  (error "Pandoc output file not created"))
              (error "Pandoc conversion failed with exit code %d" result))))
      ;; Clean up temp files
      (when (file-exists-p temp-md-file)
        (delete-file temp-md-file))
      (when (file-exists-p temp-org-file)
        (delete-file temp-org-file)))))
#+end_src

* Spell Check
#+begin_src emacs-lisp
(use-package popup :defer t)

(use-package flyspell
  :ensure nil
  :init
  ;; Hunspell configuration
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US")
  
  (when (executable-find "hunspell")
    (setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
            ("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8))))
  
  ;; Enhanced verification function with better org-mode support
  (defun my/flyspell-verify ()
    "Verify if flyspell should check current position.
Properly handles org-mode headlines and other elements."
    (cond
     ;; Org-mode specific checks
     ((derived-mode-p 'org-mode)
      (let* ((element (org-element-at-point))
             (element-type (org-element-type element))
             (context (org-element-context element)))
        (and 
         ;; Skip headlines entirely
         (not (eq element-type 'headline))
         ;; Skip various org elements where spell checking doesn't make sense
         (not (memq element-type '(src-block inline-src-block example-block
                                  fixed-width export-block latex-environment
                                  latex-fragment link property-drawer keyword
                                  planning clock timestamp comment-block
                                  diary-sexp horizontal-rule table table-row
                                  table-cell node-property babel-call)))
         ;; For paragraphs, check if we're inside special inline elements
         (if (eq element-type 'paragraph)
             (let ((context-type (org-element-type context)))
               (not (memq context-type '(link timestamp code verbatim
                                        latex-fragment entity export-snippet
                                        footnote-reference macro statistics-cookie
                                        inline-babel-call inline-src-block))))
           t)
         ;; Skip org-mode markup at point
         (not (save-excursion
                (let ((bounds (bounds-of-thing-at-point 'symbol)))
                  (and bounds
                       (goto-char (car bounds))
                       (or (looking-at "\\[\\[")          ; Links
                           (looking-at "<<")              ; Targets
                           (looking-at "{{{")             ; Macros
                           (looking-at "#\\+")            ; Keywords
                           (looking-at ":[[:alnum:]_@#%:]+:") ; Properties
                           (looking-at "\\*+\\s-")        ; Headlines
                           (looking-at "^\\s-*-\\s-")     ; List items
                           (looking-at "^\\s-*[0-9]+[.)]\\s-") ; Numbered lists
                           (looking-at "|")               ; Tables
                           (looking-at ":")               ; Drawers
                           ))))))))
     
     ;; Programming mode checks - spell check only in comments and strings
     ((derived-mode-p 'prog-mode)
      (let ((faces (get-text-property (point) 'face)))
        (when faces
          (unless (listp faces) (setq faces (list faces)))
          (seq-some (lambda (f) 
                      (memq f '(font-lock-comment-face font-lock-comment-delimiter-face
                               font-lock-string-face font-lock-doc-face))) faces))))
     
     ;; For all other modes, enable spell checking
     (t t)))
  
  ;; Optimized popup correction function
  (defun my/flyspell-popup-correct (event poss word)
    "Show flyspell correction menu using popup-el with better UX."
    (require 'popup)
    (let* ((corrects (if flyspell-sort-corrections
                         (sort (nth 2 poss) 'string<)
                       (nth 2 poss)))
           ;; Limit corrections to avoid overwhelming menu
           (limited-corrects (if (> (length corrects) 15)
                               (append (seq-take corrects 15) '("..."))
                             corrects))
           (menu-items (append 
                       (mapcar (lambda (c) (cons c c)) limited-corrects)
                       '(("" . separator)
                         ("Save word" . save)
                         ("Accept (session)" . session)
                         ("Accept (buffer)" . buffer)
                         ("Skip word" . skip))))
           (choice (popup-menu* (mapcar (lambda (item)
                                         (if (eq (cdr item) 'separator)
                                             '("──────────────" . separator)
                                           item))
                                       menu-items))))
      (when choice
        (let ((action (cdr (assoc choice menu-items))))
          (cond
           ((stringp action)
            (if (string= action "...")
                (message "More corrections available. Use M-$ for full list.")
              (flyspell-do-correct action poss word (point) action)))
           ((eq action 'skip)
            (message "Skipped word: %s" word))
           ((eq action 'separator)
            nil)
           (t
            (flyspell-do-correct action nil word (point) poss)))))))
  
  ;; Function to toggle flyspell in current buffer
  (defun my/toggle-flyspell ()
    "Toggle flyspell mode in current buffer."
    (interactive)
    (if flyspell-mode
        (flyspell-mode -1)
      (flyspell-mode 1))
    (message "Flyspell %s" (if flyspell-mode "enabled" "disabled")))
  
  ;; Function to correct word at point without popup
  (defun my/flyspell-correct-at-point ()
    "Correct word at point using first suggestion."
    (interactive)
    (when flyspell-mode
      (let ((flyspell-use-meta-tab nil))
        (flyspell-correct-word-before-point))))
  
  :config
  ;; Performance and behavior settings
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil
        flyspell-consider-dash-as-word-delimiter-flag t
        flyspell-delay 0.5                          ; Reduced delay for faster checking
        flyspell-lazy-idle-seconds 0.5              ; Reduced idle time
        flyspell-emacs-popup-function 'my/flyspell-popup-correct
        flyspell-generic-check-word-predicate 'my/flyspell-verify
        flyspell-use-meta-tab nil)
  
  ;; Disable flyspell in certain contexts
  (add-hook 'minibuffer-setup-hook (lambda () (flyspell-mode -1)))
  (add-hook 'term-mode-hook (lambda () (flyspell-mode -1)))
  (add-hook 'shell-mode-hook (lambda () (flyspell-mode -1)))
  (add-hook 'eshell-mode-hook (lambda () (flyspell-mode -1)))
  
  ;; Key bindings
  :bind (:map flyspell-mode-map
         ("C-c s" . my/toggle-flyspell)
         ("C-c f" . my/flyspell-correct-at-point)
         ("C-c w" . flyspell-correct-wrapper)
         ("C-M-i" . nil))  ; Disable default M-TAB binding
  
  :hook
  ;; Enable for text modes
  ((text-mode org-mode markdown-mode rst-mode latex-mode tex-mode 
    git-commit-mode message-mode) . flyspell-mode)
  ;; Enable for programming modes (comments/strings only)
  (prog-mode . flyspell-prog-mode))

;; Enhanced Auto-correction Mode
;; =============================
(defcustom my/auto-correct-delay 2.0
  "Delay in seconds before auto-correcting misspelled words."
  :type 'number
  :group 'flyspell)

(defvar my/auto-correct-timer nil 
  "Timer for auto-correction.")

(defvar my/auto-correct-last-point nil
  "Last point where auto-correction was attempted.")

(defun my/auto-correct-word ()
  "Auto-correct the most recent misspelled word if cursor hasn't moved."
  (when (and flyspell-mode 
             (not (minibufferp))
             (eq (point) my/auto-correct-last-point))
    (save-excursion
      (let ((overlay (car (overlays-in (point-min) (point-max)))))
        (when (and overlay (overlay-get overlay 'flyspell-overlay))
          (goto-char (overlay-start overlay))
          (let ((word (thing-at-point 'word t)))
            (when word
              (let ((corrections (flyspell-get-word)))
                (when (and corrections (nth 2 corrections))
                  (let ((first-correction (car (nth 2 corrections))))
                    (when first-correction
                      (flyspell-do-correct first-correction 
                                         corrections word (point) first-correction)
                      (message "Auto-corrected: %s → %s" word first-correction))))))))))))

(defun my/start-auto-correct-timer ()
  "Start or restart the auto-correction timer."
  (when my/auto-correct-timer
    (cancel-timer my/auto-correct-timer))
  (setq my/auto-correct-last-point (point))
  (setq my/auto-correct-timer
        (run-with-idle-timer my/auto-correct-delay nil #'my/auto-correct-word)))

(define-minor-mode my/flyspell-auto-correct-mode
  "Auto-correct flyspell errors after idle delay.
This mode will automatically apply the first correction suggestion
for misspelled words after a specified idle time."
  :lighter " AutoCorrect"
  :global t
  (if my/flyspell-auto-correct-mode
      (progn
        (add-hook 'post-command-hook #'my/start-auto-correct-timer)
        (message "Flyspell auto-correction enabled (%.1fs delay)" my/auto-correct-delay))
    (remove-hook 'post-command-hook #'my/start-auto-correct-timer)
    (when my/auto-correct-timer
      (cancel-timer my/auto-correct-timer)
      (setq my/auto-correct-timer nil))
    (message "Flyspell auto-correction disabled")))

;; Org-mode specific enhancements
;; ==============================
(with-eval-after-load 'org
  ;; Skip spell checking in org-mode specific contexts
  (add-to-list 'ispell-skip-region-alist '("^#\\+begin_src" . "^#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+begin_example" . "^#\\+end_example"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+begin_export" . "^#\\+end_export"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+begin_comment" . "^#\\+end_comment"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_quote" . "#\\+end_quote"))
  
  ;; Function to spell check only org paragraph content
  (defun my/org-flyspell-paragraph-only ()
    "Enable flyspell for org-mode but only in paragraph text."
    (when (derived-mode-p 'org-mode)
      (flyspell-mode 1)))
  
  ;; Better org-mode integration
  (defun my/org-flyspell-setup ()
    "Set up flyspell for org-mode with optimized settings."
    (setq-local flyspell-generic-check-word-predicate 'my/flyspell-verify)
    (setq-local flyspell-delay 0.8)  ; Slightly longer delay for org-mode
    (flyspell-mode 1))
  
  (add-hook 'org-mode-hook #'my/org-flyspell-setup))

;; Performance optimizations
;; ========================
(defun my/flyspell-performance-mode ()
  "Toggle flyspell performance mode for large files."
  (interactive)
  (if (bound-and-true-p my/flyspell-performance-active)
      (progn
        (setq flyspell-delay 0.5
              flyspell-lazy-idle-seconds 0.5)
        (setq my/flyspell-performance-active nil)
        (message "Flyspell performance mode disabled"))
    (setq flyspell-delay 2.0
          flyspell-lazy-idle-seconds 2.0)
    (setq my/flyspell-performance-active t)
    (message "Flyspell performance mode enabled")))

;; Global key bindings
;; ===================
(global-set-key (kbd "C-c s t") 'my/toggle-flyspell)
(global-set-key (kbd "C-c s a") 'my/flyspell-auto-correct-mode)
(global-set-key (kbd "C-c s p") 'my/flyspell-performance-mode)
(global-set-key (kbd "C-c s b") 'flyspell-buffer)
(global-set-key (kbd "C-c s r") 'flyspell-region)
#+end_src

* General Keybindings

#+begin_src emacs-lisp
(use-package general
  :after evil
  :config
  (general-evil-setup)
  (general-create-definer global-leader-key
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")
  
  (general-create-definer local-leader-key
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC m"
    :global-prefix "C-SPC m")
 
  (general-create-definer my/alt-leader-def
    :keymaps '(normal visual)
    :prefix ";"
    :global-prefix "C-;")

  (general-evil-define-key '(normal visual) org-mode-map
    "gsg" 'org-goto-first-src-block
    "gsG" 'org-goto-last-src-block
    "gsj" 'org-goto-next-src-block
    "gsk" 'org-goto-prev-src-block)

  
  (global-leader-key
    "SPC" '(execute-extended-command :wk "M-x")
    "u" '(universal-argument :wk "Universal argument"))

  ;; Buffer Management
  (global-leader-key
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "b b" '(consult-buffer :wk "Switch to buffer")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b d" '(bookmark-delete :wk "Delete bookmark")
    "b i" '(ibuffer :wk "Ibuffer")
    "b I" '(ibuffer-other-window :wk "Ibuffer Other Window")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(kill-some-buffers :wk "Kill multiple buffers")
    "b l" '(list-bookmarks :wk "List bookmarks")
    "b m" '(bookmark-set :wk "Set bookmark")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))

  ;; Direed 
  (global-leader-key
    "d"    '(:ignore t :which-key "dired")
    "d d"  '(dired :which-key "dired")
    "d j"  '(dired-jump :which-key "dired jump")
    "d D"  '(dired-other-window :which-key "dired other window")
    "d J"  '(dired-jump-other-window :which-key "dired jump other window"))

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'ibuffer-mode-map
   "b" 'ibuffer-switch-to-saved-filter-groups 
   "c" 'ibuffer-filter-by-content             
   "e" 'ibuffer-clear-filter-groups           
   "f" 'ibuffer-filter-by-filename            
   "n" 'ibuffer-filter-by-name                
   "p" 'ibuffer-filter-by-predicate           
   "v" 'ibuffer-filter-by-mode                
   "w" 'ibuffer-filter-disable                
   "z" 'ibuffer-toggle-filter-group         
   "j" 'ibuffer-forward-line
   "k" 'ibuffer-backward-line
   "h" 'ibuffer-backward-filter-group
   "l" 'ibuffer-forward-filter-group
   "RET" 'ibuffer-visit-buffer
   "o" 'ibuffer-visit-buffer-other-window
   "C-o" 'ibuffer-visit-buffer-other-window-noselect
   "TAB" 'ibuffer-toggle-filter-group
   "m" 'ibuffer-mark-forward
   "u" 'ibuffer-unmark-forward
   "U" 'ibuffer-unmark-all
   "t" 'ibuffer-toggle-marks
   "d" 'ibuffer-mark-for-delete
   "x" 'ibuffer-do-kill-on-deletion-marks
   "D" 'ibuffer-do-delete
   "S" 'ibuffer-do-save
   "R" 'ibuffer-do-rename-uniquely
   "M" 'ibuffer-do-toggle-modified
   "T" 'ibuffer-do-toggle-read-only
   "L" 'ibuffer-do-toggle-lock
   "g" 'ibuffer-update
   "q" 'quit-window
   "%" 'ibuffer-mark-by-mode
   "*" 'ibuffer-unmark-all
   "C-t" 'ibuffer-visit-tags-table
   "s" 'ibuffer-toggle-sorting-mode
   "r" 'ibuffer-bufler-style-refresh)
  
  ;; Dirvish Keybindings
  (global-leader-key
    "d f"  '(dirvish-fd :wk "find file")
    "d s"  '(dirvish-side :wk "side panel")
    "d q"  '(dirvish-quicksort :wk "quicksort")
    "d y"  '(dirvish-yank :wk "yank")
    "d Y"  '(dirvish-yank-menu :wk "yank menu")
    "d t"  '(dirvish-subtree-toggle :wk "toggle subtree")
    "d T"  '(dirvish-subtree-remove :wk "remove subtree")
    "d e"  '(dirvish-emerge-menu :wk "emerge menu")
    "d m"  '(dirvish-mark-menu :wk "mark menu")
    "d c"  '(dirvish-copy-menu :wk "copy menu")
    "d r"  '(dirvish-rename-menu :wk "rename menu")
    "d D"  '(dirvish-delete-menu :wk "delete menu")
    "d z"  '(dirvish-zip :wk "zip")
    "d Z"  '(dirvish-unzip :wk "unzip")
    "d g"  '(dirvish-goto-menu :wk "goto menu")
    "d G"  '(dirvish-layout-toggle :wk "layout toggle")
    "d v"  '(dirvish-vc-menu :wk "vc menu")
    "d H"  '(dirvish-history-menu :wk "history menu")
    "d A"  '(dirvish-quick-access :wk "quick access")
    "d L"  '(dirvish-ls-switches-menu :wk "ls switches")
    "d o"  '(dired-omit-mode :wk "toggle omit mode")
    "d h"  '(dired-hide-details-mode :wk "toggle details"))

  (global-leader-key
    ;; Evil-specific operations
    "e"   '(:ignore t :wk "evil")
    "e h"  '(evil-ex-nohighlight :wk "clear search highlight")
    "e r"  '(evil-show-registers :wk "show registers")
    "e m"  '(evil-show-marks :wk "show marks")
    "e j"  '(evil-join :wk "join lines")
    "e x"  '(evil-exchange :wk "exchange")
    "e X"  '(evil-exchange-cancel :wk "cancel exchange"))
  
  ;; File navigation 
  (global-leader-key
    "f"    '(:ignore t :wk "files")
    "f d"  '(dired :wk "dired")
    "f f"  '(find-file :wk "find file")
    "f r"  '(consult-recent-file :wk "recent files")
    "f l"  '(consult-locate :wk "locate")
    "f j"  '(dired-jump :wk "dired jump")
    "f D"  '(dired-other-window :wk "dired other window")
    "f J"  '(dired-jump-other-window :wk "dired jump other window"))

  ;; Enhanced local leader bindings for eglot 
  (local-leader-key
    :keymaps 'eglot-mode-map
    "g"   '(:ignore t :which-key "goto (LSP)")
    "g d"  '(eglot-find-declaration :which-key "declaration")
    "g D"  '(eglot-find-implementation :which-key "implementation") 
    "g t"  '(eglot-find-typeDefinition :which-key "type definition")
    "g r"  '(eglot-find-references :which-key "references")
    "g h"  '(eldoc-doc-buffer :which-key "documentation"))
  
  ;; Consolidated imenu keybindings - no duplicates
  (global-leader-key
    ;; Jump category (primary imenu access)
    "j"   '(:ignore t :which-key "jump")
    "j i"  '(imenu :which-key "imenu")
    "j I"  '(consult-imenu :which-key "consult imenu")
    "j l"  '(imenu-list-smart-toggle :which-key "imenu list")
    "j o"  '(consult-outline :which-key "outline")
    "j m"  '(consult-mark :which-key "marks")
    "j M"  '(consult-global-mark :which-key "global marks"))
    
   
  ;; Programming mode local leader bindings
  (local-leader-key
    :keymaps 'prog-mode-map
    "i"   '(:ignore t :which-key "imenu/navigation")
    "i i"  '(imenu :which-key "imenu")
    "i I"  '(consult-imenu :which-key "consult imenu")
    "i l"  '(imenu-list-smart-toggle :which-key "imenu list")
    "i r"  '((lambda () (interactive) 
              (setq imenu--index-alist nil)
              (imenu-rescan)
              (message "Imenu rescanned")) :which-key "rescan")
    "i o"  '(consult-outline :which-key "outline"))

  (global-leader-key
    "l"   '(:ignore t :which-key "layouts")
    "l s"  '(persp-switch :which-key "switch perspective")
    "l c"  '(persp-switch :which-key "create perspective") 
    "l k"  '(my/persp-kill-current :which-key "kill perspective")
    "l r"  '(persp-rename :which-key "rename perspective")
    "l l"  '(my/persp-list-perspectives :which-key "list perspectives")
    "l n"  '(persp-next :which-key "next perspective")
    "l p"  '(persp-prev :which-key "previous perspective")
    "l 1"  '((lambda () (interactive) (my/persp-switch-by-number 1)) :which-key "perspective 1")
    "l 2"  '((lambda () (interactive) (my/persp-switch-by-number 2)) :which-key "perspective 2")
    "l 3"  '((lambda () (interactive) (my/persp-switch-by-number 3)) :which-key "perspective 3")
    "l 4"  '((lambda () (interactive) (my/persp-switch-by-number 4)) :which-key "perspective 4")
    "l 5"  '((lambda () (interactive) (my/persp-switch-by-number 5)) :which-key "perspective 5")
    "l R"  '(my/persp-reset-layout :which-key "reset layout"))
  
  ;; Project.el Keybindings
  (global-leader-key
    "p" '(:ignore t :which-key "project")
    
    ;; Core project operations
    "p p" '(my/project-switch-with-perspective :which-key "switch project")
    "p f" '(my/project-find-file-dwim :which-key "find file")
    "p b" '(my/project-switch-buffer :which-key "switch buffer")
    "p d" '(project-dired :which-key "project dired")
    "p k" '(project-kill-buffers :which-key "kill project buffers")
    "p K" '(my/project-cleanup-buffers :which-key "cleanup non-project buffers")
    "p x" '(my/project-kill-all-buffers :which-key "kill all project buffers")
   
    ;; Search and navigation
    "p s" '(my/project-search-dwim :which-key "search in project")
    "p o" '(my/project-find-other-file :which-key "find other file")
    
    ;; Project tools
    "p c" '(my/project-compile :which-key "compile project")
    "p R" '(my/project-run-task :which-key "run project task")
    "p t" '(my/project-shell :which-key "project terminal")
    "p g" '(my/project-magit :which-key "project magit")
    "p T" '(my/project-treemacs :which-key "project treemacs")
    
    ;; Project management
    "p i" '(my/project-ibuffer :which-key "project ibuffer")
    "p D" '(my/project-discover-and-switch :which-key "discover projects")
    "p B" '(my/project-bookmark :which-key "bookmark project")
    
    ;; File operations
    "p y" '(my/project-copy-file-path :which-key "copy relative path")
    "p Y" '(my/project-copy-file-path-absolute :which-key "copy absolute path")
    
    ;; Project admin
    "p a" '(:ignore t :which-key "admin")
    "p a a" '(project-remember-project :which-key "add project")
    "p a d" '(project-forget-project :which-key "remove project")
    "p a l" '(project-switch-project :which-key "list projects")
    "p a c" '(project-forget-projects-under :which-key "forget projects under")

    
    ;; Version control integration
    "p v" '(:ignore t :which-key "version control")
    "p v d" '(project-vc-dir :which-key "vc directory")
    "p v g" '(my/project-magit :which-key "magit status")
    
    "p w" '(:ignort t :wk "workspace-related")
    "p w a"  '(treemacs-add-project-to-workspace :wk "Add project to workspace")
    "p w r"  '(treemacs-remove-project-from-workspace :wk "Remove project from workspace")
    "p w s"  '(treemacs-switch-workspace :wk "Switch")
    "p w c"  '(treemacs-create-workspace :wk "Create")
    "p w R"  '(treemacs-rename-workspace :wk "Rename")
    "p w d"  '(treemacs-remove-workspace :wk "Delete")
    "p w e"  '(treemacs-edit-workspaces :wk "Edit file"))

  (global-leader-key
    "t"   '(:ignore t :wk "treemacs")
    "t t"  '(my-treemacs/toggle-or-select :wk "Toggle or Select")
    "t f"  '(my-treemacs/find-in-current-project :wk "Find in Project"))
  
  ;; Window Management
  (global-leader-key
    "w"   '(:ignore t :wk "windows")
    "w h"  '(evil-window-left :wk "window left")
    "w j"  '(evil-window-down :wk "window down")
    "w k"  '(evil-window-up :wk "window up")
    "w l"  '(evil-window-right :wk "window right")
    "w s"  '(evil-window-split :wk "split below")
    "w v"  '(evil-window-vsplit :wk "split right")
    "w d"  '(evil-window-delete :wk "delete window")
    "w o"  '(delete-other-windows :wk "delete other windows")
    "w ="  '(balance-windows :wk "balance windows")
    "w x"  '(evil-window-exchange :wk "exchange windows"))

  ;; Text Manipulation
  (global-leader-key
    "x" '(:ignore t :which-key "text manipulation")
    "x j" '(move-text-down :which-key "move text down")
    "x k" '(move-text-up :which-key "move text up"))

  
  ;; Dired-specific keybindings
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "gg" 'beginning-of-buffer
   "G"  'end-of-buffer
   "q"  'quit-window
   "R"  'dired-do-rename
   "D"  'dired-do-delete
   "C"  'dired-do-copy
   "+"  'dired-create-directory
   "m"  'dired-mark
   "u"  'dired-unmark
   "U"  'dired-unmark-all-marks
   "t"  'dired-toggle-marks
   "%"  'dired-mark-files-regexp
   "s"  'dired-sort-toggle-or-edit
   "gr" 'revert-buffer)
  
  ;; Additional dired-x keybindings
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "o" 'dired-omit-mode
   "(" 'dired-hide-details-mode)

  ;; Dirvish-specific keybindings
  (general-define-key
   :keymaps 'dirvish-mode-map
   :states 'normal
   "TAB" 'dirvish-subtree-toggle
   "S-TAB" 'dirvish-subtree-remove
   "M-j" 'dirvish-fd-jump
   "M-s" 'dirvish-quicksort
   "M-y" 'dirvish-yank-menu
   "M-e" 'dirvish-emerge-menu
   "M-m" 'dirvish-mark-menu
   "M-c" 'dirvish-copy-menu
   "M-r" 'dirvish-rename-menu
   "M-d" 'dirvish-delete-menu
   "M-g" 'dirvish-goto-menu
   "M-v" 'dirvish-vc-menu
   "M-h" 'dirvish-history-menu
   "M-a" 'dirvish-quick-access
   "M-l" 'dirvish-ls-switches-menu
   "f" 'dirvish-file-info-menu
   "F" 'dirvish-toggle-fullscreen
   "y" 'dirvish-yank
   "Y" 'dirvish-yank-menu
   "p" 'dirvish-paste
   "P" 'dirvish-paste-menu
   "z" 'dirvish-zip
   "Z" 'dirvish-unzip
   "v" 'dirvish-vc-menu
   "?" 'dirvish-dispatch
   "SPC" 'dirvish-show-history
   "RET" 'dirvish-find-file-true)
  
  ;; Dired-subtree keybindings
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "TAB" 'dired-subtree-toggle
   "S-TAB" 'dired-subtree-cycle
   "C-TAB" 'dired-subtree-remove)

  ;; Dired narrow
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "/" 'dired-narrow-fuzzy
   "n" 'dired-narrow
   "N" 'dired-narrow-regexp)

  ;; Dired ranger
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "Y" 'dired-ranger-copy
   "X" 'dired-ranger-move
   "P" 'dired-ranger-paste)


  ;; Evil keybindings
  (general-define-key
   :states 'normal
   ;; Use visual lines for j/k, logical lines for gj/gk
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line
   "gj" 'evil-next-line
   "gk" 'evil-previous-line
   ;; Centered scrolling
   "C-d" 'my/evil-scroll-down-center
   "C-u" 'my/evil-scroll-up-center)
  
  ;; Evil Numbers
  (general-define-key
   :states '(normal visual)
   "C-a" 'evil-numbers/inc-at-pt
   "C-x" 'evil-numbers/dec-at-pt)

  ;; Evil Args
  (general-define-key
    :keymaps 'evil-inner-text-objects-map
    "a" 'evil-inner-arg
    :keymaps 'evil-outer-text-objects-map
    "a" 'evil-outer-arg
    :states 'normal
    "]a" 'evil-forward-arg
    "[a" 'evil-backward-arg)

  (general-define-key
   :states '(normal visual)
   "gc" 'evilnc-comment-or-uncomment-lines)

  ;; Corfu
  (general-define-key
   :keymaps 'corfu-map
   "TAB" 'corfu-next
   "S-TAB" 'corfu-previous)

  ;; Treemacs
  (general-define-key
   :keymaps 'treemacs-mode-map
   :states '(normal motion)
   "h"     'treemacs-goto-parent-node
   "j"     'treemacs-next-line
   "k"     'treemacs-previous-line
   "l"     'treemacs-TAB-action
   "gg"    'treemacs-goto-first-child
   "G"     'treemacs-goto-last-child
   "H"     'treemacs-collapse-all-projects
   "L"     'treemacs-expand-all-projects
   "M-j"   'treemacs-next-project
   "M-k"   'treemacs-previous-project
   "o"     'treemacs-visit-node-default
   "RET"   'treemacs-visit-node-default
   "s"     'treemacs-visit-node-horizontal-split
   "v"     'treemacs-visit-node-vertical-split
   "q"     'treemacs-quit
   "r"     'treemacs-refresh
   "R"     'treemacs-rename-file
   "d"     'treemacs-delete-file
   "y"     'treemacs-copy-path-at-point)

  )
#+end_src

* Miscellaneous
