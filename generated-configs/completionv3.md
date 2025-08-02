``````el
;; Comprehensive Emacs 30 Completion Setup
;; Tokyo Night Theme Integration with Completion System

;;; Core Completion Framework - Vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 15)
  :config
  ;; Tokyo Night theme integration for vertico
  (custom-set-faces
   '(vertico-current ((t (:background "#414868" :foreground "#c0caf5" :weight bold))))
   '(vertico-group-title ((t (:foreground "#bb9af7" :weight bold))))
   '(vertico-group-separator ((t (:foreground "#565f89"))))
   '(vertico-multiline ((t (:foreground "#7aa2f7"))))))

;;; Marginalia - Rich annotations in minibuffer
(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :config
  ;; Tokyo Night theme integration for marginalia
  (custom-set-faces
   '(marginalia-archive ((t (:foreground "#9ece6a"))))
   '(marginalia-char ((t (:foreground "#ff9e64"))))
   '(marginalia-date ((t (:foreground "#7dcfff"))))
   '(marginalia-documentation ((t (:foreground "#9aa5ce" :italic t))))
   '(marginalia-file-name ((t (:foreground "#c0caf5"))))
   '(marginalia-file-owner ((t (:foreground "#bb9af7"))))
   '(marginalia-file-priv-dir ((t (:foreground "#7aa2f7"))))
   '(marginalia-file-priv-exec ((t (:foreground "#9ece6a"))))
   '(marginalia-file-priv-link ((t (:foreground "#7dcfff"))))
   '(marginalia-file-priv-read ((t (:foreground "#ff9e64"))))
   '(marginalia-file-priv-write ((t (:foreground "#f7768e"))))
   '(marginalia-function ((t (:foreground "#7aa2f7"))))
   '(marginalia-key ((t (:foreground "#ff9e64"))))
   '(marginalia-lighter ((t (:foreground "#565f89"))))
   '(marginalia-list ((t (:foreground "#7dcfff"))))
   '(marginalia-mode ((t (:foreground "#bb9af7"))))
   '(marginalia-modified ((t (:foreground "#e0af68"))))
   '(marginalia-null ((t (:foreground "#565f89"))))
   '(marginalia-number ((t (:foreground "#ff9e64"))))
   '(marginalia-size ((t (:foreground "#9ece6a"))))
   '(marginalia-string ((t (:foreground "#9ece6a"))))
   '(marginalia-symbol ((t (:foreground "#bb9af7"))))
   '(marginalia-true ((t (:foreground "#9ece6a"))))
   '(marginalia-type ((t (:foreground "#7aa2f7"))))
   '(marginalia-value ((t (:foreground "#c0caf5"))))
   '(marginalia-variable ((t (:foreground "#7dcfff"))))
   '(marginalia-version ((t (:foreground "#9ece6a"))))))

;;; Consult - Consulting completing-read
(use-package consult
  :ensure t
  :after vertico
  :custom
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  :config
  ;; Configure consult-xref
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  
  ;; Configure register formatting
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  
  ;; Use Consult to select xref locations with preview
  (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  
  ;; Tokyo Night theme integration for consult
  (custom-set-faces
   '(consult-bookmark ((t (:foreground "#bb9af7"))))
   '(consult-buffer ((t (:foreground "#c0caf5"))))
   '(consult-file ((t (:foreground "#7dcfff"))))
   '(consult-imenu-prefix ((t (:foreground "#565f89"))))
   '(consult-key ((t (:foreground "#ff9e64"))))
   '(consult-line-number ((t (:foreground "#565f89"))))
   '(consult-line-number-prefix ((t (:foreground "#414868"))))
   '(consult-line-number-wrapped ((t (:foreground "#f7768e"))))
   '(consult-narrow-indicator ((t (:foreground "#e0af68"))))
   '(consult-preview-cursor ((t (:background "#ff9e64"))))
   '(consult-preview-error ((t (:foreground "#f7768e"))))
   '(consult-preview-insertion ((t (:background "#9ece6a" :foreground "#1a1b26"))))
   '(consult-preview-line ((t (:background "#414868"))))
   '(consult-preview-match ((t (:background "#bb9af7" :foreground "#1a1b26"))))
   '(consult-separator ((t (:foreground "#565f89"))))))

;;; Embark - Minibuffer actions and context menu
(use-package embark
  :ensure t
  :after (vertico consult)
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  
  ;; Tokyo Night theme integration for embark
  (custom-set-faces
   '(embark-keybinding ((t (:foreground "#ff9e64" :weight bold))))
   '(embark-collect-marked ((t (:background "#414868" :foreground "#c0caf5"))))
   '(embark-collect-group-title ((t (:foreground "#bb9af7" :weight bold))))
   '(embark-collect-group-separator ((t (:foreground "#565f89"))))))

;;; Embark-Consult integration
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Corfu - In-buffer completion
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
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
  ;; Tokyo Night theme integration for corfu
  (custom-set-faces
   '(corfu-default ((t (:background "#24283b" :foreground "#c0caf5"))))
   '(corfu-current ((t (:background "#414868" :foreground "#c0caf5" :weight bold))))
   '(corfu-bar ((t (:background "#bb9af7"))))
   '(corfu-border ((t (:background "#565f89"))))
   '(corfu-annotations ((t (:foreground "#9aa5ce" :italic t))))
   '(corfu-deprecated ((t (:foreground "#565f89" :strike-through t))))))

;;; Cape - Completion at point extensions
(use-package cape
  :ensure t
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
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  
  ;; Cape for org-mode
  (defun my/cape-org-setup ()
    "Setup Cape for org-mode."
    (setq-local completion-at-point-functions
                (list #'cape-elisp-symbol
                      #'cape-dabbrev
                      #'cape-file
                      #'cape-keyword)))
  
  (add-hook 'org-mode-hook #'my/cape-org-setup))

;;; Enhanced Dabbrev configuration
(use-package dabbrev
  :ensure nil
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;;; Nerd Icons Integration
(use-package nerd-icons
  :ensure t
  :custom
  ;; Tokyo Night color scheme for nerd-icons
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  :config
  ;; Tokyo Night theme integration for nerd-icons
  (setq nerd-icons-color-icons t)
  (setq nerd-icons-scale-factor 1.1)
  
  ;; Custom Tokyo Night colors for different icon types
  (setq nerd-icons-default-file-color "#c0caf5")
  (setq nerd-icons-default-dir-color "#7aa2f7")
  
  ;; Color mappings for different file types
  (add-to-list 'nerd-icons-extension-icon-alist
               '("org" nerd-icons-sucicon "nf-custom-orgmode" :face (:foreground "#9ece6a")))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("el" nerd-icons-sucicon "nf-custom-emacs" :face (:foreground "#bb9af7")))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("py" nerd-icons-devicon "nf-dev-python" :face (:foreground "#7dcfff")))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("js" nerd-icons-devicon "nf-dev-javascript" :face (:foreground "#e0af68")))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("json" nerd-icons-devicon "nf-dev-javascript" :face (:foreground "#ff9e64")))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("md" nerd-icons-octicon "nf-oct-markdown" :face (:foreground "#7dcfff")))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("yaml" nerd-icons-octicon "nf-oct-gear" :face (:foreground "#f7768e")))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("yml" nerd-icons-octicon "nf-oct-gear" :face (:foreground "#f7768e"))))

;;; Nerd Icons Completion
(use-package nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;; Nerd Icons Corfu
(use-package nerd-icons-corfu
  :ensure t
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  ;; Tokyo Night colors for corfu icons
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face (:foreground "#7dcfff"))
          (boolean :style "cod" :icon "symbol_boolean" :face (:foreground "#9ece6a"))
          (class :style "cod" :icon "symbol_class" :face (:foreground "#bb9af7"))
          (color :style "cod" :icon "symbol_color" :face (:foreground "#ff9e64"))
          (command :style "cod" :icon "terminal" :face (:foreground "#7aa2f7"))
          (constant :style "cod" :icon "symbol_constant" :face (:foreground "#e0af68"))
          (constructor :style "cod" :icon "triangle_right" :face (:foreground "#7dcfff"))
          (enummember :style "cod" :icon "symbol_enum_member" :face (:foreground "#9ece6a"))
          (enum-member :style "cod" :icon "symbol_enum_member" :face (:foreground "#9ece6a"))
          (enum :style "cod" :icon "symbol_enum" :face (:foreground "#bb9af7"))
          (event :style "cod" :icon "symbol_event" :face (:foreground "#f7768e"))
          (field :style "cod" :icon "symbol_field" :face (:foreground "#7dcfff"))
          (file :style "cod" :icon "symbol_file" :face (:foreground "#c0caf5"))
          (folder :style "cod" :icon "folder" :face (:foreground "#7aa2f7"))
          (interface :style "cod" :icon "symbol_interface" :face (:foreground "#bb9af7"))
          (keyword :style "cod" :icon "symbol_keyword" :face (:foreground "#ff9e64"))
          (macro :style "cod" :icon "symbol_misc" :face (:foreground "#e0af68"))
          (magic :style "cod" :icon "wand" :face (:foreground "#bb9af7"))
          (method :style "cod" :icon "symbol_method" :face (:foreground "#7aa2f7"))
          (function :style "cod" :icon "symbol_method" :face (:foreground "#7aa2f7"))
          (module :style "cod" :icon "file_submodule" :face (:foreground "#9ece6a"))
          (numeric :style "cod" :icon "symbol_numeric" :face (:foreground "#ff9e64"))
          (operator :style "cod" :icon "symbol_operator" :face (:foreground "#f7768e"))
          (param :style "cod" :icon "symbol_parameter" :face (:foreground "#7dcfff"))
          (property :style "cod" :icon "symbol_property" :face (:foreground "#9ece6a"))
          (reference :style "cod" :icon "references" :face (:foreground "#7dcfff"))
          (snippet :style "cod" :icon "symbol_snippet" :face (:foreground "#e0af68"))
          (string :style "cod" :icon "symbol_string" :face (:foreground "#9ece6a"))
          (struct :style "cod" :icon "symbol_structure" :face (:foreground "#bb9af7"))
          (text :style "cod" :icon "symbol_key" :face (:foreground "#c0caf5"))
          (typeparameter :style "cod" :icon "list_unordered" :face (:foreground "#7dcfff"))
          (type-parameter :style "cod" :icon "list_unordered" :face (:foreground "#7dcfff"))
          (unit :style "cod" :icon "symbol_ruler" :face (:foreground "#9ece6a"))
          (value :style "cod" :icon "symbol_field" :face (:foreground "#c0caf5"))
          (variable :style "cod" :icon "symbol_variable" :face (:foreground "#7dcfff"))
          (t :style "cod" :icon "code" :face (:foreground "#c0caf5")))))

;;; General.el for Spacemacs-style keybindings
(use-package general
  :ensure t
  :demand t
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  
  ;; File operations
  (my/leader-keys
    "f"   '(:ignore t :which-key "files")
    "ff"  '(find-file :which-key "find file")
    "fr"  '(consult-recent-file :which-key "recent files")
    "fs"  '(save-buffer :which-key "save file")
    "fS"  '(save-some-buffers :which-key "save all")
    "fd"  '((lambda () (interactive) (find-file user-init-file)) :which-key "open config"))
  
  ;; Buffer operations
  (my/leader-keys
    "b"   '(:ignore t :which-key "buffers")
    "bb"  '(consult-buffer :which-key "switch buffer")
    "bk"  '(kill-this-buffer :which-key "kill buffer")
    "bK"  '(kill-buffer :which-key "kill buffer (choose)")
    "br"  '(revert-buffer :which-key "revert buffer")
    "bs"  '(consult-buffer-other-window :which-key "switch buffer other window"))
  
  ;; Search operations
  (my/leader-keys
    "s"   '(:ignore t :which-key "search")
    "ss"  '(consult-line :which-key "search in buffer")
    "sS"  '(consult-line-multi :which-key "search in all buffers")
    "sr"  '(consult-ripgrep :which-key "ripgrep")
    "sg"  '(consult-grep :which-key "grep")
    "sf"  '(consult-find :which-key "find files")
    "sF"  '(consult-locate :which-key "locate files")
    "si"  '(consult-imenu :which-key "imenu")
    "sI"  '(consult-imenu-multi :which-key "imenu multi")
    "so"  '(consult-outline :which-key "outline")
    "sm"  '(consult-mark :which-key "marks")
    "sM"  '(consult-global-mark :which-key "global marks"))
  
  ;; Navigation
  (my/leader-keys
    "j"   '(:ignore t :which-key "jump")
    "jj"  '(consult-goto-line :which-key "goto line")
    "jm"  '(consult-mark :which-key "jump to mark")
    "jM"  '(consult-global-mark :which-key "jump to global mark")
    "ji"  '(consult-imenu :which-key "jump to imenu")
    "jo"  '(consult-outline :which-key "jump to outline"))
  
  ;; Help operations
  (my/leader-keys
    "h"   '(:ignore t :which-key "help")
    "hf"  '(describe-function :which-key "describe function")
    "hv"  '(describe-variable :which-key "describe variable")
    "hk"  '(describe-key :which-key "describe key")
    "hm"  '(describe-mode :which-key "describe mode")
    "hp"  '(describe-package :which-key "describe package")
    "hi"  '(info :which-key "info")
    "ha"  '(consult-apropos :which-key "apropos"))
  
  ;; Embark actions
  (my/leader-keys
    "a"   '(:ignore t :which-key "actions")
    "aa"  '(embark-act :which-key "embark act")
    "ad"  '(embark-dwim :which-key "embark dwim")
    "ab"  '(embark-bindings :which-key "embark bindings"))
  
  ;; Completion operations
  (my/leader-keys
    "c"   '(:ignore t :which-key "completion")
    "cp"  '(completion-at-point :which-key "complete at point")
    "cd"  '(cape-dabbrev :which-key "dabbrev")
    "cf"  '(cape-file :which-key "file")
    "cs"  '(cape-elisp-symbol :which-key "elisp symbol")
    "ck"  '(cape-keyword :which-key "keyword")
    "cl"  '(cape-line :which-key "line")
    "ch"  '(cape-history :which-key "history")
    "ca"  '(cape-abbrev :which-key "abbreviation")
    "ce"  '(cape-emoji :which-key "emoji")
    "ct"  '(cape-tex :which-key "tex"))
  
  ;; Org-mode specific (when in org buffers)
  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal visual)
   :prefix "SPC"
   "m"   '(:ignore t :which-key "org")
   "ml"  '(org-insert-link :which-key "insert link")
   "mt"  '(org-todo :which-key "todo")
   "mT"  '(org-show-todo-tree :which-key "todo tree")
   "ms"  '(org-schedule :which-key "schedule")
   "md"  '(org-deadline :which-key "deadline")
   "me"  '(org-export-dispatch :which-key "export")
   "ma"  '(org-agenda :which-key "agenda")
   "mc"  '(org-capture :which-key "capture")
   "mr"  '(org-refile :which-key "refile")
   "mA"  '(org-archive-subtree :which-key "archive subtree")))

;;; Org-mode integration for completion in src blocks
(use-package org
  :ensure nil
  :after (cape corfu)
  :config
  ;; Enhanced completion in org-src blocks
  (defun my/org-babel-complete-at-point ()
    "Complete at point in org-babel src blocks."
    (when (org-in-src-block-p)
      (let ((lang (car (org-babel-get-src-block-info t))))
        (cond
         ((string-equal lang "emacs-lisp")
          (elisp-completion-at-point))
         ((string-equal lang "python")
          (python-completion-at-point))
         (t
          (cape-dabbrev))))))
  
  (defun my/org-src-setup ()
    "Setup completion for org-src blocks."
    (add-hook 'completion-at-point-functions #'my/org-babel-complete-at-point nil t))
  
  (add-hook 'org-mode-hook #'my/org-src-setup)
  
  ;; Better completion in org-mode
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list #'cape-elisp-symbol
                                #'org-pcomplete-initial
                                #'cape-dabbrev
                                #'cape-file
                                #'cape-keyword))))
  
  ;; Tokyo Night theme for org-mode completion elements
  (custom-set-faces
   '(org-block-begin-line ((t (:foreground "#565f89" :background "#1f2335"))))
   '(org-block-end-line ((t (:foreground "#565f89" :background "#1f2335"))))
   '(org-code ((t (:foreground "#ff9e64" :background "#24283b"))))
   '(org-verbatim ((t (:foreground "#9ece6a" :background "#24283b"))))))

;;; Enhanced minibuffer and completion behavior
(use-package minibuffer
  :ensure nil
  :custom
  (completion-cycle-threshold 3)
  (completions-detailed t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :config
  ;; Support opening new minibuffers from inside existing minibuffers
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  
  ;; Tokyo Night theme for minibuffer
  (custom-set-faces
   '(minibuffer-prompt ((t (:foreground "#7aa2f7" :weight bold))))
   '(completions-annotations ((t (:foreground "#9aa5ce" :italic t))))
   '(completions-common-part ((t (:foreground "#bb9af7" :weight bold))))
   '(completions-first-difference ((t (:foreground "#e0af68" :weight bold)))))))

;; Performance optimizations and enhancements
(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)
  
  ;; Enable indentation+completion using the TAB key
  (tab-always-indent 'complete)
  
  ;; Hide commands in M-x which do not apply to the current mode
  (read-extended-command-predicate #'command-completion-default-include-p)
  
  :config
  ;; Add prompt indicator to `completing-read-multiple'
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  
  ;; Support opening new minibuffers from inside existing minibuffers
  (setq enable-recursive-minibuffers t)
  
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current mode
  (when (>= emacs-major-version 28)
    (setq read-extended-command-predicate #'command-completion-default-include-p)))

;;; Additional enhancements

;; Orderless completion style (recommended for better fuzzy matching)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  ;; Tokyo Night theme for orderless
  (custom-set-faces
   '(orderless-match-face-0 ((t (:foreground "#bb9af7" :weight bold))))
   '(orderless-match-face-1 ((t (:foreground "#7aa2f7" :weight bold))))
   '(orderless-match-face-2 ((t (:foreground "#9ece6a" :weight bold))))
   '(orderless-match-face-3 ((t (:foreground "#e0af68" :weight bold)))))))

;; Which-key for keybinding help
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-prefix-prefix "+")
  :config
  ;; Tokyo Night theme for which-key
  (custom-set-faces
   '(which-key-key-face ((t (:foreground "#7aa2f7" :weight bold))))
   '(which-key-description-face ((t (:foreground "#c0caf5"))))
   '(which-key-group-description-face ((t (:foreground "#bb9af7"))))
   '(which-key-command-description-face ((t (:foreground "#9ece6a"))))
   '(which-key-local-map-description-face ((t (:foreground "#7dcfff"))))
   '(which-key-separator-face ((t (:foreground "#565f89"))))
   '(which-key-prefix-face ((t (:foreground "#ff9e64" :weight bold))))
   '(which-key-note-face ((t (:foreground "#565f89")))))))

;; Savehist - persist history over Emacs restarts
(use-package savehist
  :ensure nil
  :init
  (savehist-mode)
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

;; Recentf - track recently opened files
(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  (recentf-exclude '("\\elpa" "\\straight" "/tmp/" "/ssh:" "/sudo:" "COMMIT_EDITMSG")))

;; Abbrev mode enhancements
(use-package abbrev
  :ensure nil
  :diminish abbrev-mode
  :config
  (setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
  (setq save-abbrevs 'silently)
  (add-hook 'text-mode-hook #'abbrev-mode)
  (add-hook 'prog-mode-hook #'abbrev-mode))

;; Hippie expand configuration
(use-package hippie-exp
  :ensure nil
  :bind ("M-/" . hippie-expand)
  :custom
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol)))

;; Enhanced dabbrev with better defaults
(use-package dabbrev
  :ensure nil
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil)
  (dabbrev-upcase-means-case-search t))

;; Completion enhancements for programming modes
(defun my/prog-mode-completion-setup ()
  "Enhanced completion setup for programming modes."
  (setq-local completion-at-point-functions
              (list #'cape-elisp-symbol
                    #'cape-dabbrev
                    #'cape-file
                    #'cape-keyword
                    #'cape-line)))

(add-hook 'prog-mode-hook #'my/prog-mode-completion-setup)

;; Text mode completion setup
(defun my/text-mode-completion-setup ()
  "Enhanced completion setup for text modes."
  (setq-local completion-at-point-functions
              (list #'cape-dabbrev
                    #'cape-file
                    #'cape-dict
                    #'cape-line
                    #'cape-abbrev)))

(add-hook 'text-mode-hook #'my/text-mode-completion-setup)

;; Enhanced completion for shell modes
(defun my/shell-mode-completion-setup ()
  "Enhanced completion setup for shell modes."
  (setq-local completion-at-point-functions
              (list #'cape-history
                    #'cape-file
                    #'cape-dabbrev
                    #'cape-keyword)))

(add-hook 'shell-mode-hook #'my/shell-mode-completion-setup)
(add-hook 'eshell-mode-hook #'my/shell-mode-completion-setup)

;; Completion in minibuffer
(defun my/minibuffer-completion-setup ()
  "Enhanced completion setup for minibuffer."
  (setq-local completion-at-point-functions
              (list #'cape-history
                    #'cape-file
                    #'cape-dabbrev)))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-completion-setup)

;; Advanced Corfu configuration
(with-eval-after-load 'corfu
  ;; Enable Corfu more aggressively
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  
  ;; Corfu history (check if available)
  (when (require 'corfu-history nil t)
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  
  ;; Corfu popupinfo (check if available)
  (when (require 'corfu-popupinfo nil t)
    (corfu-popupinfo-mode 1)
    (setq corfu-popupinfo-delay '(0.5 . 0.2))
    
    ;; Tokyo Night theme for corfu-popupinfo
    (custom-set-faces
     '(corfu-popupinfo ((t (:background "#24283b" :foreground "#c0caf5"))))
     '(corfu-popupinfo-documentation ((t (:foreground "#9aa5ce" :italic t)))))))

;; Advanced Consult configuration
(with-eval-after-load 'consult
  ;; Consult customizations
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  
  ;; Optionally configure the narrowing key
  (setq consult-narrow-key "<")
  
  ;; Configure a function which returns the project root directory
  (when (fboundp 'project-current)
    (setq consult-project-function
          (lambda (_)
            (when-let (project (project-current))
              (if (fboundp 'project-root)
                  (project-root project)
                (car (project-roots project)))))))
  
  ;; Advanced search configurations
  (setq consult-find-args "find . -not ( -wholename */.* -prune )")
  (setq consult-grep-args "grep --null --line-buffered --color=never --ignore-case --exclude-dir=.git --line-number -I -r")
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip"))

;; Advanced Embark configuration
(with-eval-after-load 'embark
  ;; Which-key integration
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
            embark-isearch-highlight-indicator))
    
    (defun embark-hide-which-key-indicator (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      (when (fboundp 'which-key--hide-popup-ignore-command)
        (which-key--hide-popup-ignore-command))
      (let ((embark-indicators
             (remq #'embark-which-key-indicator embark-indicators)))
        (apply fn args)))
    
    (advice-add #'embark-completing-read-prompter
                :around #'embark-hide-which-key-indicator)))

;; Performance optimizations
(use-package emacs
  :ensure nil
  :config
  ;; Increase the large file warning threshold
  (setq large-file-warning-threshold 10000000)
  
  ;; Increase garbage collection threshold for better performance
  (setq gc-cons-threshold (* 50 1000 1000))
  
  ;; Increase the amount of data which Emacs reads from the process
  (setq read-process-output-max (* 1024 1024))
  
  ;; Reduce the frequency of garbage collection by making it happen on
  ;; each 50MB of allocated data (the default is on every 0.76MB)
  (setq gc-cons-threshold 50000000)
  
  ;; Warn when opening files bigger than 100MB
  (setq large-file-warning-threshold 100000000)
  
  ;; Disable bidirectional text rendering for performance
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t))

;; Custom functions for enhanced completion workflow
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

(defun my/cape-company-to-capf (backend)
  "Convert company BACKEND to cape completion-at-point-function."
  (when (and (fboundp backend) (fboundp 'cape-company-to-capf))
    (cape-company-to-capf backend)))

;; Additional keybindings for enhanced workflow
(my/leader-keys
  ;; Enhanced search with symbol at point
  "s*" '(my/consult-line-symbol-at-point :which-key "search symbol at point")
  "s8" '(my/consult-ripgrep-symbol-at-point :which-key "ripgrep symbol at point")
  
  ;; Quick access to completion functions
  "SPC" '(execute-extended-command :which-key "M-x")
  ":" '(eval-expression :which-key "eval expression")
  
  ;; Window management
  "w" '(:ignore t :which-key "windows")
  "wh" '(windmove-left :which-key "move left")
  "wj" '(windmove-down :which-key "move down")
  "wk" '(windmove-up :which-key "move up")
  "wl" '(windmove-right :which-key "move right")
  "wd" '(delete-window :which-key "delete window")
  "ws" '(split-window-below :which-key "split below")
  "wv" '(split-window-right :which-key "split right")
  "wm" '(delete-other-windows :which-key "maximize")
  
  ;; Toggles
  "t" '(:ignore t :which-key "toggles")
  "tl" '(display-line-numbers-mode :which-key "line numbers")
  "tw" '(whitespace-mode :which-key "whitespace")
  "tr" '(read-only-mode :which-key "read only")
  "tt" '(consult-theme :which-key "theme")
  
  ;; Quit operations
  "q" '(:ignore t :which-key "quit")
  "qq" '(save-buffers-kill-terminal :which-key "quit emacs")
  "qf" '(delete-frame :which-key "delete frame"))

;; Add restart-emacs function if not available
(unless (fboundp 'restart-emacs)
  (defun restart-emacs ()
    "Restart Emacs."
    (interactive)
    (save-buffers-kill-emacs t)))

;; Final message
(message "Comprehensive completion setup with Tokyo Night theme loaded successfully!")

;; Add some helpful startup tips
(defun my/completion-tips ()
  "Display helpful tips for the completion system."
  (interactive)
  (with-current-buffer (get-buffer-create "*Completion Tips*")
    (erase-buffer)
    (insert "# Emacs Completion System Tips\n\n")
    (insert "## Key Bindings (Spacemacs style with SPC leader)\n")
    (insert "- SPC ff: Find file\n")
    (insert "- SPC bb: Switch buffer\n")
    (insert "- SPC ss: Search in buffer (consult-line)\n")
    (insert "- SPC sr: Ripgrep search\n")
    (insert "- SPC aa: Embark act\n")
    (insert "- SPC cp: Complete at point\n")
    (insert "- SPC cd: Dabbrev completion\n")
    (insert "- C-.: Embark act\n")
    (insert "- C-;: Embark DWIM\n")
    (insert "- M-/: Hippie expand\n")
    (insert "- TAB: Complete or indent\n\n")
    (insert "## Completion Features\n")
    (insert "- Corfu: In-buffer completion with icons\n")
    (insert "- Vertico: Vertical completion in minibuffer\n")
    (insert "- Consult: Enhanced search and navigation\n")
    (insert "- Embark: Context-aware actions\n")
    (insert "- Cape: Completion at point extensions\n")
    (insert "- Marginalia: Rich annotations\n")
    (insert "- Orderless: Fuzzy matching\n\n")
    (insert "## Tips\n")
    (insert "- Use < to narrow completion results\n")
    (insert "- Use space to separate search terms in orderless\n")
    (insert "- Use ! to negate terms in orderless\n")
    (insert "- Use TAB for completion in programming modes\n")
    (insert "- Use embark-act (C-.) for context actions\n")
    (insert "- Icons are provided by nerd-icons with Tokyo Night colors\n")
    (org-mode)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;; Bind the help function
(my/leader-keys
  "hC" '(my/completion-tips :which-key "completion tips"))

(use-package wgrep
  :ensure t
  :after consult
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-enable-key "r")
  (wgrep-change-readonly-file t)
  :config
  ;; Tokyo Night theme for wgrep
  (custom-set-faces
   '(wgrep-face ((t (:background "#414868" :foreground "#c0caf5"))))
   '(wgrep-file-face ((t (:foreground "#7aa2f7" :weight bold))))
   '(wgrep-reject-face ((t (:foreground "#f7768e" :weight bold))))
   '(wgrep-done-face ((t (:foreground "#9ece6a" :weight bold))))))

``````
