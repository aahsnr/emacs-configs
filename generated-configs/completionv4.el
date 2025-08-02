;; Comprehensive Emacs 30 Completion Setup
;; Gruvbox Theme Integration with Completion System

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core Completion Framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Vertico - Vertical completion interface
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 15)
  :config
  ;; Gruvbox theme integration for vertico
  (custom-set-faces
   '(vertico-current ((t (:background "#504945" :foreground "#ebdbb2" :weight bold))))
   '(vertico-group-title ((t (:foreground "#d3869b" :weight bold))))
   '(vertico-group-separator ((t (:foreground "#7c6f64"))))
   '(vertico-multiline ((t (:foreground "#83a598"))))))

;;; Corfu - In-buffer completion
(use-package corfu
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
  ;; Gruvbox theme integration for corfu
  (custom-set-faces
   '(corfu-default ((t (:background "#282828" :foreground "#ebdbb2"))))
   '(corfu-current ((t (:background "#504945" :foreground "#ebdbb2" :weight bold))))
   '(corfu-bar ((t (:background "#b16286"))))
   '(corfu-border ((t (:background "#7c6f64"))))
   '(corfu-annotations ((t (:foreground "#a89984" :italic t))))
   '(corfu-deprecated ((t (:foreground "#7c6f64" :strike-through t)))))
  ;; Enable Corfu more aggressively in the minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

;;; Corfu extensions
(use-package corfu-history
  :ensure t
  :after corfu
  :init (corfu-history-mode 1)
  :config (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-popupinfo
  :ensure t
  :after corfu
  :init (corfu-popupinfo-mode 1)
  :custom (corfu-popupinfo-delay '(0.5 . 0.2))
  :config
  ;; Gruvbox theme for corfu-popupinfo
  (custom-set-faces
   '(corfu-popupinfo ((t (:background "#282828" :foreground "#ebdbb2"))))
   '(corfu-popupinfo-documentation ((t (:foreground "#a89984" :italic t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Annotations, Filtering, and Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Marginalia - Rich annotations in minibuffer
(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :config
  ;; Gruvbox theme integration for marginalia
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

;;; Orderless - Fuzzy matching completion style
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  ;; Gruvbox theme for orderless
  (custom-set-faces
   '(orderless-match-face-0 ((t (:foreground "#d3869b" :weight bold))))
   '(orderless-match-face-1 ((t (:foreground "#83a598" :weight bold))))
   '(orderless-match-face-2 ((t (:foreground "#b8bb26" :weight bold))))
   '(orderless-match-face-3 ((t (:foreground "#fabd2f" :weight bold))))))

;;; Consult - Enhanced search commands
(use-package consult
  :ensure t
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
  
  ;; Gruvbox theme integration for consult
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

;; General.el configuration for Spacemacs-like keybindings
(use-package general
  :ensure t
  :config
  ;; Set up leader key (SPC in normal state, M-SPC in insert/emacs state)
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "M-SPC")
  
  ;; Buffer operations (SPC b)
  (my/leader-keys
    "b" '(:ignore t :which-key "buffer")
    "bb" '(consult-buffer :which-key "switch buffer")
    "br" '(consult-recent-file :which-key "recent files")
    "bB" '(consult-buffer-other-window :which-key "switch buffer other window")
    "bi" '(consult-imenu :which-key "imenu")
    "bI" '(consult-imenu-multi :which-key "imenu multi")
    "bo" '(consult-outline :which-key "outline")
    "bm" '(consult-bookmark :which-key "bookmarks")
    "by" '(consult-yank-pop :which-key "yank ring"))
  
  ;; File operations (SPC f)
  (my/leader-keys
    "f" '(:ignore t :which-key "file")
    "ff" '(consult-find :which-key "find file")
    "fr" '(consult-recent-file :which-key "recent files")
    "fl" '(consult-locate :which-key "locate"))
  
  ;; Search operations (SPC s)
  (my/leader-keys
    "s" '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "search line")
    "sS" '(consult-line-multi :which-key "search line multi")
    "sp" '(consult-ripgrep :which-key "ripgrep project")
    "sP" '(consult-git-grep :which-key "git grep")
    "sd" '(consult-find :which-key "find file")
    "sk" '(consult-keep-lines :which-key "keep lines")
    "sK" '(consult-flush-lines :which-key "flush lines")
    "sf" '(consult-focus-lines :which-key "focus lines"))
  
  ;; Jump/Go operations (SPC j)
  (my/leader-keys
    "j" '(:ignore t :which-key "jump")
    "jj" '(consult-line :which-key "jump to line")
    "jm" '(consult-mark :which-key "jump to mark")
    "jM" '(consult-global-mark :which-key "jump to global mark")
    "jo" '(consult-outline :which-key "jump to outline")
    "ji" '(consult-imenu :which-key "jump to imenu")
    "jI" '(consult-imenu-multi :which-key "jump to imenu multi"))
  
  ;; Help operations (SPC h)
  (my/leader-keys
    "h" '(:ignore t :which-key "help")
    "ha" '(consult-apropos :which-key "apropos")
    "hm" '(consult-man :which-key "man pages")
    "hi" '(consult-info :which-key "info"))
  
  ;; Project operations (SPC p)
  (my/leader-keys
    "p" '(:ignore t :which-key "project")
    "pf" '(consult-find :which-key "find file in project")
    "pp" '(consult-project-buffer :which-key "project buffers")
    "ps" '(consult-ripgrep :which-key "search in project"))
  
  ;; Register operations (SPC r)
  (my/leader-keys
    "r" '(:ignore t :which-key "register")
    "rr" '(consult-register :which-key "registers")
    "rs" '(consult-register-store :which-key "store register")
    "rl" '(consult-register-load :which-key "load register"))
  
  ;; Error/Compilation operations (SPC e)
  (my/leader-keys
    "e" '(:ignore t :which-key "error")
    "ee" '(consult-flymake :which-key "flymake errors")
    "ec" '(consult-compile-error :which-key "compilation errors"))
  
  ;; Version control operations (SPC g)
  (my/leader-keys
    "g" '(:ignore t :which-key "git")
    "gs" '(consult-git-grep :which-key "git grep"))
  
  ;; Alternative single-key bindings for frequently used commands
  (general-define-key
   :keymaps 'override
   "C-s" 'consult-line
   "C-x b" 'consult-buffer
   "C-x C-r" 'consult-recent-file
   "M-y" 'consult-yank-pop
   "M-g g" 'consult-goto-line
   "M-g m" 'consult-mark
   "M-g M" 'consult-global-mark
   "M-g o" 'consult-outline
   "M-g i" 'consult-imenu
   "C-c h" 'consult-history
   "C-c k" 'consult-kmacro
   "C-c m" 'consult-mode-command
   "C-c c" 'consult-complex-command))

;; Optional: Configure consult integration with other packages
(use-package consult-flymake
  :ensure t
  :after (consult flymake)
  :config
  (my/leader-keys
    "ef" '(consult-flymake :which-key "flymake errors")))

;; Optional: Configure consult-dir for enhanced directory navigation
(use-package consult-dir
  :ensure t
  :after consult
  :config
  (my/leader-keys
    "fd" '(consult-dir :which-key "change directory")))

;;; wgrep - Editable grep buffers
(use-package wgrep
  :ensure t
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Actions and Completion-at-Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Embark - Context-aware actions
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

  ;; Gruvbox theme integration for embark
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

;;; Embark-Consult integration
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI Enhancements (Icons & Key Hints)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Nerd Icons - For beautiful icons
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  (nerd-icons-color-icons t)
  (nerd-icons-scale-factor 1.1)
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

;;; Nerd Icons Completion Integration
(use-package nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;; Nerd Icons Corfu Integration
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

;;; Which-key for keybinding hints
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-prefix-prefix "+")
  :config
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings (General.el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package general
  :ensure t
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my/leader-keys
    ;; File operations
    "f"   '(:ignore t :which-key "files")
    "ff"  '(find-file :which-key "find file")
    "fr"  '(consult-recent-file :which-key "recent files")
    "fs"  '(save-buffer :which-key "save file")
    "fS"  '(save-some-buffers :which-key "save all")
    "fd"  '((lambda () (interactive) (find-file user-init-file)) :which-key "open config")

    ;; Buffer operations
    "b"   '(:ignore t :which-key "buffers")
    "bb"  '(consult-buffer :which-key "switch buffer")
    "bk"  '(kill-this-buffer :which-key "kill buffer")
    "bK"  '(kill-buffer :which-key "kill buffer (choose)")
    "br"  '(revert-buffer :which-key "revert buffer")
    "bs"  '(consult-buffer-other-window :which-key "switch buffer other window")

    ;; Search operations
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
    "sM"  '(consult-global-mark :which-key "global marks")
    "s*"  '(my/consult-line-symbol-at-point :which-key "search symbol at point")
    "s8"  '(my/consult-ripgrep-symbol-at-point :which-key "ripgrep symbol at point")

    ;; Navigation
    "j"   '(:ignore t :which-key "jump")
    "jj"  '(consult-goto-line :which-key "goto line")
    "jm"  '(consult-mark :which-key "jump to mark")
    "jM"  '(consult-global-mark :which-key "jump to global mark")
    "ji"  '(consult-imenu :which-key "jump to imenu")
    "jo"  '(consult-outline :which-key "jump to outline")

    ;; Help operations
    "h"   '(:ignore t :which-key "help")
    "hf"  '(describe-function :which-key "describe function")
    "hv"  '(describe-variable :which-key "describe variable")
    "hk"  '(describe-key :which-key "describe key")
    "hm"  '(describe-mode :which-key "describe mode")
    "hp"  '(describe-package :which-key "describe package")
    "hi"  '(info :which-key "info")
    "ha"  '(consult-apropos :which-key "apropos")
    "hC"  '(my/completion-tips :which-key "completion tips")

    ;; Embark actions
    "a"   '(:ignore t :which-key "actions")
    "aa"  '(embark-act :which-key "embark act")
    "ad"  '(embark-dwim :which-key "embark dwim")
    "ab"  '(embark-bindings :which-key "embark bindings")

    ;; Completion operations
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
    "ct"  '(cape-tex :which-key "tex")

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
    "qf" '(delete-frame :which-key "delete frame")

    ;; Quick Access
    "SPC" '(execute-extended-command :which-key "M-x")
    ":"   '(eval-expression :which-key "eval expression"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode-Specific Configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Org-mode
(use-package org
  :ensure nil ; built-in
  :after (cape corfu)
  :hook (org-mode . (lambda ()
                      (setq-local completion-at-point-functions
                                  (list #'cape-elisp-symbol
                                        #'org-pcomplete-initial
                                        #'cape-dabbrev
                                        #'cape-file
                                        #'cape-keyword))
                      (add-hook 'completion-at-point-functions #'my/org-babel-complete-at-point nil t)))
  :config
  (defun my/org-babel-complete-at-point ()
    "Complete at point in org-babel src blocks."
    (when (org-in-src-block-p)
      (let ((lang (car (org-babel-get-src-block-info t))))
        (cond
         ((string-equal lang "emacs-lisp") (elisp-completion-at-point))
         ((string-equal lang "python") (python-completion-at-point))
         (t (cape-dabbrev))))))
  ;; Gruvbox theme for org-mode elements
  (custom-set-faces
   '(org-block-begin-line ((t (:foreground "#7c6f64" :background "#32302f"))))
   '(org-block-end-line ((t (:foreground "#7c6f64" :background "#32302f"))))
   '(org-code ((t (:foreground "#fe8019" :background "#3c3836"))))
   '(org-verbatim ((t (:foreground "#b8bb26" :background "#3c3836"))))))

;;; Programming modes completion setup
(defun my/prog-mode-completion-setup ()
  "Enhanced completion setup for programming modes."
  (setq-local completion-at-point-functions
              (list #'cape-elisp-symbol
                    #'cape-dabbrev
                    #'cape-file
                    #'cape-keyword
                    #'cape-line)))
(add-hook 'prog-mode-hook #'my/prog-mode-completion-setup)

;;; Text mode completion setup
(defun my/text-mode-completion-setup ()
  "Enhanced completion setup for text modes."
  (setq-local completion-at-point-functions
              (list #'cape-dabbrev
                    #'cape-file
                    #'cape-dict
                    #'cape-line
                    #'cape-abbrev)))
(add-hook 'text-mode-hook #'my/text-mode-completion-setup)

;;; Shell modes completion setup
(defun my/shell-mode-completion-setup ()
  "Enhanced completion setup for shell modes."
  (setq-local completion-at-point-functions
              (list #'cape-history
                    #'cape-file
                    #'cape-dabbrev
                    #'cape-keyword)))
(add-hook 'shell-mode-hook #'my/shell-mode-completion-setup)
(add-hook 'eshell-mode-hook #'my/shell-mode-completion-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs Behavior & Enhancements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; General Emacs settings and performance
(use-package emacs
  :ensure nil ; built-in
  :custom
  ;; Completion
  (completion-cycle-threshold 3)
  (completions-detailed t)
  (tab-always-indent 'complete)
  ;; Performance
  (gc-cons-threshold (* 50 1000 1000))
  (read-process-output-max (* 1024 1024))
  (large-file-warning-threshold 100000000)
  (bidi-inhibit-bpa t)
  (bidi-paragraph-direction 'left-to-right)
  :config
  ;; Hide commands in M-x that don't apply to the current mode
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;; Add prompt indicator to `completing-read-multiple'
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;;; Minibuffer enhancements
(use-package minibuffer
  :ensure nil ; built-in
  :custom
  (enable-recursive-minibuffers t)
  :config
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (add-hook 'minibuffer-setup-hook #'(lambda () (setq-local completion-at-point-functions
                                                          (list #'cape-history #'cape-file #'cape-dabbrev))))
  ;; Gruvbox theme for minibuffer
  (custom-set-faces
   '(minibuffer-prompt ((t (:foreground "#83a598" :weight bold))))
   '(completions-annotations ((t (:foreground "#a89984" :italic t))))
   '(completions-first-difference ((t (:foreground "#fabd2f" :weight bold))))
   '(completions-first-difference ((t (:foreground "#fabd2f" :weight bold))))))

;;; Savehist - persist history over Emacs restarts
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

;;; Recentf - track recently opened files
(use-package recentf
  :ensure nil ; built-in
  :init (recentf-mode 1)
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  (recentf-exclude '("\\elpa" "\\straight" "/tmp/" "/ssh:" "/sudo:" "COMMIT_EDITMSG")))

;;; Abbrev mode enhancements
(use-package abbrev
  :ensure nil ; built-in
  :diminish abbrev-mode
  :custom
  (save-abbrevs 'silently)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
  (add-hook 'text-mode-hook #'abbrev-mode)
  (add-hook 'prog-mode-hook #'abbrev-mode))

;;; Dabbrev (Dynamic Abbreviation)
(use-package dabbrev
  :ensure nil ; built-in
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil)
  (dabbrev-upcase-means-case-search t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom Functions & Finalization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; Function to show helpful startup tips
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
    (insert "- M-/: Dabbrev completion\n")
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
    (insert "- Use `<` to narrow completion results in Consult\n")
    (insert "- Use ` ` (space) to separate search terms in orderless\n")
    (insert "- Use `!` to negate terms in orderless\n")
    (insert "- Use `TAB` for completion in programming modes\n")
    (insert "- Use `embark-act` (C-.) for context actions\n")
    (insert "- Icons are provided by nerd-icons with Gruvbox colors\n")
    (org-mode)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;; Final message
(message "Comprehensive completion setup with Gruvbox theme loaded successfully!")
