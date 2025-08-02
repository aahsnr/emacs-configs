``````el
;; ==================================================
;; Comprehensive Integration Configuration for:
;; imenu, project.el, ibuffer, treemacs, perspective.el
;; with nerd-icons and vertico/cape/consult integration
;; Emacs 30 Compatible
;; ==================================================

;; Ensure required packages are installed
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  :config
  ;; Auto-install fonts if needed
  (unless (member "Symbols Nerd Font Mono" (font-family-list))
    (nerd-icons-install-fonts t)))

;; ==================================================
;; IMENU CONFIGURATION
;; ==================================================

(use-package imenu
  :ensure nil ; built-in
  :custom
  (imenu-auto-rescan t)
  (imenu-use-popup-menu nil)
  (imenu-eager-completion-buffer t)
  (imenu-space-replacement " ")
  (imenu-level-separator "/")
  (imenu-max-item-length 100)
  :config
  ;; Better imenu expressions for various modes
  (defun my/imenu-setup-expressions ()
    "Set up better imenu expressions for different modes."
    (when (derived-mode-p 'emacs-lisp-mode)
      (setq imenu-generic-expression
            '(("Functions" "^\\s-*(defun\\s-+\\([a-zA-Z0-9-_/]+\\)" 1)
              ("Macros" "^\\s-*(defmacro\\s-+\\([a-zA-Z0-9-_/]+\\)" 1)
              ("Variables" "^\\s-*(defvar\\s-+\\([a-zA-Z0-9-_/]+\\)" 1)
              ("Constants" "^\\s-*(defconst\\s-+\\([a-zA-Z0-9-_/]+\\)" 1)
              ("Custom" "^\\s-*(defcustom\\s-+\\([a-zA-Z0-9-_/]+\\)" 1)
              ("Faces" "^\\s-*(defface\\s-+\\([a-zA-Z0-9-_/]+\\)" 1)
              ("Advice" "^\\s-*(defadvice\\s-+\\([a-zA-Z0-9-_/]+\\)" 1))))
    (when (derived-mode-p 'python-mode)
      (setq imenu-generic-expression
            '(("Classes" "^\\s-*class\\s-+\\([a-zA-Z0-9_]+\\)" 1)
              ("Functions" "^\\s-*def\\s-+\\([a-zA-Z0-9_]+\\)" 1)
              ("Methods" "^\\s-*def\\s-+\\(self\\.\\)?\\([a-zA-Z0-9_]+\\)" 2)))))
  
  (add-hook 'emacs-lisp-mode-hook #'my/imenu-setup-expressions)
  (add-hook 'python-mode-hook #'my/imenu-setup-expressions)
  
  ;; Integration with consult
  (with-eval-after-load 'consult
    (setq consult-imenu-config
          '((emacs-lisp-mode :toplevel "Functions"
                           :types ((?f "Functions" font-lock-function-name-face)
                                 (?m "Macros" font-lock-function-name-face)
                                 (?v "Variables" font-lock-variable-name-face)
                                 (?c "Constants" font-lock-constant-face)
                                 (?C "Custom" font-lock-variable-name-face)
                                 (?F "Faces" font-lock-type-face)
                                 (?a "Advice" font-lock-keyword-face)))
            (python-mode :toplevel "Classes"
                        :types ((?c "Classes" font-lock-type-face)
                               (?f "Functions" font-lock-function-name-face)
                               (?m "Methods" font-lock-function-name-face)))))))

;; Enhanced imenu with nerd-icons
(use-package imenu-list
  :ensure t
  :after (imenu nerd-icons)
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t)
  (imenu-list-position 'left)
  (imenu-list-size 0.25)
  (imenu-list-idle-update-delay 1)
  :config
  ;; Add nerd-icons to imenu-list
  (defun my/imenu-list-add-icons ()
    "Add nerd-icons to imenu-list entries."
    (when (and (display-graphic-p) 
               (featurep 'nerd-icons))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (looking-at "^\\(\\s-*\\)\\(.+\\)$")
            (let* ((indent (match-string 1))
                   (item (match-string 2))
                   (icon (cond
                         ((string-match-p "function\\|def\\|defun" item)
                          (nerd-icons-codicon "nf-cod-symbol_method"))
                         ((string-match-p "class\\|defclass\\|type" item)
                          (nerd-icons-codicon "nf-cod-symbol_class"))
                         ((string-match-p "variable\\|var\\|defvar\\|defcustom" item)
                          (nerd-icons-codicon "nf-cod-symbol_variable"))
                         ((string-match-p "macro\\|defmacro" item)
                          (nerd-icons-codicon "nf-cod-symbol_keyword"))
                         ((string-match-p "const\\|defconst" item)
                          (nerd-icons-codicon "nf-cod-symbol_constant"))
                         (t (nerd-icons-codicon "nf-cod-symbol_misc")))))
              (replace-match (format "%s%s %s" indent icon item))))
          (forward-line 1)))))
  
  (add-hook 'imenu-list-update-hook #'my/imenu-list-add-icons)
  
  :bind
  (("C-c i l" . imenu-list-smart-toggle)))

;; ==================================================
;; PROJECT.EL CONFIGURATION
;; ==================================================

(use-package project
  :ensure nil ; built-in in Emacs 28+
  :custom
  (project-switch-commands
   '((project-find-file "Find file" ?f)
     (project-find-regexp "Find regexp" ?g)
     (project-find-dir "Find directory" ?d)
     (project-vc-dir "VC-Dir" ?v)
     (project-shell "Shell" ?s)
     (project-eshell "Eshell" ?e)
     (project-compile "Compile" ?c)
     (magit-project-status "Magit" ?m)))
  (project-kill-buffer-conditions
   '(buffer-file-name ; Only kill file-visiting buffers
     (major-mode . fundamental-mode)
     (derived-mode . special-mode)
     (derived-mode . compilation-mode)
     (derived-mode . dired-mode)))
  :config
  ;; Project integration with perspective
  (defun my/project-switch-project-action (project-dir)
    "Switch to project with perspective integration."
    (interactive)
    (let ((project-name (file-name-nondirectory 
                        (directory-file-name project-dir))))
      (when (bound-and-true-p perspective-mode)
        (unless (persp-with-name-exists-p project-name)
          (persp-new project-name))
        (persp-switch project-name))
      (let ((default-directory project-dir))
        (call-interactively #'find-file))))
  
  ;; Enhanced project finding with consult integration
  (defun my/consult-project-buffer ()
    "Consult project buffers with current project context."
    (interactive)
    (if-let ((project (project-current)))
        (consult-project-buffer project)
      (consult-buffer)))
  
  (defun my/consult-project-find-file ()
    "Find file in current project using consult."
    (interactive)
    (if-let ((project (project-current)))
        (let ((default-directory (project-root project)))
          (consult-find default-directory))
      (consult-find default-directory)))
  
  ;; Project-aware treemacs integration
  (defun my/project-treemacs ()
    "Open treemacs for current project."
    (interactive)
    (if-let ((project (project-current)))
        (progn
          (treemacs-select-window)
          (treemacs-add-project-to-workspace (project-root project)
                                           (project-name project)))
      (treemacs)))
  
  (defun my/project-name (project)
    "Get a readable name for PROJECT."
    (file-name-nondirectory 
     (directory-file-name (project-root project))))
  
  :bind
  (("C-x p t" . my/project-treemacs)
   ("C-x p b" . my/consult-project-buffer)
   ("C-x p f" . my/consult-project-find-file)
   ("C-x p P" . my/project-switch-project-action)))

;; ==================================================
;; IBUFFER CONFIGURATION (Bufler-like interface)
;; ==================================================

(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-default-sorting-mode 'alphabetic)
  (ibuffer-always-show-last-buffer t)
  (ibuffer-view-ibuffer t)
  (ibuffer-use-header-line t)
  (ibuffer-default-directory default-directory)
  (ibuffer-use-other-window nil)
  (ibuffer-show-empty-filter-groups nil)
  :config
  ;; Bufler-like grouping with better organization
  (setq ibuffer-saved-filter-groups
        '(("Default"
           ("Modified" (and (modified . t)
                           (visiting-file . t)))
           ("Project Files" 
            (predicate . (lambda ()
                          (when-let ((project (project-current)))
                            (and buffer-file-name
                                 (string-prefix-p (project-root project)
                                                buffer-file-name))))))
           ("Dired" (mode . dired-mode))
           ("Org" (or (mode . org-mode)
                     (mode . org-agenda-mode)
                     (name . "^\\*Org .*\\*$")))
           ("Programming" 
            (or (derived-mode . prog-mode)
                (mode . compilation-mode)
                (mode . grep-mode)
                (mode . occur-mode)))
           ("Text & Markup"
            (or (derived-mode . text-mode)
                (mode . markdown-mode)
                (mode . rst-mode)
                (mode . tex-mode)
                (mode . latex-mode)))
           ("Mail & Communication"
            (or (mode . message-mode)
                (mode . mail-mode)
                (mode . gnus-group-mode)
                (mode . gnus-summary-mode)
                (mode . gnus-article-mode)))
           ("Version Control"
            (or (mode . magit-status-mode)
                (mode . magit-log-mode)
                (mode . magit-diff-mode)
                (mode . magit-revision-mode)
                (mode . vc-dir-mode)
                (mode . vc-log-mode)
                (mode . log-view-mode)))
           ("Shell & Terminal"
            (or (mode . shell-mode)
                (mode . eshell-mode)
                (mode . term-mode)
                (mode . vterm-mode)))
           ("Documentation"
            (or (mode . help-mode)
                (mode . Info-mode)
                (mode . Man-mode)
                (mode . woman-mode)
                (name . "^\\*info\\*$")))
           ("Emacs Internal"
            (or (name . "^\\*scratch\\*$")
                (name . "^\\*Messages\\*$")
                (name . "^\\*Backtrace\\*$")
                (name . "^\\*Compile-Log\\*$")
                (name . "^\\*Completions\\*$")
                (name . "^\\*Warnings\\*$")
                (name . "^\\*Apropos\\*$")
                (name . "^\\*Buffer List\\*$")
                (name . "^\\*Ibuffer\\*$"))))))
  
  ;; Automatically apply grouping
  (defun my/ibuffer-setup ()
    "Setup ibuffer with saved filter groups."
    (ibuffer-switch-to-saved-filter-groups "Default"))
  
  (add-hook 'ibuffer-mode-hook #'my/ibuffer-setup)
  
  ;; Custom ibuffer formats with better information
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 30 30 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 15 15 :left :elide)
                " "
                (vc-status 10 10 :left)
                " "
                filename-and-process)
          (mark " "
                (name 25 -1)
                " " filename)))
  
  ;; Better column for filename with project context
  (define-ibuffer-column filename-and-process
    (:name "File/Process" :inline t)
    (cond
     (buffer-file-name
      (let ((project (project-current nil default-directory)))
        (if project
            (file-relative-name buffer-file-name (project-root project))
          (abbreviate-file-name buffer-file-name))))
     ((get-buffer-process buffer)
      (format "(%s)" (process-name (get-buffer-process buffer))))
     (t (buffer-name buffer))))
  
  ;; VC status column
  (define-ibuffer-column vc-status
    (:name "VC")
    (if buffer-file-name
        (let ((state (vc-state buffer-file-name)))
          (cond
           ((eq state 'edited) "*")
           ((eq state 'added) "+")
           ((eq state 'removed) "-")
           ((eq state 'missing) "!")
           ((eq state 'ignored) "I")
           ((eq state 'unregistered) "?")
           (t "")))
      ""))
  
  :bind
  (("C-x C-b" . ibuffer)
   :map ibuffer-mode-map
   ("M-RET" . ibuffer-diff-with-file)
   ("C-c C-a" . ibuffer-auto-mode)))

;; Enhanced ibuffer with project integration
(use-package ibuffer-project
  :ensure t
  :after (ibuffer project)
  :custom
  (ibuffer-project-use-cache t)
  :config
  (defun my/ibuffer-project-setup ()
    "Setup ibuffer with project-aware grouping."
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))
  
  :hook
  (ibuffer . my/ibuffer-project-setup))

;; ibuffer with nerd-icons
(use-package nerd-icons-ibuffer
  :ensure t
  :after (ibuffer nerd-icons)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom
  (nerd-icons-ibuffer-formats
   '((mark modified read-only locked " "
           (icon 2 2 :left :elide)
           " "
           (name 25 25 :left :elide)
           " "
           (size-h 9 -1 :right)
           " "
           (mode 12 12 :left :elide)
           " "
           filename-and-process))))

;; ==================================================
;; TREEMACS CONFIGURATION
;; ==================================================

(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-collapse-dirs 3)
  (treemacs-deferred-git-apply-delay 0.5)
  (treemacs-directory-name-transformer #'identity)
  (treemacs-display-in-side-window t)
  (treemacs-eldoc-display 'simple)
  (treemacs-file-event-delay 2000)
  (treemacs-file-extension-regex treemacs-last-period-regex-value)
  (treemacs-file-follow-delay 0.2)
  (treemacs-file-name-transformer #'identity)
  (treemacs-follow-after-init t)
  (treemacs-expand-after-init t)
  (treemacs-find-workspace-method 'find-for-file-or-pick-first)
  (treemacs-git-command-pipe "")
  (treemacs-goto-tag-strategy 'refetch-index)
  (treemacs-header-scroll-indicators '(nil . "^^^^^^"))
  (treemacs-hide-dot-git-directory t)
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-is-never-other-window nil)
  (treemacs-max-git-entries 5000)
  (treemacs-missing-project-action 'ask)
  (treemacs-move-forward-on-expand nil)
  (treemacs-no-png-images nil)
  (treemacs-no-delete-other-windows t)
  (treemacs-project-follow-cleanup nil)
  (treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-position 'left)
  (treemacs-read-string-input 'from-child-frame)
  (treemacs-recenter-distance 0.1)
  (treemacs-recenter-after-file-follow nil)
  (treemacs-recenter-after-tag-follow nil)
  (treemacs-recenter-after-project-jump 'always)
  (treemacs-recenter-after-project-expand 'on-distance)
  (treemacs-litter-directories '("/node_modules" "/.venv" "/.cask" "/target" "/build" "/.git"))
  (treemacs-project-follow-into-home nil)
  (treemacs-show-cursor nil)
  (treemacs-show-hidden-files t)
  (treemacs-silent-filewatch nil)
  (treemacs-silent-refresh nil)
  (treemacs-sorting 'alphabetic-asc)
  (treemacs-select-when-already-in-treemacs 'move-back)
  (treemacs-space-between-root-nodes t)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1.5)
  (treemacs-text-scale nil)
  (treemacs-user-mode-line-format nil)
  (treemacs-user-header-line-format nil)
  (treemacs-wide-toggle-width 70)
  (treemacs-width 35)
  (treemacs-width-increment 1)
  (treemacs-width-is-initially-locked t)
  (treemacs-workspace-switch-cleanup nil)
  :config
  ;; Integration with perspective
  (defun my/treemacs-perspective-integration ()
    "Integrate treemacs with perspective."
    (when (and (bound-and-true-p perspective-mode)
               (treemacs-current-workspace))
      (let* ((current-persp (persp-name (persp-curr)))
             (workspace (treemacs-current-workspace)))
        (unless (string= (treemacs-workspace->name workspace) current-persp)
          (condition-case nil
              (treemacs-switch-workspace)
            (error nil))))))
  
  (add-hook 'treemacs-mode-hook #'my/treemacs-perspective-integration)
  
  ;; Custom treemacs actions
  (defun my/treemacs-consult-find-file ()
    "Use consult to find file in treemacs node."
    (interactive)
    (when-let ((path (treemacs-current-button-path)))
      (let ((default-directory (if (file-directory-p path) path
                                (file-name-directory path))))
        (consult-find default-directory))))
  
  (defun my/treemacs-add-current-project ()
    "Add current project to treemacs."
    (interactive)
    (if-let ((project (project-current)))
        (treemacs-add-project-to-workspace (project-root project)
                                         (my/project-name project))
      (message "Not in a project")))
  
  ;; Project integration
  (defun my/treemacs-project-follow-mode ()
    "Follow current project in treemacs."
    (when-let ((project (project-current)))
      (treemacs-find-file)))
  
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)
        ("C-x t f"   . my/treemacs-consult-find-file)
        ("C-x t p"   . my/treemacs-add-current-project))
  (:map treemacs-mode-map
        ("C-c f" . my/treemacs-consult-find-file)))

;; Treemacs with nerd-icons
(use-package treemacs-nerd-icons
  :ensure t
  :after (treemacs nerd-icons)
  :config
  (treemacs-load-theme "nerd-icons"))

;; Treemacs magit integration
(use-package treemacs-magit
  :ensure t
  :after (treemacs magit)
  :defer t)

;; Treemacs perspective integration
(use-package treemacs-persp
  :ensure t
  :after (treemacs perspective)
  :config
  (treemacs-set-scope-type 'Perspectives))

;; ==================================================
;; PERSPECTIVE.EL CONFIGURATION
;; ==================================================

(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-initial-frame-name "Main")
  (persp-suppress-no-prefix-key-warning t)
  (persp-interactive-completion-function #'completing-read)
  (persp-sort 'name)
  (persp-state-default-file (expand-file-name ".cache/persp-state" user-emacs-directory))
  :init
  ;; Ensure cache directory exists
  (let ((cache-dir (expand-file-name ".cache" user-emacs-directory)))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir t)))
  :config
  (persp-mode 1)
  
  ;; Integration with project.el
  (defun my/persp-switch-to-project ()
    "Switch to project perspective or create one."
    (interactive)
    (if-let ((project (project-current)))
        (let ((project-name (my/project-name project)))
          (unless (persp-with-name-exists-p project-name)
            (persp-new project-name))
          (persp-switch project-name)
          ;; Set up the project environment
          (let ((default-directory (project-root project)))
            (when (fboundp #'my/treemacs-add-current-project)
              (my/treemacs-add-current-project))))
      (call-interactively #'persp-switch)))
  
  ;; Integration with ibuffer
  (defun my/persp-ibuffer ()
    "Open ibuffer with current perspective buffers."
    (interactive)
    (let ((persp-buffers (persp-current-buffers)))
      (if persp-buffers
          (ibuffer nil (format "*Ibuffer-%s*" (persp-name (persp-curr)))
                   `((predicate . (lambda () (memq (current-buffer) ',persp-buffers)))))
        (ibuffer))))
  
  ;; Integration with consult
  (defun my/consult-buffer-perspective ()
    "Consult buffers in current perspective."
    (interactive)
    (let ((persp-buffers (persp-current-buffers)))
      (if persp-buffers
          (consult-buffer `(:name ,(format "Perspective: %s" (persp-name (persp-curr)))
                           :narrow ?p
                           :category buffer
                           :face consult-buffer
                           :history buffer-name-history
                           :state ,#'consult--buffer-state
                           :default t
                           :items ,(lambda () (mapcar #'buffer-name persp-buffers))))
        (consult-buffer))))
  
  ;; Perspective-aware window management
  (defun my/persp-kill-buffer* ()
    "Kill buffer and remove from perspective."
    (interactive)
    (let ((buffer (current-buffer)))
      (when (and (bound-and-true-p perspective-mode)
                 (persp-buffer-in-other-p buffer))
        (persp-remove-buffer buffer))
      (kill-buffer buffer)))
  
  ;; Save/restore perspective state
  (defun my/persp-save-state ()
    "Save perspective state."
    (interactive)
    (when (bound-and-true-p perspective-mode)
      (persp-state-save persp-state-default-file)))
  
  (defun my/persp-load-state ()
    "Load perspective state."
    (interactive)
    (when (and (bound-and-true-p perspective-mode)
               (file-exists-p persp-state-default-file))
      (persp-state-load persp-state-default-file)))
  
  ;; Auto-save perspective state
  (add-hook 'kill-emacs-hook #'my/persp-save-state)
  (add-hook 'after-init-hook #'my/persp-load-state)
  
  ;; Create project perspectives automatically
  (defun my/persp-auto-create-project-perspective ()
    "Automatically create perspective for project."
    (when-let ((project (project-current)))
      (let ((project-name (my/project-name project)))
        (unless (persp-with-name-exists-p project-name)
          (persp-new project-name)
          (message "Created perspective: %s" project-name)))))
  
  (add-hook 'project-find-file-hook #'my/persp-auto-create-project-perspective)
  
  :bind
  (("C-x C-b" . my/persp-ibuffer)
   ("C-x b"   . my/consult-buffer-perspective)
   ("C-x k"   . my/persp-kill-buffer*)
   ("C-x p s" . my/persp-switch-to-project)
   :map perspective-map
   ("b" . my/consult-buffer-perspective)
   ("i" . my/persp-ibuffer)
   ("S" . my/persp-save-state)
   ("L" . my/persp-load-state)))

;; ==================================================
;; INTEGRATION BINDINGS & COMMANDS
;; ==================================================

;; Global integration commands
(defun my/workspace-setup ()
  "Setup workspace with treemacs, ibuffer, and perspective."
  (interactive)
  (delete-other-windows)
  (treemacs)
  (other-window 1)
  (split-window-right)
  (my/persp-ibuffer)
  (other-window 1)
  (when (fboundp 'imenu-list-smart-toggle)
    (split-window-below)
    (other-window 1)
    (imenu-list-smart-toggle)
    (other-window -1)))

(defun my/project-workspace-setup ()
  "Setup project workspace."
  (interactive)
  (if-let ((project (project-current)))
      (progn
        (my/persp-switch-to-project)
        (my/workspace-setup)
        (when (treemacs-current-workspace)
          (my/treemacs-add-current-project)))
    (message "Not in a project")))

;; Quick navigation commands
(defun my/quick-switch-buffer ()
  "Quick buffer switching with consult."
  (interactive)
  (if (bound-and-true-p perspective-mode)
      (my/consult-buffer-perspective)
    (consult-buffer)))

(defun my/quick-find-file ()
  "Quick file finding with project awareness."
  (interactive)
  (if (project-current)
      (my/consult-project-find-file)
    (consult-find)))

(defun my/quick-imenu ()
  "Quick imenu with consult integration."
  (interactive)
  (if (fboundp 'consult-imenu)
      (consult-imenu)
    (imenu)))

;; ==================================================
;; GLOBAL KEYBINDINGS
;; ==================================================

(global-set-key (kbd "C-x C-b") #'my/persp-ibuffer)
(global-set-key (kbd "C-c w") #'my/workspace-setup)
(global-set-key (kbd "C-c W") #'my/project-workspace-setup)
(global-set-key (kbd "C-c b") #'my/quick-switch-buffer)
(global-set-key (kbd "C-c f") #'my/quick-find-file)
(global-set-key (kbd "C-c i") #'my/quick-imenu)

;; ==================================================
;; ADDITIONAL INTEGRATIONS
;; ==================================================

;; Which-key integration for better discoverability
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "C-x p"     "project"
    "C-x t"     "treemacs"
    "C-c M-p"   "perspective"
    "C-c w"     "workspace"
    "C-c W"     "project-workspace"
    "C-c b"     "buffer-switch"
    "C-c f"     "find-file"
    "C-c i"     "imenu"))

;; Completion integration enhancements
(with-eval-after-load 'vertico
  ;; Enhance vertico with better project/perspective context
  (defun my/vertico-insert-perspective-name ()
    "Insert current perspective name in vertico."
    (interactive)
    (when (bound-and-true-p perspective-mode)
      (insert (persp-name (persp-curr)))))
  
  (defun my/vertico-insert-project-name ()
    "Insert current project name in vertico."
    (interactive)
    (when-let ((project (project-current)))
      (insert (my/project-name project))))
  
  (define-key vertico-map (kbd "C-c p") #'my/vertico-insert-perspective-name)
  (define-key vertico-map (kbd "C-c P") #'my/vertico-insert-project-name))

;; Enhanced consult integration
(with-eval-after-load 'consult
  ;; Custom consult sources for better integration
  (defvar my/consult--source-perspective-buffer
    `(:name "Perspective Buffers"
      :narrow ?p
      :category buffer
      :face consult-buffer
      :history buffer-name-history
      :state ,#'consult--buffer-state
      :default t
      :items ,(lambda ()
                (when (bound-and-true-p perspective-mode)
                  (mapcar #'buffer-name (persp-current-buffers)))))
    "Perspective buffer candidate source for `consult-buffer'.")
  
  (defvar my/consult--source-project-buffer
    `(:name "Project Buffers"
      :narrow ?P
      :category buffer
      :face consult-buffer
      :history buffer-name-history
      :state ,#'consult--buffer-state
      :items ,(lambda ()
                (when-let ((project (project-current)))
                  (mapcar #'buffer-name 
                         (seq-filter (lambda (buf)
                                     (when-let ((file (buffer-file-name buf)))
                                       (string-prefix-p (project-root project) file)))
                                   (buffer-list))))))
    "Project buffer candidate source for `consult-buffer'.")
  
  ;; Add custom sources to consult-buffer
  (add-to-list 'consult-buffer-sources 'my/consult--source-perspective-buffer t)
  (add-to-list 'consult-buffer-sources 'my/consult--source-project-buffer t)
  
  ;; Enhanced consult-imenu with project context
  (defun my/consult-imenu-project ()
    "Consult imenu across all project buffers."
    (interactive)
    (if-let ((project (project-current)))
        (let ((project-buffers 
               (seq-filter (lambda (buf)
                            (when-let ((file (buffer-file-name buf)))
                              (string-prefix-p (project-root project) file)))
                          (buffer-list))))
          (consult-imenu-multi project-buffers))
      (consult-imenu)))
  
  (global-set-key (kbd "C-c I") #'my/consult-imenu-project))

;; ==================================================
;; WORKSPACE MANAGEMENT
;; ==================================================

;; Workspace state management
(defvar my/workspace-states '()
  "Saved workspace states.")

(defun my/save-workspace-state (name)
  "Save current workspace state with NAME."
  (interactive "sWorkspace name: ")
  (let ((state `((perspective . ,(when (bound-and-true-p perspective-mode)
                                  (persp-name (persp-curr))))
                (project . ,(when-let ((project (project-current)))
                             (project-root project)))
                (treemacs . ,(when (treemacs-current-workspace)
                              (treemacs-workspace->name (treemacs-current-workspace))))
                (window-config . ,(current-window-configuration)))))
    (setf (alist-get name my/workspace-states nil nil #'string=) state)
    (message "Workspace '%s' saved" name)))

(defun my/load-workspace-state (name)
  "Load workspace state with NAME."
  (interactive (list (completing-read "Load workspace: " 
                                     (mapcar #'car my/workspace-states))))
  (when-let ((state (alist-get name my/workspace-states nil nil #'string=)))
    (when-let ((persp-name (alist-get 'perspective state)))
      (when (bound-and-true-p perspective-mode)
        (persp-switch persp-name)))
    (when-let ((project-root (alist-get 'project state)))
      (when (file-exists-p project-root)
        (let ((default-directory project-root))
          (project-current))))
    (when-let ((window-config (alist-get 'window-config state)))
      (set-window-configuration window-config))
    (message "Workspace '%s' loaded" name)))

;; Workspace templates
(defun my/create-coding-workspace ()
  "Create a coding workspace layout."
  (interactive)
  (delete-other-windows)
  ;; Left: Treemacs
  (treemacs)
  ;; Center: Main editing area
  (other-window 1)
  (split-window-right)
  ;; Right top: ibuffer
  (other-window 1)
  (my/persp-ibuffer)
  ;; Right bottom: imenu-list
  (split-window-below)
  (other-window 1)
  (when (fboundp 'imenu-list-smart-toggle)
    (imenu-list-smart-toggle))
  ;; Back to main editing area
  (other-window -2))

(defun my/create-writing-workspace ()
  "Create a writing workspace layout."
  (interactive)
  (delete-other-windows)
  ;; Simple two-pane layout
  (split-window-right)
  ;; Left: main document
  ;; Right: reference/notes
  (other-window 1)
  (split-window-below)
  ;; Right top: reference
  ;; Right bottom: imenu for navigation
  (other-window 1)
  (when (fboundp 'imenu-list-smart-toggle)
    (imenu-list-smart-toggle))
  ;; Back to main document
  (other-window -2))

;; ==================================================
;; HYDRA INTEGRATION (Optional but recommended)
;; ==================================================

(use-package hydra
  :ensure t
  :config
  (defhydra my/workspace-hydra (:color blue :hint nil)
    "
^Workspace^       ^Navigation^      ^Project^         ^Perspective^
^─────────^       ^──────────^      ^───────^         ^───────────^
_w_: Setup        _b_: Buffer       _p_: Switch       _s_: Switch
_W_: Project      _f_: Find file    _t_: Treemacs     _n_: New
_c_: Coding       _i_: Imenu        _I_: Proj Imenu   _k_: Kill
_r_: Writing      _g_: Grep         _c_: Compile      _r_: Rename
_S_: Save state   _j_: Jump         _v_: VC           _b_: Buffers
_L_: Load state   _q_: Quit
"
    ("w" my/workspace-setup)
    ("W" my/project-workspace-setup)
    ("c" my/create-coding-workspace)
    ("r" my/create-writing-workspace)
    ("S" my/save-workspace-state)
    ("L" my/load-workspace-state)
    ("b" my/quick-switch-buffer)
    ("f" my/quick-find-file)
    ("i" my/quick-imenu)
    ("I" my/consult-imenu-project)
    ("g" consult-grep)
    ("j" consult-goto-line)
    ("p" project-switch-project)
    ("t" my/project-treemacs)
    ("c" project-compile)
    ("v" project-vc-dir)
    ("s" my/persp-switch-to-project)
    ("n" persp-new)
    ("k" persp-kill)
    ("r" persp-rename)
    ("b" my/persp-ibuffer)
    ("q" nil))
  
  (global-set-key (kbd "C-c SPC") #'my/workspace-hydra/body))

;; ==================================================
;; INTEGRATION HOOKS AND ADVICE
;; ==================================================

;; Auto-setup project workspace when switching projects
(defun my/auto-setup-project-workspace ()
  "Automatically setup workspace when switching to a project."
  (when-let ((project (project-current)))
    (run-with-idle-timer 0.5 nil #'my/treemacs-add-current-project)))

(add-hook 'project-find-file-hook #'my/auto-setup-project-workspace)

;; Auto-refresh treemacs when switching perspectives
(defun my/treemacs-refresh-on-perspective-switch ()
  "Refresh treemacs when switching perspectives."
  (when (and (bound-and-true-p treemacs-mode)
             (treemacs-current-workspace))
    (treemacs-refresh)))

(add-hook 'persp-switch-hook #'my/treemacs-refresh-on-perspective-switch)

;; Better buffer management
(defun my/kill-buffer-dwim ()
  "Kill buffer intelligently based on context."
  (interactive)
  (cond
   ;; In treemacs, close treemacs
   ((eq major-mode 'treemacs-mode)
    (treemacs-quit))
   ;; In ibuffer, close ibuffer
   ((eq major-mode 'ibuffer-mode)
    (quit-window))
   ;; In imenu-list, close imenu-list
   ((eq major-mode 'imenu-list-major-mode)
    (imenu-list-quit-window))
   ;; Otherwise, kill buffer normally
   (t (my/persp-kill-buffer*))))

(global-set-key (kbd "C-x k") #'my/kill-buffer-dwim)

;; ==================================================
;; PERFORMANCE OPTIMIZATIONS
;; ==================================================

;; Defer loading of heavy packages
(with-eval-after-load 'treemacs
  (setq treemacs-no-png-images nil) ; Use PNG images for better performance
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always))

;; Optimize ibuffer for large buffer lists
(with-eval-after-load 'ibuffer
  (setq ibuffer-expert t) ; Don't ask for confirmation
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-auto-mode t)) ; Auto-update

;; Optimize perspective for better performance
(with-eval-after-load 'perspective
  (setq persp-modestring-short t)
  (setq persp-show-modestring t))

;; ==================================================
;; ERROR HANDLING AND SAFETY
;; ==================================================

;; Safe project operations
(defun my/safe-project-current ()
  "Safely get current project."
  (condition-case nil
      (project-current)
    (error nil)))

;; Safe perspective operations
(defun my/safe-persp-switch (name)
  "Safely switch to perspective NAME."
  (condition-case err
      (persp-switch name)
    (error (message "Failed to switch to perspective %s: %s" name err))))

;; Safe treemacs operations
(defun my/safe-treemacs-add-project (path name)
  "Safely add project to treemacs."
  (condition-case err
      (treemacs-add-project-to-workspace path name)
    (error (message "Failed to add project to treemacs: %s" err))))

;; ==================================================
;; FINAL SETUP AND MESSAGES
;; ==================================================

;; Initialize workspace if needed
(defun my/initialize-workspace ()
  "Initialize workspace on startup."
  (when (and (bound-and-true-p perspective-mode)
             (= (length (persp-names)) 1)) ; Only default perspective
    (when-let ((project (my/safe-project-current)))
      (my/persp-switch-to-project))))

;; Run initialization after everything is loaded
(add-hook 'after-init-hook #'my/initialize-workspace)

;; Provide feature
(provide 'my-integrated-workspace)

;; Final message
(message "✓ Integrated workspace configuration loaded successfully!")
(message "  • Use C-c SPC for workspace hydra")
(message "  • Use C-c w for basic workspace setup")
(message "  • Use C-c W for project workspace setup")
(message "  • All packages integrated with nerd-icons and consult/vertico")

``````
