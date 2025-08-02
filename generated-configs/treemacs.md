``````el
;; Treemacs Configuration for Emacs 30
;; Comprehensive setup with nerd icons, evil, perspective.el, and project.el integration

;; Ensure nerd-icons is installed and configured
(use-package nerd-icons
  :ensure t
  :config
  ;; Install nerd fonts if not already done
  ;; Run M-x nerd-icons-install-fonts manually if needed
  )

;; Main treemacs package
(use-package treemacs
  :config
  (progn
    ;; Basic treemacs settings
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           t
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          t
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                t
          treemacs-silent-refresh                  t
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; Create cache directory if it doesn't exist
    (unless (file-exists-p (expand-file-name ".cache" user-emacs-directory))
      (make-directory (expand-file-name ".cache" user-emacs-directory) t))

    ;; Enable modes
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    
    ;; Git mode configuration with proper checks
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . nil)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; Treemacs Nerd Icons integration
(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons)
  :config
  (treemacs-load-theme "nerd-icons"))

;; Treemacs Evil integration (extra package, not on MELPA)
(use-package treemacs-evil
  :after (treemacs evil))

;; Treemacs Perspective integration (extra package, not on MELPA)
(use-package treemacs-perspective
  :after (treemacs perspective)
  :config
  (treemacs-set-scope-type 'Perspectives))

;; Enhanced keybindings with General
(use-package general
  :ensure t
  :config
  ;; Create leader key definer
  (general-create-definer space-leader-def
    :prefix "SPC"
    :states '(normal visual motion)
    :keymaps 'override
    :global-prefix "C-SPC") ; fallback for non-evil users

  ;; Project management keybindings
  (space-leader-def
    "p"   '(:ignore t :wk "project")
    "pt"  '(my/treemacs-project-toggle :wk "project treemacs")
    "pf"  '(my/treemacs-find-current-project :wk "find current project")
    "pc"  '(my/treemacs-add-current-project :wk "add current project")
    "pa"  '(treemacs-add-project-to-workspace :wk "add project to workspace")
    "pr"  '(treemacs-remove-project-from-workspace :wk "remove project")
    "ps"  '(treemacs-display-current-project-exclusively :wk "show current project only"))

  ;; File tree keybindings
  (space-leader-def
    "f"   '(:ignore t :wk "file")
    "ft"  '(my/treemacs-toggle-or-goto :wk "toggle treemacs")
    "fT"  '(treemacs-find-file :wk "treemacs find file")
    "f0"  '(treemacs-select-window :wk "select treemacs window"))

  ;; Toggle keybindings
  (space-leader-def
    "t"   '(:ignore t :wk "toggle")
    "tt"  '(treemacs :wk "treemacs")
    "tf"  '(treemacs-follow-mode :wk "toggle follow mode")
    "ta"  '(treemacs-filewatch-mode :wk "toggle filewatch mode")
    "th"  '(treemacs-toggle-show-dotfiles :wk "toggle hidden files")
    "tw"  '(treemacs-toggle-width :wk "toggle width"))

  ;; Workspace management
  (space-leader-def
    "w"   '(:ignore t :wk "workspace")
    "ws"  '(treemacs-switch-workspace :wk "switch workspace")
    "wc"  '(treemacs-create-workspace :wk "create workspace")
    "wd"  '(treemacs-remove-workspace :wk "delete workspace")
    "we"  '(treemacs-edit-workspaces :wk "edit workspaces")
    "wr"  '(treemacs-rename-workspace :wk "rename workspace"))

  ;; Quick access
  (space-leader-def
    "0"   '(treemacs-select-window :wk "select treemacs window")))

;; Custom functions for enhanced treemacs experience
(defun my/treemacs-toggle-or-goto ()
  "Toggle treemacs or go to treemacs window if it's visible."
  (interactive)
  (cond
   ;; If treemacs window is selected, close it
   ((treemacs-is-treemacs-window-selected?)
    (treemacs-quit))
   ;; If treemacs window exists but not selected, select it
   ((treemacs-get-local-window)
    (treemacs-select-window))
   ;; If no treemacs window, open it
   (t
    (treemacs))))

(defun my/treemacs-project-toggle ()
  "Toggle treemacs and add current project if using project.el."
  (interactive)
  (let ((project-root (when (fboundp 'project-current)
                       (when-let ((project (project-current)))
                         (project-root project)))))
    (if project-root
        (progn
          (treemacs-add-project-to-workspace project-root)
          (treemacs-display-current-project-exclusively))
      (treemacs))))

(defun my/treemacs-add-current-project ()
  "Add the current project.el project to treemacs workspace."
  (interactive)
  (when (fboundp 'project-current)
    (when-let ((project (project-current)))
      (let ((project-root (project-root project)))
        (treemacs-add-project-to-workspace project-root)
        (message "Added project: %s" project-root)))))

(defun my/treemacs-find-current-project ()
  "Find and display the current project.el project in treemacs."
  (interactive)
  (when (fboundp 'project-current)
    (when-let ((project (project-current)))
      (let ((project-root (project-root project)))
        (treemacs-add-project-to-workspace project-root)
        (treemacs-display-current-project-exclusively)
        (treemacs-find-file)))))

;; Treemacs mode specific keybindings for Evil users
(with-eval-after-load 'treemacs
  (when (featurep 'evil)
    (general-define-key
     :states '(normal motion)
     :keymaps 'treemacs-mode-map
     ;; Navigation
     "j" 'treemacs-next-line
     "k" 'treemacs-previous-line
     "h" 'treemacs-goto-parent-node
     "l" 'treemacs-TAB-action
     "H" 'treemacs-collapse-all-projects
     "L" 'treemacs-expand-all-projects
     "M-j" 'treemacs-next-project
     "M-k" 'treemacs-previous-project
     "gg" 'treemacs-goto-first-child
     "G" 'treemacs-goto-last-child
     
     ;; Actions
     "o" 'treemacs-visit-node-default
     "O" 'treemacs-visit-node-ace
     "TAB" 'treemacs-TAB-action
     "RET" 'treemacs-RET-action
     "s" 'treemacs-visit-node-horizontal-split
     "v" 'treemacs-visit-node-vertical-split
     
     ;; File operations
     "r" 'treemacs-refresh
     "R" 'treemacs-rename-file
     "cf" 'treemacs-create-file
     "cd" 'treemacs-create-dir
     "d" 'treemacs-delete-file
     "y" 'treemacs-copy-path-at-point
     
     ;; Toggles
     "th" 'treemacs-toggle-show-dotfiles
     "tw" 'treemacs-toggle-width
     
     ;; Project operations
     "pa" 'treemacs-add-project-to-workspace
     "pr" 'treemacs-remove-project-from-workspace
     "ps" 'treemacs-display-current-project-exclusively
     
     ;; Quit
     "q" 'treemacs-quit)))

;; Project.el integration
(defun my/treemacs-project-root ()
  "Get the root of the current project using project.el."
  (when (fboundp 'project-current)
    (when-let ((project (project-current)))
      (project-root project))))

(defun my/treemacs-auto-add-project ()
  "Automatically add project.el projects to treemacs when opening files."
  (when-let ((project-root (my/treemacs-project-root)))
    (when (buffer-file-name)
      (unless (treemacs-is-path-in-dir? (buffer-file-name) project-root)
        (treemacs-add-project-to-workspace project-root)))))

;; Hooks
(add-hook 'find-file-hook #'my/treemacs-auto-add-project)

(add-hook 'treemacs-mode-hook
          (lambda ()
            ;; Disable line numbers in treemacs
            (when (fboundp 'display-line-numbers-mode)
              (display-line-numbers-mode -1))
            ;; Enable visual line mode
            (visual-line-mode 1)))

;; Perspective integration
(with-eval-after-load 'perspective
  (with-eval-after-load 'treemacs-perspective
    (setq treemacs-scope-type 'Perspectives)))

;; Which-key integration
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "SPC p" "project"
    "SPC f" "file"
    "SPC t" "toggle"
    "SPC w" "workspace"
    "SPC 0" "treemacs window"))

;; Cleanup on exit
(add-hook 'kill-emacs-hook
          (lambda ()
            (when (fboundp 'treemacs-persist)
              (treemacs-persist))))

(message "Treemacs configuration loaded successfully!")``````
