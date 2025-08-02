``````el
;;; Treemacs Configuration
;;; A streamlined setup for Treemacs with integrations for evil, perspective,
;;; project.el, and nerd-icons.

;; 1. Custom Functions
;; ---------------------------------------------------------------------
;; Helper functions to integrate treemacs with project.el and provide
;; enhanced user interaction.

(defun my-treemacs--get-project-root ()
  "Return the current project.el project root, if any."
  (when (fboundp 'project-current)
    (when-let ((project (project-current)))
      (project-root project))))

(defun my-treemacs/toggle-or-select ()
  "Toggle treemacs, or select its window if it is already open."
  (interactive)
  (if (treemacs-is-treemacs-window-selected?)
      (treemacs-quit)
    (if (treemacs-get-local-window)
        (treemacs-select-window)
      (treemacs))))

(defun my-treemacs/find-in-current-project ()
  "Open treemacs, add the current project, and focus it."
  (interactive)
  (when-let ((project-root (my-treemacs--get-project-root)))
    (treemacs)
    (treemacs-add-and-display-project project-root)))

(defun my-treemacs/auto-add-project-on-find ()
  "Hook to automatically add the current project to the workspace."
  (when-let ((project-root (my-treemacs--get-project-root)))
    ;; Check if buffer's file is part of the current project before adding
    (when (and (buffer-file-name) (string-prefix-p project-root (buffer-file-name)))
      (unless (treemacs--project-in-workspace-p project-root)
        (treemacs-add-project-to-workspace project-root)))))


;; 2. Packages
;; ---------------------------------------------------------------------

(use-package nerd-icons
  :ensure t)

(use-package treemacs
  :ensure t
  :after nerd-icons
  :config
  ;; Custom settings that differ from the default values.
  (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
        treemacs-follow-after-init               t
        treemacs-expand-after-init               t
        treemacs-is-never-other-window           t
        treemacs-no-delete-other-windows         t
        treemacs-project-follow-cleanup          t
        treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                        'left
        treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
        treemacs-silent-filewatch                t
        treemacs-silent-refresh                  t
        treemacs-width                           35
        treemacs-width-is-initially-locked       t)

  ;; Ensure the cache directory exists
  (make-directory (file-name-directory treemacs-persist-file) t)

  ;; Enable essential modes
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-load-theme "nerd-icons")

  ;; Configure git integration based on system capabilities
  (pcase (cons (executable-find "git") treemacs-python-executable)
    (`(t . t)   (treemacs-git-mode 'deferred))
    (`(t . nil) (treemacs-git-mode 'simple)))

  ;; Hooks
  (add-hook 'find-file-hook #'my-treemacs/auto-add-project-on-find)
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (visual-line-mode 1)))
  (add-hook 'kill-emacs-hook #'treemacs-persist)

  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; Integrations with other packages
(use-package treemacs-evil
  :ensure t
  :after (treemacs evil)
  ;; For a more vim-like experience in the treemacs buffer
  :config
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
   "y"     'treemacs-copy-path-at-point))

(use-package treemacs-perspective
  :ensure t
  :after (treemacs perspective)
  :config (setq treemacs-scope-type 'Perspectives))


;; 3. Keybindings with General.el
;; ---------------------------------------------------------------------
(use-package general
  :ensure t
  :config
  (general-create-definer space-leader-def
    :prefix "SPC"
    :states '(normal visual motion)
    :keymaps 'override
    :global-prefix "C-SPC")

  (space-leader-def
    ;; File Tree
    "t"   '(:ignore t :wk "treemacs")
    "tt"  '(my-treemacs/toggle-or-select :wk "Toggle or Select")
    "tf"  '(my-treemacs/find-in-current-project :wk "Find in Project")

    ;; Project Management
    "p"   '(:ignore t :wk "project")
    "pa"  '(treemacs-add-project-to-workspace :wk "Add project to workspace")
    "pr"  '(treemacs-remove-project-from-workspace :wk "Remove project from workspace")

    ;; Workspace Management
    "w"   '(:ignore t :wk "workspace")
    "ws"  '(treemacs-switch-workspace :wk "Switch")
    "wc"  '(treemacs-create-workspace :wk "Create")
    "wr"  '(treemacs-rename-workspace :wk "Rename")
    "wd"  '(treemacs-remove-workspace :wk "Delete")
    "we"  '(treemacs-edit-workspaces :wk "Edit file")))
``````
