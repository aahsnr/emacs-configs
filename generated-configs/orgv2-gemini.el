;;; --- Enhanced Org Mode Configuration ---
;;; A comprehensive, clean, and modular setup for Org mode in Emacs.
;;; All configurations are consolidated into single `use-package` blocks for clarity and maintainability.

;; =============================================================================
;; SECTION 1: CORE INFRASTRUCTURE (Package Management & Keybindings)
;; =============================================================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/stable-packages/"))
(package-initialize)

;; --- General.el for Keybindings ---
;; Centralized keybinding setup using general.el.
;; We define leader and local-leader keys here to be used throughout the config.
(use-package general
  :ensure t
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer my/local-leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC m"
    :global-prefix "C-SPC m"))

;; =============================================================================
;; SECTION 2: ORG MODE CORE CONFIGURATION
;; =============================================================================

;; --- Directory Structure ---
;; Centralized path definitions for all Org-related files.
(defvar my/org-directory (expand-file-name "~/org/") "Base directory for all org files.")
(defvar my/org-roam-directory (expand-file-name "roam/" my/org-directory) "Directory for org-roam files.")
(defvar my/org-downloads-directory (expand-file-name "downloads/" my/org-directory) "Directory for org-download files.")
(defvar my/org-noter-directory (expand-file-name "noter/" my/org-directory) "Directory for org-noter files.")
(defvar my/org-archive-directory (expand-file-name "archive/" my/org-directory) "Directory for archived org files.")
(defvar my/org-backup-directory (expand-file-name "backups/" my/org-directory) "Directory for backup files.")
(defvar my/org-projects-directory (expand-file-name "projects/" my/org-directory) "Directory for project files.")
(defvar my/org-reviews-directory (expand-file-name "reviews/" my/org-directory) "Directory for review files.")

;; --- Create Directories ---
;; Ensure all necessary directories exist on startup.
(dolist (dir (list my/org-directory
                   my/org-roam-directory
                   my/org-downloads-directory
                   my/org-noter-directory
                   my/org-archive-directory
                   my/org-backup-directory
                   my/org-projects-directory
                   my/org-reviews-directory))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; --- Main Org Configuration ---
(use-package org
  :ensure t ;; For latest updates, can be set to nil to use built-in
  :pin gnu
  :hook ((org-mode . my/org-mode-setup)
         (org-mode . org-cdlatex-mode)
         (org-mode . org-indent-mode))
  :custom
  ;; Basic Setup
  (org-directory my/org-directory)
  (org-default-notes-file (concat my/org-directory "inbox.org"))
  (org-agenda-files (list my/org-directory))
  (org-archive-location (concat my/org-archive-directory "%s_archive::"))

  ;; Startup and Visuals
  (org-startup-folded 'overview)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-startup-with-latex-preview t)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-hide-emphasis-markers t)
  (org-ellipsis " ⤵")
  (org-cycle-separator-lines 2)

  ;; Editing Behavior
  (org-catch-invisible-edits 'smart)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-yank-adjusted-subtrees t)
  (org-return-follows-link t)
  (org-M-RET-may-split-line '((default . t)))

  ;; Logging
  (org-log-done 'time)
  (org-log-into-drawer t)

  ;; Lists and Checkboxes
  (org-list-allow-alphabetical t)

  ;; Babel
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)
  (org-confirm-babel-evaluate nil)
  (org-babel-load-languages
   '((emacs-lisp . t) (python . t) (shell . t) (js . t) (css . t) (plantuml . t) (dot . t) (latex . t) (calc . t)))

  ;; Export
  (org-export-in-background t)
  (org-export-with-smart-quotes t)
  (org-export-headline-levels 4)

  ;; Refile
  (org-refile-targets '((org-agenda-files :maxlevel . 4)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  
  ;; Priority Faces (Corrected Syntax)
  (org-priority-faces '((?A . (:foreground "#fb4934" :weight bold))
                        (?B . (:foreground "#fabd2f" :weight bold))
                        (?C . (:foreground "#83a598" :weight bold))))

  :config
  ;; -- Keybindings --
  (my/leader-keys
    "o"   '(:ignore t :which-key "Org")
    "oa"  'org-agenda
    "oc"  'org-capture
    "ol"  'org-store-link
    "ob"  'org-switchb)
  
  (my/local-leader-keys
    :keymaps 'org-mode-map
    "c"   'org-ctrl-c-ctrl-c
    "e"   'org-export-dispatch
    "i"   'org-insert-link
    "l"   'org-insert-last-stored-link
    "m"   'org-meta-return
    "n"   'org-add-note
    "o"   'org-open-at-point
    "p"   'org-priority
    "q"   'org-set-tags-command
    "r"   'org-refile
    "s"   'org-schedule
    "d"   'org-deadline
    "t"   'org-todo
    "x"   'org-toggle-checkbox)

  ;; -- TODO Keywords --
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#fb4934" :weight bold))
          ("NEXT" . (:foreground "#fabd2f" :weight bold))
          ("PROG" . (:foreground "#83a598" :weight bold))
          ("WAIT" . (:foreground "#d3869b" :weight bold))
          ("DONE" . (:foreground "#b8bb26" :weight bold))
          ("CANCELLED" . (:foreground "#928374" :weight bold))))

  ;; -- Tags --
  (setq org-tag-alist '((:startgroup)
                        ("@work" . ?w) ("@home" . ?h) ("@computer" . ?c)
                        (:endgroup)
                        ("project" . ?j) ("someday" . ?s) ("goal" . ?g)))

  ;; -- Font and Color Customization --
  (defun my/org-font-setup ()
    "Set custom fonts for org headings."
    (dolist (face '((org-level-1 . 1.4) (org-level-2 . 1.3) (org-level-3 . 1.2) (org-level-4 . 1.1) (org-level-5 . 1.05)))
      (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'bold :height (cdr face)))
    (set-face-attribute 'org-level-1 nil :foreground "#fb4934")
    (set-face-attribute 'org-level-2 nil :foreground "#fabd2f")
    (set-face-attribute 'org-level-3 nil :foreground "#83a598")
    (set-face-attribute 'org-level-4 nil :foreground "#d3869b")
    (set-face-attribute 'org-level-5 nil :foreground "#8ec07c")
    (set-face-attribute 'org-link nil :foreground "#83a598" :underline t)
    (set-face-attribute 'org-date nil :foreground "#d3869b"))

  ;; -- General Mode Setup Hook --
  (defun my/org-mode-setup ()
    "Custom settings to apply when Org mode starts."
    (visual-line-mode 1)
    (auto-fill-mode 1)
    (flyspell-mode 1)
    (my/org-font-setup)
    (setq fill-column 80))

  ;; -- Initial File Creation --
  (defun my/create-initial-org-files ()
    "Create essential org files if they don't exist."
    (let ((files '(("inbox.org" . "#+TITLE: Inbox\n\n* Tasks\n\n* Notes")
                   ("projects.org" . "#+TITLE: Projects")
                   ("journal.org" . "#+TITLE: Journal")
                   ("reading.org" . "#+TITLE: Reading List")
                   ("habits.org" . "#+TITLE: Habits")
                   ("goals.org" . "#+TITLE: Goals"))))
      (dolist (file-info files)
        (let* ((filename (car file-info))
               (template (cdr file-info))
               (full-path (concat my/org-directory filename)))
          (unless (file-exists-p full-path)
            (with-temp-file full-path (insert template)))))))

  (my/create-initial-org-files)
  (message "🎉 Org Mode Core configuration loaded successfully!"))

;; =============================================================================
;; SECTION 3: ORG AGENDA
;; =============================================================================

(use-package org-agenda
  :ensure nil ;; part of org
  :after org
  :custom
  (org-agenda-window-setup 'current-window)
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday nil)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "NEXT" ((org-agenda-overriding-header "⚡ Next Tasks")))
       (tags-todo "project" ((org-agenda-overriding-header "🚀 Active Projects")))))
     ("w" "Work"
      ((tags-todo "@work" ((org-agenda-overriding-header "💼 Work Tasks"))))
      ((tags-todo "@home" ((org-agenda-overriding-header "🏠 Home Tasks"))))
      ((tags-todo "@home" ((org-agenda-overriding-header "🏠 Home Tasks"))))))))

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "🔥 Overdue" :deadline past)
          (:name "📅 Today" :time-grid t :scheduled today)
          (:name "⚡ Next" :todo "NEXT")
          (:name "🔴 Important" :priority "A")
          (:name "🚀 Projects" :tag "project")
          (:name "⏳ Waiting" :todo "WAIT")
          (:auto-tags t)
          (:discard (:anything t)))))

;; =============================================================================
;; SECTION 4: ORG CAPTURE
;; =============================================================================

(use-package org-capture
  :ensure nil ;; part of org
  :after org
  :custom
  (org-capture-templates
   '(("t" "Task" entry (file+headline (concat my/org-directory "inbox.org") "Tasks")
      "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")
     ("n" "Note" entry (file+headline (concat my/org-directory "inbox.org") "Notes")
      "* %? :note:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")
     ("j" "Journal" entry (file+datetree (concat my/org-directory "journal.org"))
      "* %U %?")
     ("p" "Project" entry (file+headline (concat my/org-directory "projects.org") "Active Projects")
      "* NEXT %? :project:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n** Goals\n** Tasks\n"))))

;; =============================================================================
;; SECTION 5: KNOWLEDGE MANAGEMENT (ROAM, NOTER)
;; =============================================================================

(use-package org-roam
  :ensure t
  :after org
  :custom
  (org-roam-directory my/org-roam-directory)
  (org-roam-db-location (concat my/org-directory "org-roam.db"))
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template "${title:*} ${tags:10}")
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n#+date: %U\n#+filetags: \n\n"))))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>"))))
  :config
  (org-roam-db-autosync-mode)
  ;; Keybindings for Org Roam
  (my/leader-keys
    "n"   '(:ignore t :which-key "Roam")
    "nf"  'org-roam-node-find
    "ni"  'org-roam-node-insert
    "nc"  'org-roam-capture
    "nd"  'org-roam-dailies-capture-today
    "nt"  'org-roam-dailies-goto-today))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  :config
  (my/leader-keys "nu" 'org-roam-ui-open))

(use-package org-noter
  :ensure t
  :after org
  :custom
  (org-noter-notes-search-path (list my/org-noter-directory))
  (org-noter-separate-notes-from-heading t)
  (org-noter-insert-note-no-questions t))

;; =============================================================================
;; SECTION 6: VISUAL & UI ENHANCEMENTS
;; =============================================================================

(use-package org-modern
  :ensure t
  :after org
  :hook (org-mode . org-modern-mode))

(use-package org-fancy-priorities
  :ensure t
  :after org
  :hook (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("🔥" "⚡" "🔴" "🟡")))

(use-package org-fragtog
  :ensure t
  :after org
  :hook (org-mode . org-fragtog-mode))

;; =============================================================================
;; SECTION 7: UTILITIES & EXTENSIONS
;; =============================================================================

(use-package org-download
  :ensure t
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-image-dir my/org-downloads-directory)
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-download-image-attr-list '("#+ATTR_HTML: :width 80% :align center"
                                  "#+ATTR_ORG: :width 300"))
  :config
  (my/leader-keys "is" 'org-download-screenshot))

(use-package org-clock
  :ensure nil ;; part of org
  :after org
  :custom
  (org-clock-persist t)
  (org-clock-in-resume t)
  (org-clock-report-include-clocking-task t)
  (org-clock-out-remove-zero-time-clocks t)
  :config
  (org-clock-persistence-insinuate)
  ;; Keybindings
  (my/leader-keys
    "oj" 'org-clock-goto
    "ok" 'org-clock-in-last
    "oK" 'org-clock-out))

(use-package org-pomodoro
  :ensure t
  :after org
  :config
  (my/leader-keys "op" 'org-pomodoro))

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

;; =============================================================================
;; SECTION 8: CUSTOM FUNCTIONS & HYDRAS
;; =============================================================================

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-org-clock (:color blue :hint nil)
    "
Clock: _i_n, _o_ut, _c_ontinue | Display: _g_oto, _d_isplay, _r_eport
"
    ("i" org-clock-in)
    ("o" org-clock-out)
    ("c" org-clock-in-last)
    ("g" org-clock-goto)
    ("d" org-clock-display)
    ("r" org-clock-report)
    ("q" nil :color blue))

  (my/local-leader-keys
    :keymaps 'org-mode-map
    "C" 'hydra-org-clock/body))

;; --- Finalization ---
(provide 'org-config)
;;; org-config.el ends here
