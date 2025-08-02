;;; Enhanced Comprehensive Org-Mode Configuration
;;; A complete org-mode setup with modern features, built-in enhancements, and general.el keybindings

;; =============================================================================
;; GENERAL KEYBINDING SETUP
;; =============================================================================

(use-package general
  :ensure t
  :demand t
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
;; DIRECTORY AND PATH SETUP
;; =============================================================================

(defvar my/org-directory "~/org/"
  "Base directory for all org files.")

(defvar my/org-roam-directory (concat my/org-directory "roam/")
  "Directory for org-roam files.")

(defvar my/org-downloads-directory (concat my/org-directory "downloads/")
  "Directory for org-download files.")

(defvar my/org-noter-directory (concat my/org-directory "noter/")
  "Directory for org-noter files.")

(defvar my/org-archive-directory (concat my/org-directory "archive/")
  "Directory for archived org files.")

;; Create necessary directories
(dolist (dir (list my/org-directory
                   my/org-roam-directory
                   my/org-downloads-directory
                   my/org-noter-directory
                   my/org-archive-directory))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; =============================================================================
;; ORG-MODE CORE CONFIGURATION
;; =============================================================================

(use-package org
  :ensure nil
  :pin manual
  :defer t
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . flyspell-mode)
         (org-mode . display-line-numbers-mode)
         (org-mode . auto-save-mode)
         (org-mode . org-cdlatex-mode)
         (org-mode . abbrev-mode)
         (org-mode . auto-fill-mode))
  :custom
  ;; Basic settings
  (org-directory my/org-directory)
  (org-default-notes-file (concat my/org-directory "inbox.org"))
  (org-agenda-files (list my/org-directory))
  (org-archive-location (concat my/org-archive-directory "%s_archive::"))
  
  ;; Startup and display
  (org-startup-folded 'overview)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-startup-with-latex-preview t)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-use-property-inheritance t)
  (org-cycle-separator-lines 2)
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (org-insert-heading-respect-content t)
  (org-ellipsis " ⤵")
  (org-hide-emphasis-markers t)
  
  ;; Editing behavior
  (org-catch-invisible-edits 'smart)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-yank-adjusted-subtrees t)
  (org-yank-folded-subtrees nil)
  (org-M-RET-may-split-line '((default . t)))
  (org-return-follows-link t)
  (org-mouse-1-follows-link t)
  
  ;; Logging and timestamps
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  (org-log-redeadline 'note)
  (org-log-reschedule 'note)
  
  ;; Lists and checkboxes
  (org-list-allow-alphabetical t)
  (org-list-automatic-rules '((bullet . t) (checkbox . t) (indent . t)))
  (org-checkbox-statistics-hook '(org-update-parent-todo-statistics))
  (org-hierarchical-todo-statistics t)
  
  ;; Source blocks and babel
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)
  (org-link-elisp-confirm-function nil)
  
  ;; Export settings
  (org-export-in-background t)
  (org-export-with-smart-quotes t)
  (org-export-with-sub-superscripts nil)
  (org-export-with-toc nil)
  (org-export-headline-levels 4)
  (org-export-coding-system 'utf-8)
  
  ;; Performance optimizations
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-inhibit-startup t)
  (org-agenda-use-tag-inheritance nil)
  (org-agenda-ignore-properties '(effort appt category))
  (org-fast-tag-selection-single-key t)
  
  :config
  ;; File associations
  (add-to-list 'org-file-apps '("\\.pdf\\'" . default))
  (add-to-list 'org-file-apps '("\\.png\\'" . default))
  (add-to-list 'org-file-apps '("\\.jpg\\'" . default))
  
  ;; Refile settings
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)
  
  ;; Speed commands
  (setq org-use-speed-commands t
        org-speed-commands-user '(("0" . ignore)
                                  ("1" . ignore)
                                  ("2" . ignore)
                                  ("3" . ignore)
                                  ("4" . ignore)
                                  ("5" . ignore)
                                  ("6" . ignore)
                                  ("7" . ignore)
                                  ("8" . ignore)
                                  ("9" . ignore)
                                  ("a" . org-archive-subtree-default-with-confirmation)
                                  ("d" . org-deadline)
                                  ("s" . org-schedule)
                                  ("i" . org-clock-in)
                                  ("o" . org-clock-out)
                                  ("$" . org-archive-subtree)))
  
  ;; Tags
  (setq org-tag-alist '((:startgroup)
                        ("@work" . ?w) ("@home" . ?h) ("@computer" . ?c)
                        ("@phone" . ?p) ("@errands" . ?e)
                        (:endgroup)
                        (:startgroup)
                        ("project" . ?j) ("someday" . ?s) ("goal" . ?g)
                        (:endgroup)
                        ("meeting" . ?m) ("read" . ?r) ("finance" . ?f)
                        ("health" . ?l) ("learning" . ?n) ("creative" . ?a)))
  
  ;; Todo keywords with enhanced workflow
  (setq org-todo-keywords
        '((sequence "☛ TODO(t)" "⚡ NEXT(n)" "🔄 PROG(p)" "⏳ WAIT(w@/!)" "|" "✅ DONE(d!)" "❌ CANCELLED(c@)")
          (sequence "📋 PLAN(P)" "🔍 RESEARCH(R)" "📝 DRAFT(D)" "|" "📤 PUBLISHED(u)" "🗑 TRASH(T)")
          (sequence "🎯 GOAL(G)" "🚀 ACTIVE(A)" "⏸ PAUSED(x)" "|" "🏆 ACHIEVED(a)" "🚫 DROPPED(X)")))
  
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
  
  ;; Priority settings
  (setq org-priority-faces '((?A . (:foreground "#fb4934" :weight bold)))
                            (?B . (:foreground "#fabd2f" :weight bold))
                            (?C . (:foreground "#83a598" :weight bold)))
  
  ;; Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (sql . t)
     (sqlite . t)
     (js . t)
     (css . t)
     (plantuml . t)
     (dot . t)
     (org . t)
     (latex . t)
     (calc . t))))

;; =============================================================================
;; ORG BUILT-IN ENHANCEMENTS
;; =============================================================================

(use-package org-id
  :ensure nil
  :after org
  :custom
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-id-locations-file (concat my/org-directory ".org-id-locations"))
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
  (org-attach-directory (concat my/org-directory "attachments/"))
  (org-attach-store-link-p 'attached)
  (org-attach-auto-tag "attachment"))

(use-package org-crypt
  :ensure nil
  :after org
  :custom
  (org-crypt-key nil)  ; Use symmetric encryption
  (org-tags-exclude-from-inheritance '("crypt"))
  :config
  (org-crypt-use-before-save-magic))

;; =============================================================================
;; ORG MODERN VISUAL ENHANCEMENTS
;; =============================================================================

(use-package org-modern
  :ensure t
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"))
  (org-modern-list '((43 . "➤") (45 . "–") (42 . "•")))
  (org-modern-tag nil)
  (org-modern-priority nil)
  (org-modern-todo nil)
  (org-modern-checkbox '((88 . "☑") (45 . "☐") (32 . "☐")))
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2)
  (org-modern-block nil)
  (org-modern-keyword nil)
  (org-modern-timestamp t)
  (org-modern-statistics t)
  (org-modern-progress nil))

(use-package org-fancy-priorities
  :ensure t
  :after org
  :hook (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("🔥" "⚡" "🔴" "🟡")))

;; =============================================================================
;; ORG PRETTIFY SYMBOLS
;; =============================================================================

(use-package org
  :ensure nil
  :hook (org-mode . my/org-prettify-symbols)
  :config
  (defun my/org-prettify-symbols ()
    "Setup prettify symbols for org-mode."
    (setq prettify-symbols-alist
          '(("#+BEGIN_SRC" . "")
            ("#+END_SRC" . "")
            ("#+BEGIN_EXAMPLE" . "")
            ("#+END_EXAMPLE" . "")
            ("#+BEGIN_QUOTE" . """)
            ("#+END_QUOTE" . """)
            ("#+RESULTS:" . "")
            ("[ ]" . "☐")
            ("[-]" . "◐")
            ("[X]" . "☑")
            ("lambda" . "λ")
            ("->" . "→")
            ("=>" . "⇒")
            ("<=" . "≤")
            (">=" . "≥")
            ("!=" . "≠")
            ("#+TITLE:" . "")
            ("#+AUTHOR:" . "")
            ("#+EMAIL:" . "")
            ("#+DATE:" . "")
            ("#+PROPERTY:" . "")
            ("#+OPTIONS:" . "")
            ("#+STARTUP:" . "")
            ("#+TAGS:" . "")
            ("#+FILETAGS:" . "")))
    (prettify-symbols-mode 1)))

;; =============================================================================
;; FONT AND COLOR CONFIGURATION
;; =============================================================================

(use-package org
  :ensure nil
  :hook (org-mode . my/org-font-setup)
  :hook (org-mode . my/org-gruvbox-colors)
  :config
  (defun my/org-font-setup ()
    "Configure fonts for org-mode with JetBrains Mono Nerd Font."
    (dolist (face '((org-level-1 . 1.4)
                    (org-level-2 . 1.3)
                    (org-level-3 . 1.2)
                    (org-level-4 . 1.1)
                    (org-level-5 . 1.05)
                    (org-level-6 . 1.0)
                    (org-level-7 . 1.0)
                    (org-level-8 . 1.0)))
      (set-face-attribute (car face) nil
                          :font "JetBrainsMono Nerd Font"
                          :weight 'bold
                          :height (cdr face))))
  
  (defun my/org-gruvbox-colors ()
    "Apply Gruvbox dark colors to org-mode elements."
    (set-face-attribute 'org-level-1 nil :foreground "#fb4934") ; red
    (set-face-attribute 'org-level-2 nil :foreground "#fabd2f") ; yellow
    (set-face-attribute 'org-level-3 nil :foreground "#83a598") ; blue
    (set-face-attribute 'org-level-4 nil :foreground "#d3869b") ; purple
    (set-face-attribute 'org-level-5 nil :foreground "#8ec07c") ; aqua
    (set-face-attribute 'org-level-6 nil :foreground "#fe8019") ; orange
    (set-face-attribute 'org-level-7 nil :foreground "#b8bb26") ; green
    (set-face-attribute 'org-level-8 nil :foreground "#a89984") ; gray
    
    ;; Additional elements
    (set-face-attribute 'org-todo nil :foreground "#fb4934" :weight 'bold)
    (set-face-attribute 'org-done nil :foreground "#b8bb26" :weight 'bold)
    (set-face-attribute 'org-priority nil :foreground "#fabd2f")
    (set-face-attribute 'org-tag nil :foreground "#8ec07c")
    (set-face-attribute 'org-link nil :foreground "#83a598" :underline t)
    (set-face-attribute 'org-date nil :foreground "#d3869b")
    (set-face-attribute 'org-special-keyword nil :foreground "#928374")
    
    ;; Enhanced emphasis
    (setq org-emphasis-alist
          '(("*" (bold :foreground "#fb4934"))
            ("/" (italic :foreground "#83a598"))
            ("_" (:underline t :foreground "#8ec07c"))
            ("=" (:background "#3c3836" :foreground "#fbf1c7"))
            ("~" (:background "#3c3836" :foreground "#fe8019"))
            ("+" (:strike-through t :foreground "#928374"))))))

;; =============================================================================
;; ORG AGENDA CONFIGURATION
;; =============================================================================

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
            (todo "⚡ NEXT"
                  ((org-agenda-overriding-header "⚡ Next Tasks")))
            (tags-todo "project/🚀 ACTIVE"
                       ((org-agenda-overriding-header "🚀 Active Projects")))
            (tags-todo "+PRIORITY=\"A\""
                       ((org-agenda-overriding-header "🔥 High Priority")))
            (todo "⏳ WAIT"
                  ((org-agenda-overriding-header "⏳ Waiting On")))
            (tags-todo "+habit"
                       ((org-agenda-overriding-header "🔄 Habits")))
            (stuck ""
                   ((org-agenda-overriding-header "🚫 Stuck Projects")))))
          
          ("n" "Next Tasks"
           ((todo "⚡ NEXT"
                  ((org-agenda-overriding-header "⚡ Next Tasks")))))
          
          ("w" "Work Context" 
           ((tags-todo "@work/⚡ NEXT"
                       ((org-agenda-overriding-header "💼 Work Next")))
            (tags-todo "@work/☛ TODO"
                       ((org-agenda-overriding-header "💼 Work Tasks")))
            (tags-todo "@work+project/🚀 ACTIVE"
                       ((org-agenda-overriding-header "💼 Work Projects")))))
          
          ("h" "Home Context"
           ((tags-todo "@home/⚡ NEXT"
                       ((org-agenda-overriding-header "🏠 Home Next")))
            (tags-todo "@home/☛ TODO"
                       ((org-agenda-overriding-header "🏠 Home Tasks")))))
          
          ("p" "Projects Overview"
           ((tags "project"
                  ((org-agenda-overriding-header "📋 All Projects")))))
          
          ("g" "Goals Review"
           ((tags-todo "goal"
                       ((org-agenda-overriding-header "🎯 Goals")))))
          
          ("r" "Review"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-overriding-header "📅 Today")))
            (todo "✅ DONE"
                  ((org-agenda-overriding-header "✅ Completed Today")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottoday))))
            (stuck ""
                   ((org-agenda-overriding-header "🚫 Stuck Projects"))))))))

(use-package org-super-agenda
  :ensure t
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

;; =============================================================================
;; ORG CAPTURE TEMPLATES
;; =============================================================================

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

;; =============================================================================
;; ORG ROAM V2 CONFIGURATION
;; =============================================================================

(use-package org-roam
  :ensure t
  :after org
  :custom
  (org-roam-directory my/org-roam-directory)
  (org-roam-db-location (concat my/org-directory "org-roam.db"))
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n#+date: %U\n#+filetags: \n\n")
      :unnarrowed t)
     
     ("n" "note" plain "* ${title}\n%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n#+date: %U\n#+filetags: note\n\n")
      :unnarrowed t)
     
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** ☛ TODO Add initial tasks\n\n* Resources\n\n* Timeline\n\n* Notes\n\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n#+date: %U\n#+filetags: project\n\n")
      :unnarrowed t)
     
     ("b" "book notes" plain "* Summary\n\n%?\n\n* Key Takeaways\n\n* Important Quotes\n\n* Personal Reflections\n\n* Related Notes\n\n* Action Items\n\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n#+date: %U\n#+filetags: book literature\n\n")
      :unnarrowed t)
     
     ("r" "reference" plain "* Overview\n\n%?\n\n* Key Points\n\n* Related Topics\n\n* Sources\n\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n#+date: %U\n#+filetags: reference\n\n")
      :unnarrowed t)
     
     ("c" "concept" plain "* Definition\n\n%?\n\n* Examples\n\n* Applications\n\n* Related Concepts\n\n* References\n\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n#+date: %U\n#+filetags: concept\n\n")
      :unnarrowed t)
     
     ("m" "meeting" plain "* Attendees\n\n* Agenda\n\n* Discussion\n\n%?\n\n* Action Items\n\n* Follow-up\n\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n#+date: %U\n#+filetags: meeting\n\n")
      :unnarrowed t)))
  
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?"
      :target (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d>\n#+date: %U\n#+filetags: daily\n\n"))
     
     ("t" "task" entry "* ☛ TODO %?\n  SCHEDULED: %t\n"
      :target (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d>\n#+date: %U\n#+filetags: daily\n\n"))
     
     ("j" "journal" entry "* %<%H:%M> %?\n"
      :target (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d>\n#+date: %U\n#+filetags: daily\n\n"))
     
     ("m" "meeting" entry "* %<%H:%M> Meeting: %?\n** Attendees\n** Notes\n** Action Items\n"
      :target (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d>\n#+date: %U\n#+filetags: daily\n\n"))))
  
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:10}" 'face 'org-tag)))
  
  :config
  (org-roam-db-autosync-mode)
  
  ;; Create roam dailies directory if it doesn't exist
  (unless (file-directory-p (concat org-roam-directory org-roam-dailies-directory))
    (make-directory (concat org-roam-directory org-roam-dailies-directory) t)))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

;; =============================================================================
;; ORG DOWNLOAD CONFIGURATION
;; =============================================================================

(use-package org-download
  :ensure t
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

;; =============================================================================
;; ORG NOTER CONFIGURATION
;; =============================================================================

(use-package org-noter
  :ensure t
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

;; =============================================================================
;; PDF TOOLS INTEGRATION
;; =============================================================================

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1)
  (pdf-annot-activate-created-annotations t))

;; =============================================================================
;; ORG FRAGTOG (LATEX FRAGMENTS)
;; =============================================================================

(use-package org-fragtog
  :ensure t
  :after org
  :hook (org-mode . org-fragtog-mode)
  :custom
  (org-fragtog-preview-delay 0.5))

;; =============================================================================
;; ADDITIONAL BUILT-IN ENHANCEMENTS
;; =============================================================================

(use-package calendar
  :ensure nil
  :custom
  (calendar-mark-holidays-flag t)
  (calendar-mark-diary-entries-flag t)
  (calendar-view-holidays-initially-flag t))

(use-package diary-lib
  :ensure nil
  :custom
  (diary-file (concat my/org-directory "diary"))
  (diary-display-function 'diary-fancy-display)
  (diary-list-entries-hook '(diary-include-other-diary-files diary-sort-entries))
  (diary-hook '(diary-make-entry)))

(use-package appt
  :ensure nil
  :custom
  (appt-display-diary nil)
  (appt-display-duration 30)
  (appt-display-mode-line t)
  (appt-disp-window-function 'appt-disp-window)
  :config
  (appt-activate 1))

;; Holidays customization
(use-package holidays
  :ensure nil
  :custom
  (holiday-general-holidays nil)
  (holiday-local-holidays nil)
  (holiday-other-holidays nil)
  (holiday-bahai-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil))

;; =============================================================================
;; CUSTOM UTILITY FUNCTIONS
;; =============================================================================

(use-package org
  :ensure nil
  :config
  (defun my/org-insert-screenshot ()
    "Take a screenshot and insert it into the current org file."
    (interactive)
    (org-download-screenshot))
  
  (defun my/org-archive-done-tasks ()
    "Archive all DONE tasks in the current buffer."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file)
    (message "Archived all DONE tasks"))
  
  (defun my/org-clock-in-last-task ()
    "Clock in the last task."
    (interactive)
    (org-clock-in-last))
  
  (defun my/org-insert-heading-inactive-timestamp ()
    "Insert heading with inactive timestamp."
    (interactive)
    (org-insert-heading-respect-content)
    (org-insert-time-stamp (current-time) t t))
  
  (defun my/org-toggle-emphasis-markers ()
    "Toggle visibility of org emphasis markers."
    (interactive)
    (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
    (font-lock-flush)
    (message "Emphasis markers %s" (if org-hide-emphasis-markers "hidden" "visible")))
  
  (defun my/org-count-words ()
    "Count words in current org buffer or subtree."
    (interactive)
    (save-excursion
      (let ((begin (if (org-region-active-p) (region-beginning)
                     (if (org-at-heading-p) (org-back-to-heading) (point-min))))
            (end (if (org-region-active-p) (region-end)
                   (if (org-at-heading-p) (org-end-of-subtree) (point-max)))))
        (message "Word count: %d" (count-words begin end)))))
  
  (defun my/org-insert-structure-template ()
    "Insert a structure template with completion."
    (interactive)
    (let ((template (completing-read "Template: " 
                                     '("src" "example" "quote" "verse" "center" "comment"))))
      (org-insert-structure-template template)))
  
  (defun my/org-agenda-bulk-mark-regexp-category ()
    "Bulk mark items by category using regexp."
    (interactive)
    (let ((regexp (read-string "Mark items with category matching regexp: ")))
      (org-agenda-bulk-unmark-all)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (when (get-text-property (point) 'org-category)
            (org-agenda-bulk-mark))))))
  
  (defun my/org-sort-list-by-todo-keyword ()
    "Sort list items by TODO keyword."
    (interactive)
    (org-sort-list nil ?f
                   (lambda ()
                     (if (looking-at org-complex-heading-regexp)
                         (or (match-string-no-properties 2) "")
                       ""))))
  
  (defun my/org-insert-elisp-template ()
    "Insert an elisp source block template."
    (interactive)
    (insert "#+BEGIN_SRC elisp\n\n#+END_SRC")
    (forward-line -1))
  
  (defun my/org-babel-load-file-async (file)
    "Load org babel file asynchronously."
    (interactive "fFile: ")
    (async-start
     `(lambda ()
        (require 'org)
        (org-babel-load-file ,file))
     (lambda (result)
       (message "Finished loading %s" file))))
  
  (defun my/org-create-project-from-template ()
    "Create a new project from template."
    (interactive)
    (let* ((project-name (read-string "Project name: "))
           (project-file (concat my/org-directory "projects/" 
                                (downcase (replace-regexp-in-string " " "-" project-name)) 
                                ".org"))
           (project-template 
            (concat "#+TITLE: " project-name "\n"
                    "#+DATE: " (format-time-string "%Y-%m-%d") "\n"
                    "#+FILETAGS: project\n\n"
                    "* Project Overview\n\n"
                    "** Goals\n\n"
                    "** Success Criteria\n\n"
                    "** Constraints\n\n"
                    "* Planning\n\n"
                    "** Tasks\n\n"
                    "*** ☛ TODO Define project scope\n"
                    "*** ☛ TODO Create detailed timeline\n"
                    "*** ☛ TODO Identify required resources\n\n"
                    "** Timeline\n\n"
                    "** Resources\n\n"
                    "* Execution\n\n"
                    "** Progress Log\n\n"
                    "** Issues and Blockers\n\n"
                    "* Review\n\n"
                    "** Lessons Learned\n\n"
                    "** Next Steps\n\n")))
      (unless (file-directory-p (concat my/org-directory "projects/"))
        (make-directory (concat my/org-directory "projects/") t))
      (with-temp-file project-file
        (insert project-template))
      (find-file project-file)
      (goto-char (point-min))
      (re-search-forward "\\*\\* Goals" nil t)
      (end-of-line)))
  
  (defun my/org-weekly-review ()
    "Open weekly review template."
    (interactive)
    (let* ((week-start (format-time-string "%Y-%m-%d" 
                                          (time-subtract (current-time) 
                                                        (days-to-time 
                                                         (string-to-number 
                                                          (format-time-string "%w"))))))
           (review-file (concat my/org-directory "reviews/weekly-" week-start ".org"))
           (review-template
            (concat "#+TITLE: Weekly Review - " week-start "\n"
                    "#+DATE: " (format-time-string "%Y-%m-%d") "\n"
                    "#+FILETAGS: review weekly\n\n"
                    "* Week in Review\n\n"
                    "** Accomplishments\n\n"
                    "** Challenges\n\n"
                    "** Key Insights\n\n"
                    "* Goals Review\n\n"
                    "** Completed\n\n"
                    "** In Progress\n\n"
                    "** Not Started\n\n"
                    "* Next Week Planning\n\n"
                    "** Priorities\n\n"
                    "** Focus Areas\n\n"
                    "** Potential Obstacles\n\n"
                    "* Metrics\n\n"
                    "** Time Tracking\n\n"
                    "** Habit Completion\n\n"
                    "* Notes\n\n")))
      (unless (file-directory-p (concat my/org-directory "reviews/"))
        (make-directory (concat my/org-directory "reviews/") t))
      (unless (file-exists-p review-file)
        (with-temp-file review-file
          (insert review-template)))
      (find-file review-file))))

;; =============================================================================
;; INITIAL FILE CREATION
;; =============================================================================

(use-package org
  :ensure nil
  :config
  (defun my/create-initial-org-files ()
    "Create initial org files if they don't exist."
    (let ((files '(("inbox.org" . "#+TITLE: Inbox\n#+DATE: %s\n#+FILETAGS: inbox\n\n* Tasks\n\n* Notes\n\n* Meetings\n\n")
                   ("projects.org" . "#+TITLE: Projects\n#+DATE: %s\n#+FILETAGS: projects\n\n* Active Projects\n\n* Planned Projects\n\n* Completed Projects\n\n")
                   ("journal.org" . "#+TITLE: Journal\n#+DATE: %s\n#+FILETAGS: journal\n\n")
                   ("reading.org" . "#+TITLE: Reading List\n#+DATE: %s\n#+FILETAGS: reading books\n\n* Currently Reading\n\n* To Read\n\n* Completed\n\n")
                   ("habits.org" . "#+TITLE: Habits\n#+DATE: %s\n#+FILETAGS: habits\n\n* Daily Habits\n\n* Weekly Habits\n\n* Monthly Habits\n\n")
                   ("goals.org" . "#+TITLE: Goals\n#+DATE: %s\n#+FILETAGS: goals\n\n* Short-term Goals\n\n* Long-term Goals\n\n* Completed Goals\n\n")
                   ("ideas.org" . "#+TITLE: Ideas\n#+DATE: %s\n#+FILETAGS: ideas\n\n* Project Ideas\n\n* Random Thoughts\n\n* Interesting Concepts\n\n")
                   ("links.org" . "#+TITLE: Useful Links\n#+DATE: %s\n#+FILETAGS: links resources\n\n* Development\n\n* Learning\n\n* Tools\n\n* Articles\n\n")
                   ("work.org" . "#+TITLE: Work\n#+DATE: %s\n#+FILETAGS: work\n\n* Current Tasks\n\n* Projects\n\n* Meetings\n\n* Notes\n\n"))))
      (dolist (file-info files)
        (let ((file (car file-info))
              (template (cdr file-info)))
          (let ((full-path (concat my/org-directory file)))
            (unless (file-exists-p full-path)
              (with-temp-file full-path
                (insert (format template (format-time-string "%Y-%m-%d"))))))))))
  
  ;; Run initial setup
  (my/create-initial-org-files))

;; =============================================================================
;; KEYBINDINGS WITH GENERAL.EL
;; =============================================================================

(use-package general
  :ensure t
  :config
  ;; Global org-mode keybindings
  (my/leader-keys
    "o"   '(:ignore t :which-key "org")
    "oa"  'org-agenda
    "oc"  'org-capture
    "ol"  'org-store-link
    "ob"  'org-switchb
    "oj"  'org-clock-goto
    "ok"  'org-clock-in-last
    "oK"  'org-clock-out
    "op"  'org-pomodoro
    "or"  'org-refile
    "os"  'org-save-all-org-buffers
    "ot"  'org-todo-list
    "oT"  'org-tags-view
    "ox"  'org-export-dispatch)
  
  ;; Org-roam keybindings
  (my/leader-keys
    "n"   '(:ignore t :which-key "notes/roam")
    "nf"  'org-roam-node-find
    "ni"  'org-roam-node-insert
    "nc"  'org-roam-capture
    "nd"  'org-roam-dailies-capture-today
    "nt"  'org-roam-dailies-goto-today
    "ny"  'org-roam-dailies-goto-yesterday
    "nr"  'org-roam-node-random
    "ns"  'org-roam-db-sync
    "ng"  'org-roam-graph
    "nu"  'org-roam-ui-open
    "nb"  'org-roam-buffer-toggle)
  
  ;; Custom functions
  (my/leader-keys
    "i"   '(:ignore t :which-key "insert/custom")
    "is"  'my/org-insert-screenshot
    "ia"  'my/org-archive-done-tasks
    "ic"  'my/org-clock-in-last-task
    "ih"  'my/org-insert-heading-inactive-timestamp
    "it"  'my/org-insert-structure-template
    "ie"  'my/org-insert-elisp-template
    "ip"  'my/org-create-project-from-template
    "ir"  'my/org-weekly-review)
  
  ;; Utilities
  (my/leader-keys
    "u"   '(:ignore t :which-key "utilities")
    "uw"  'my/org-count-words
    "ue"  'my/org-toggle-emphasis-markers
    "us"  'my/org-sort-list-by-todo-keyword))

;; Local leader keys for org-mode
(use-package general
  :ensure t
  :config
  (my/local-leader-keys
    :keymaps 'org-mode-map
    "'"   'org-edit-special
    "a"   'org-attach
    "b"   'org-tree-to-indirect-buffer
    "c"   'org-ctrl-c-ctrl-c
    "d"   'org-deadline
    "e"   'org-export-dispatch
    "f"   'org-footnote-action
    "h"   'org-toggle-heading
    "i"   'org-insert-link
    "k"   'org-store-link
    "l"   'org-insert-last-stored-link
    "m"   'org-meta-return
    "n"   'org-add-note
    "o"   'org-open-at-point
    "p"   'org-priority
    "q"   'org-set-tags-command
    "r"   'org-refile
    "s"   'org-schedule
    "t"   'org-todo
    "T"   'org-show-todo-tree
    "v"   'org-reveal
    "w"   'org-cut-subtree
    "x"   'org-toggle-checkbox
    "y"   'org-copy-subtree
    "z"   'org-add-note
    
    ;; Subtree operations
    "S"   '(:ignore t :which-key "subtree")
    "Sh"  'org-promote-subtree
    "Sl"  'org-demote-subtree
    "Sk"  'org-move-subtree-up
    "Sj"  'org-move-subtree-down
    "Sr"  'org-refile
    "Ss"  'org-sort
    "Sa"  'org-archive-subtree
    
    ;; Tables
    "tt"  '(:ignore t :which-key "tables")
    "tta" 'org-table-align
    "ttb" 'org-table-blank-field
    "ttc" 'org-table-create-or-convert-from-region
    "tte" 'org-table-export
    "ttf" 'org-table-get-formula
    "tth" 'org-table-previous-field
    "ttH" 'org-table-move-column-left
    "tti" 'org-table-import
    "ttj" 'org-table-next-row
    "ttJ" 'org-table-move-row-down
    "ttK" 'org-table-move-row-up
    "ttl" 'org-table-next-field
    "ttL" 'org-table-move-column-right
    "ttn" 'org-table-create
    "ttN" 'org-table-create-with-table.el
    "ttr" 'org-table-recalculate
    "tts" 'org-table-sort-lines
    "ttw" 'org-table-wrap-region
    
    ;; Clock
    "cc"  '(:ignore t :which-key "clock")
    "cci" 'org-clock-in
    "cco" 'org-clock-out
    "ccr" 'org-clock-report
    "ccl" 'org-clock-in-last
    "ccc" 'org-clock-cancel
    "ccg" 'org-clock-goto
    "ccd" 'org-clock-display
    "cce" 'org-clock-modify-effort-estimate
    
    ;; Babel
    "bb"  '(:ignore t :which-key "babel")
    "bbp" 'org-babel-previous-src-block
    "bbn" 'org-babel-next-src-block
    "bbe" 'org-babel-execute-src-block
    "bbo" 'org-babel-open-src-block-result
    "bbv" 'org-babel-expand-src-block
    "bbu" 'org-babel-goto-src-block-head
    "bbg" 'org-babel-goto-named-src-block
    "bbr" 'org-babel-goto-named-result
    "bbb" 'org-babel-execute-buffer
    "bbs" 'org-babel-execute-subtree
    "bbd" 'org-babel-demarcate-block
    "bbt" 'org-babel-tangle
    "bbf" 'org-babel-tangle-file
    "bbc" 'org-babel-check-src-block
    "bbj" 'org-babel-insert-header-arg
    "bbl" 'org-babel-load-in-session
    "bbi" 'org-babel-lob-ingest
    "bbI" 'org-babel-view-src-block-info
    "bbz" 'org-babel-switch-to-session
    "bba" 'org-babel-sha1-hash
    "bbx" 'org-babel-do-key-sequence-in-edit-buffer
    "bb." 'org-babel-transient-state/body))

;; =============================================================================
;; HYDRAS FOR COMPLEX OPERATIONS
;; =============================================================================

(use-package hydra
  :ensure t
  :after org
  :config
  (defhydra hydra-org-clock (:color blue :hint nil)
    "
Clock   ^In/Out^     ^Edit^   ^Summary^    | ^Timers^           ^Other^
        _i_n         _e_dit   _g_oto entry | _z_: Start timer   _j_: Jump to current
        _c_ontinue   _q_uit   _d_isplay    | _Z_: Pause timer   _r_: Report
        _o_ut        ^ ^      _r_eport     | _E_: Set effort    _R_: Clocktable
    "
    ("i" org-clock-in)
    ("o" org-clock-out)
    ("c" org-clock-in-last)
    ("e" org-clock-modify-effort-estimate)
    ("q" org-clock-cancel)
    ("g" org-clock-goto)
    ("d" org-clock-display)
    ("r" org-clock-report)
    ("R" org-clocktable-try-shift)
    ("z" org-timer-start)
    ("Z" org-timer-pause-or-continue)
    ("E" org-set-effort)
    ("j" (org-clock-goto t))
    ("q" nil :color blue))
  
  (defhydra hydra-org-template (:color blue :hint nil)
    "
 Templates: _s_rc  _e_xample  _q_uote  _v_erse  _c_enter  _l_aTeX  _h_tml  _a_scii
"
    ("s" (org-insert-structure-template "src"))
    ("e" (org-insert-structure-template "example"))
    ("q" (org-insert-structure-template "quote"))
    ("v" (org-insert-structure-template "verse"))
    ("c" (org-insert-structure-template "center"))
    ("l" (org-insert-structure-template "latex"))
    ("h" (org-insert-structure-template "html"))
    ("a" (org-insert-structure-template "ascii"))
    ("ESC" nil :color blue))
  
  ;; Add hydra keybindings
  (my/local-leader-keys
    :keymaps 'org-mode-map
    "C" 'hydra-org-clock/body
    "I" 'hydra-org-template/body))

;; =============================================================================
;; PERFORMANCE AND STARTUP OPTIMIZATIONS
;; =============================================================================

(use-package org
  :ensure nil
  :config
  ;; Optimize startup
  (setq org-agenda-inhibit-startup t
        org-agenda-dim-blocked-tasks nil
        org-startup-indented t
        org-startup-folded 'overview)
  
  ;; Optimize fontification
  (setq org-fontify-whole-heading-line nil
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)
  
  ;; Cache expensive operations
  (setq org-element-cache-persistent t
        org-element-use-cache t))

;; =============================================================================
;; AUTO-SAVE AND BACKUP CONFIGURATION
;; =============================================================================

(use-package org
  :ensure nil
  :config
  ;; Auto-save org files
  (add-hook 'org-mode-hook 'auto-save-mode)
  
  ;; Backup settings
  (setq backup-by-copying t
        backup-directory-alist `((".*" . ,(concat my/org-directory "backups/")))
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)
  
  ;; Create backup directory
  (unless (file-directory-p (concat my/org-directory "backups/"))
    (make-directory (concat my/org-directory "backups/") t))
  
  ;; Auto-save all org buffers
  (defun my/save-all-org-buffers ()
    "Save all org buffers without prompting."
    (interactive)
    (save-some-buffers t (lambda () (derived-mode-p 'org-mode)))
    (message "Saved all org buffers"))
  
  ;; Save org buffers when idle
  (run-with-idle-timer 300 t 'my/save-all-org-buffers))

;; =============================================================================
;; FINAL MESSAGE AND HOOKS
;; =============================================================================

(use-package org
  :ensure nil
  :config
  ;; Final setup hook
  (defun my/org-mode-setup ()
    "Final org-mode setup."
    (visual-line-mode 1)
    (org-indent-mode 1)
    (flyspell-mode 1)
    (auto-fill-mode 1)
    (setq fill-column 80))
  
  (add-hook 'org-mode-hook 'my/org-mode-setup)
  
  ;; Confirmation message
  (message "🎉 Enhanced Comprehensive Org-Mode configuration loaded successfully!")
  (message "Use SPC o for org commands, SPC n for roam, SPC i for custom functions"))

;; =============================================================================
;; END OF CONFIGURATION
;; =============================================================================

(provide 'enhanced-org-config)
;;; enhanced-org-config.el ends here
