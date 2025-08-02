``````el
;;; Comprehensive Org-Mode Configuration
;;; A complete org-mode setup with modern features and integrations

;; =============================================================================
;; PACKAGE SETUP
;; =============================================================================

(use-package org
  :ensure nil
  :pin manual
  :defer t)

(use-package org-modern
  :ensure t
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (org-super-agenda-mode))

(use-package org-fancy-priorities
  :ensure t
  :after org
  :hook (org-mode . org-fancy-priorities-mode))

(use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-v2-ack t))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-download
  :ensure t
  :after org
  :hook (dired-mode . org-download-enable))

(use-package org-fragtog
  :ensure t
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package org-noter
  :ensure t
  :after org
  :config
  (setq org-noter-notes-search-path '("~/org/noter/")))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;; =============================================================================
;; DIRECTORY SETUP
;; =============================================================================

(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "inbox.org"))
(setq org-agenda-files (list org-directory))

;; Create necessary directories
(unless (file-directory-p org-directory)
  (make-directory org-directory t))

(unless (file-directory-p (concat org-directory "roam/"))
  (make-directory (concat org-directory "roam/") t))

(unless (file-directory-p (concat org-directory "noter/"))
  (make-directory (concat org-directory "noter/") t))

(unless (file-directory-p (concat org-directory "downloads/"))
  (make-directory (concat org-directory "downloads/") t))

;; =============================================================================
;; FONT AND VISUAL CONFIGURATION
;; =============================================================================

;; JetBrains Mono Nerd Font configuration for headlines
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

;; Apply font configuration
(add-hook 'org-mode-hook #'my/org-font-setup)

;; Gruvbox dark theme integration
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
  
  ;; Additional gruvbox colors
  (set-face-attribute 'org-todo nil :foreground "#fb4934" :weight 'bold)
  (set-face-attribute 'org-done nil :foreground "#b8bb26" :weight 'bold)
  (set-face-attribute 'org-priority nil :foreground "#fabd2f")
  (set-face-attribute 'org-tag nil :foreground "#8ec07c")
  (set-face-attribute 'org-link nil :foreground "#83a598" :underline t)
  (set-face-attribute 'org-date nil :foreground "#d3869b")
  (set-face-attribute 'org-special-keyword nil :foreground "#928374"))

(add-hook 'org-mode-hook #'my/org-gruvbox-colors)

;; =============================================================================
;; ORG-MODERN CONFIGURATION
;; =============================================================================

(with-eval-after-load 'org-modern
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))
        org-modern-tag nil
        org-modern-priority nil
        org-modern-todo nil
        org-modern-checkbox '((88 . "☑") (45 . "☐") (32 . "☐"))
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-block nil
        org-modern-keyword nil
        org-modern-timestamp t
        org-modern-statistics t
        org-modern-progress nil))

;; Custom ellipsis that works well with org-modern stars
(setq org-ellipsis " ⤵")

;; =============================================================================
;; ORG BASIC CONFIGURATION
;; =============================================================================

(with-eval-after-load 'org
  (setq org-startup-folded 'overview
        org-startup-indented t
        org-pretty-entities t
        org-use-property-inheritance t
        org-log-done 'time
        org-list-allow-alphabetical t
        org-export-in-background t
        org-catch-invisible-edits 'smart
        org-cycle-separator-lines 2
        org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function nil
        org-export-with-smart-quotes t
        org-export-with-sub-superscripts nil))

;; Start all org files in overview mode
(setq org-startup-folded 'overview)

;; =============================================================================
;; PRETTIFY SYMBOLS AND SOURCE BLOCKS
;; =============================================================================

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
  (prettify-symbols-mode 1))

(add-hook 'org-mode-hook #'my/org-prettify-symbols)

;; Enhanced source block configuration
(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-edit-src-content-indentation 0
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

;; =============================================================================
;; ORG TODO CONFIGURATION WITH UNICODE
;; =============================================================================

(setq org-todo-keywords
      '((sequence "☛ TODO(t)" "⚡ NEXT(n)" "🔄 PROG(p)" "⏳ WAIT(w@/!)" "|" "✅ DONE(d!)" "❌ CANCELLED(c@)")
        (sequence "📋 PLAN(P)" "🔍 RESEARCH(R)" "📝 DRAFT(D)" "|" "📤 PUBLISHED(p)" "🗑 TRASH(T)")))

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
        ("🗑 TRASH" . (:foreground "#928374" :weight bold))))

;; =============================================================================
;; ORG FANCY PRIORITIES CONFIGURATION
;; =============================================================================

(with-eval-after-load 'org-fancy-priorities
  (setq org-fancy-priorities-list '("🔥" "⚡" "🔴" "🟡")
        org-priority-faces '((?A . (:foreground "#fb4934" :weight bold))
                            (?B . (:foreground "#fabd2f" :weight bold))
                            (?C . (:foreground "#83a598" :weight bold)))))

;; =============================================================================
;; ORG AGENDA CONFIGURATION
;; =============================================================================

(setq org-agenda-files (list org-directory)
      org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t
      org-agenda-span 'week
      org-agenda-start-on-weekday nil
      org-agenda-start-day "today"
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator ?─
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t)

;; Custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "⚡ NEXT"
                ((org-agenda-overriding-header "Next Tasks")))
          (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))
          (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "High Priority")))
          (tags-todo "+TODO=\"WAIT\"" ((org-agenda-overriding-header "Waiting On")))))
        ("n" "Next Tasks"
         ((todo "⚡ NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))
        ("w" "Work Tasks" tags-todo "+work")
        ("h" "Home Tasks" tags-todo "+home")
        ("p" "Projects" tags "project")))

;; =============================================================================
;; ORG SUPER AGENDA CONFIGURATION
;; =============================================================================

(with-eval-after-load 'org-super-agenda
  (setq org-super-agenda-groups
        '((:name "🔥 Overdue" :deadline past)
          (:name "📅 Today" :time-grid t :scheduled today)
          (:name "⚡ Next" :todo "⚡ NEXT")
          (:name "🔴 Important" :priority "A")
          (:name "📋 Projects" :tag "project")
          (:name "🏠 Home" :tag "home")
          (:name "💼 Work" :tag "work")
          (:name "⏳ Waiting" :todo "⏳ WAIT")
          (:name "📚 Reading" :tag "read")
          (:name "🎯 Goals" :tag "goal")
          (:discard (:anything t)))))

;; =============================================================================
;; TEXT FORMATTING ENHANCEMENTS
;; =============================================================================

;; Enhanced emphasis markers
(setq org-emphasis-alist
      '(("*" (bold :foreground "#fb4934"))
        ("/" (italic :foreground "#83a598"))
        ("_" (:underline t :foreground "#8ec07c"))
        ("=" (:background "#3c3836" :foreground "#fbf1c7"))
        ("~" (:background "#3c3836" :foreground "#fe8019"))
        ("+" (:strike-through t :foreground "#928374"))))

(setq org-hide-emphasis-markers t)

;; =============================================================================
;; ORG DOWNLOAD CONFIGURATION
;; =============================================================================

(with-eval-after-load 'org-download
  (setq org-download-method 'directory
        org-download-image-dir (concat org-directory "downloads/")
        org-download-heading-lvl nil
        org-download-timestamp "%Y%m%d-%H%M%S_"
        org-download-screenshot-method "flameshot gui --raw > %s"
        org-download-annotate-function (lambda (_link) "#+ATTR_ORG: :width 300\n")))

;; =============================================================================
;; ORG ROAM V2 CONFIGURATION
;; =============================================================================

(setq org-roam-directory (concat org-directory "roam/")
      org-roam-db-location (concat org-directory "org-roam.db")
      org-roam-dailies-directory "daily/"
      org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: ${title}\n#+date: %U\n#+filetags: \n\n")
         :unnarrowed t)
        ("n" "note" plain "* ${title}\n%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: ${title}\n#+date: %U\n#+filetags: note\n\n")
         :unnarrowed t)
        ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Resources\n\n"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: ${title}\n#+date: %U\n#+filetags: project\n\n")
         :unnarrowed t)
        ("b" "book notes" plain "* Summary\n\n%?\n\n* Key Takeaways\n\n* Quotes\n\n* Related Notes\n\n"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: ${title}\n#+date: %U\n#+filetags: book literature\n\n")
         :unnarrowed t)))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                           "#+title: %<%Y-%m-%d>\n#+date: %U\n#+filetags: daily\n\n"))))

;; Brain-like features
(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:10}" 'face 'org-tag)))

;; Doom Emacs-like keybindings
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n c") 'org-roam-capture)
(global-set-key (kbd "C-c n d") 'org-roam-dailies-capture-today)
(global-set-key (kbd "C-c n t") 'org-roam-dailies-goto-today)
(global-set-key (kbd "C-c n y") 'org-roam-dailies-goto-yesterday)
(global-set-key (kbd "C-c n r") 'org-roam-node-random)
(global-set-key (kbd "C-c n s") 'org-roam-db-sync)
(global-set-key (kbd "C-c n g") 'org-roam-graph)
(global-set-key (kbd "C-c n u") 'org-roam-ui-open)

;; Initialize org-roam
(org-roam-db-autosync-mode)

;; =============================================================================
;; ORG NOTER CONFIGURATION
;; =============================================================================

(with-eval-after-load 'org-noter
  (setq org-noter-notes-search-path (list (concat org-directory "noter/"))
        org-noter-separate-notes-from-heading t
        org-noter-always-create-document-property t
        org-noter-insert-note-no-questions t
        org-noter-use-indirect-buffer t
        org-noter-kill-frame-at-session-end nil
        org-noter-auto-save-last-location t
        org-noter-default-heading-title "Notes for page $p$"))

;; Integration with org-roam
(defun my/org-noter-init-roam ()
  "Initialize org-noter with org-roam integration."
  (when (and (buffer-file-name)
             (string-match-p org-roam-directory (buffer-file-name)))
    (org-noter)))

;; =============================================================================
;; CAPTURE TEMPLATES
;; =============================================================================

(setq org-capture-templates
      '(("t" "Task" entry (file+headline "~/org/inbox.org" "Tasks")
         "* ☛ TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")
        ("n" "Note" entry (file+headline "~/org/inbox.org" "Notes")
         "* %? :note:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %U %?\n")
        ("m" "Meeting" entry (file+headline "~/org/inbox.org" "Meetings")
         "* Meeting: %? :meeting:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n** Agenda\n** Notes\n** Action Items\n")
        ("p" "Project" entry (file+headline "~/org/projects.org" "Projects")
         "* 📋 PLAN %? :project:\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n** Goals\n** Tasks\n** Resources\n")
        ("b" "Book" entry (file+headline "~/org/reading.org" "Reading List")
         "* %? :book:\n  :PROPERTIES:\n  :CREATED: %U\n  :AUTHOR: \n  :GENRE: \n  :PAGES: \n  :END:\n")))

;; =============================================================================
;; KEYBINDINGS
;; =============================================================================

;; Global org-mode keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)

;; Org-mode specific keybindings
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-j") 'org-goto)
  (define-key org-mode-map (kbd "C-c C-q") 'counsel-org-tag)
  (define-key org-mode-map (kbd "C-c s") 'org-schedule)
  (define-key org-mode-map (kbd "C-c d") 'org-deadline))

;; =============================================================================
;; ADDITIONAL QUALITY OF LIFE IMPROVEMENTS
;; =============================================================================

;; Auto-save org buffers
(add-hook 'org-mode-hook 'auto-save-mode)

;; Show line numbers in org-mode
(add-hook 'org-mode-hook 'display-line-numbers-mode)

;; Wrap lines in org-mode
(add-hook 'org-mode-hook 'visual-line-mode)

;; Enable flyspell for spell checking
(add-hook 'org-mode-hook 'flyspell-mode)

;; Custom functions for enhanced workflow
(defun my/org-insert-screenshot ()
  "Take a screenshot and insert it into the current org file."
  (interactive)
  (org-download-screenshot))

(defun my/org-archive-done-tasks ()
  "Archive all DONE tasks in the current buffer."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(defun my/org-clock-in-last-task ()
  "Clock in the last task."
  (interactive)
  (org-clock-in-last))

;; Additional keybindings for custom functions
(global-set-key (kbd "C-c i s") 'my/org-insert-screenshot)
(global-set-key (kbd "C-c i a") 'my/org-archive-done-tasks)
(global-set-key (kbd "C-c i c") 'my/org-clock-in-last-task)

;; =============================================================================
;; BABEL CONFIGURATION
;; =============================================================================

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (sql . t)
   (sqlite . t)
   (js . t)
   (css . t)
   (plantuml . t)))

;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)

;; =============================================================================
;; EXPORT CONFIGURATION
;; =============================================================================

(setq org-export-with-smart-quotes t
      org-export-with-sub-superscripts nil
      org-export-with-toc nil
      org-export-headline-levels 4)

;; HTML export settings
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil)

;; =============================================================================
;; PERFORMANCE OPTIMIZATIONS
;; =============================================================================

;; Improve org-mode performance
(setq org-agenda-dim-blocked-tasks nil
      org-agenda-inhibit-startup t
      org-agenda-use-tag-inheritance nil
      org-agenda-ignore-properties '(effort appt category))

;; =============================================================================
;; FINAL SETUP
;; =============================================================================

;; Create initial org files if they don't exist
(defun my/create-initial-org-files ()
  "Create initial org files if they don't exist."
  (let ((files '("inbox.org" "projects.org" "journal.org" "reading.org")))
    (dolist (file files)
      (let ((full-path (concat org-directory file)))
        (unless (file-exists-p full-path)
          (with-temp-file full-path
            (insert (format "#+TITLE: %s\n#+DATE: %s\n\n"
                           (capitalize (file-name-sans-extension file))
                           (format-time-string "%Y-%m-%d")))))))))

;; Run initial setup
(my/create-initial-org-files)

;; Message to confirm configuration is loaded
(message "Comprehensive Org-Mode configuration loaded successfully!")

;; =============================================================================
;; END OF CONFIGURATION
;; =============================================================================

``````
