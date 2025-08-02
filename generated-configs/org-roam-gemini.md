``````el
;; Enhanced Org-Roam v2 Configuration with Advanced Packages
;; Integrated with org-roam-ql, org-roam-ql-ql, embark-org-roam, and org-roam-timestamps
;; Gruvbox Dark Theme Compatible
;; Keybindings managed by general.el with SPC leader

;;; === PACKAGE MANAGEMENT ===
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; === GENERAL.EL KEYBINDING SETUP ===
(use-package general
  :ensure t
  :config
  (general-create-definer my/leader-keys
    :keymaps 'override
    :prefix "SPC")
  (general-create-definer my/org-roam-leader-keys
    :keymaps 'override
    :prefix "SPC n"))

;;; === DIRECTORY SETUP ===
(defvar my/org-directory (expand-file-name "~/org/")
  "Main org directory path.")

(defvar my/org-roam-directory (expand-file-name "roam/" my/org-directory)
  "Org-roam directory path.")

;; Ensure directories exist
(unless (file-directory-p my/org-directory)
  (make-directory my/org-directory t))

(unless (file-directory-p my/org-roam-directory)
  (make-directory my/org-roam-directory t))

;;; === CORE ORG-ROAM CONFIGURATION ===
(use-package org-roam
  :ensure t
  :after org
  :demand t
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

;;; === ORG-ROAM-TIMESTAMPS INTEGRATION ===
(use-package org-roam-timestamps
  :ensure t
  :after org-roam
  :config
  (org-roam-timestamps-mode 1)
  :custom
  (org-roam-timestamps-parent-file t)
  (org-roam-timestamps-remember-timestamps t)
  (org-roam-timestamps-minimum-gap 3600)) ; 1 hour minimum gap

;;; === ORG-ROAM-QL INTEGRATION ===
(use-package org-roam-ql
  :ensure t
  :after org-roam
  :config
  ;; Custom QL predicates for enhanced searching
  (setq org-roam-ql-default-org-roam-buffer-display-function
        #'org-roam-buffer-display-dedicated))

;;; === ORG-ROAM-QL-QL INTEGRATION ===
(use-package org-roam-ql-ql
  :ensure t
  :after (org-roam org-roam-ql)
  :config
  ;; Enhanced QL query interface
  (setq org-roam-ql-ql-default-completions
        '("tags" "title" "content" "file" "links" "backlinks" "refs")))

;;; === EMBARK-ORG-ROAM INTEGRATION ===
(use-package embark-org-roam
  :ensure t
  :after (embark org-roam)
  :config
  ;; Add org-roam actions to embark
  (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-node-map))
  (add-to-list 'embark-keymap-alist '(org-roam-ref . embark-org-roam-ref-map))

  ;; Custom embark actions
  (defun my/embark-org-roam-node-open-with (node)
    "Open org-roam NODE with external application."
    (find-file-other-window (org-roam-node-file node)))

  (defun my/embark-org-roam-node-copy-id (node)
    "Copy org-roam NODE id to kill ring."
    (kill-new (org-roam-node-id node))
    (message "Copied node ID: %s" (org-roam-node-id node)))

  ;; Add custom actions to embark map
  (define-key embark-org-roam-node-map (kbd "w") #'my/embark-org-roam-node-open-with)
  (define-key embark-org-roam-node-map (kbd "y") #'my/embark-org-roam-node-copy-id))

;;; === ORG-ROAM UI ===
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil)
  (org-roam-ui-port 35901))

;;; === CONSULT-ORG-ROAM INTEGRATION ===
(use-package consult-org-roam
  :ensure t
  :after (consult org-roam)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r)
  (consult-org-roam-buffer-after-buffers t)
  :config
  (when (fboundp 'consult-org-roam-mode)
    (consult-org-roam-mode 1)))

;;; === UTILITY FUNCTIONS ===
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
  (org-roam-capture- :node (org-roam-node-create)
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

;;; === ADVANCED QUERY FUNCTIONS ===
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

;;; === TIMESTAMP UPDATE HOOKS ===
(defun my/org-roam-update-modified-timestamp ()
  "Update modified timestamp in org-roam files."
  (when (org-roam-file-p)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+modified:" nil t)
        (delete-region (point) (line-end-position))
        (insert (format " %s" (format-time-string "[%Y-%m-%d %a %H:%M]")))))))

(add-hook 'before-save-hook #'my/org-roam-update-modified-timestamp)

;;; === BUFFER DISPLAY CONFIGURATION ===
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

;;; === GRUVBOX THEME INTEGRATION ===
(with-eval-after-load 'org-roam
  (set-face-attribute 'org-roam-link nil
                      :foreground "#83a598"  ; Gruvbox bright blue
                      :weight 'normal)
  (set-face-attribute 'org-roam-link-current nil
                      :foreground "#fabd2f"  ; Gruvbox bright yellow
                      :weight 'bold)
  (when (facep 'org-roam-link-invalid)
    (set-face-attribute 'org-roam-link-invalid nil
                        :foreground "#fb4934"  ; Gruvbox bright red
                        :strike-through t))
  (when (facep 'org-roam-tag)
    (set-face-attribute 'org-roam-tag nil
                        :foreground "#b8bb26"  ; Gruvbox bright green
                        :weight 'normal
                        :height 0.9))
  (when (facep 'org-roam-title)
    (set-face-attribute 'org-roam-title nil
                        :foreground "#fe8019"  ; Gruvbox bright orange
                        :weight 'bold)))

;;; === ROAM BUFFER SECTIONS ===
(with-eval-after-load 'org-roam
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-section
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section)))

;;; === COMPLETION CONFIGURATION ===
(add-hook 'org-roam-find-file-hook
          (lambda ()
            (when (org-roam-file-p)
              (setq-local completion-at-point-functions
                          (append completion-at-point-functions
                                  (list #'org-roam-completion-at-point))))))

;;; === PERFORMANCE OPTIMIZATIONS ===
(setq org-roam-db-gc-threshold most-positive-fixnum
      org-roam-db-update-on-save t)

;; Optimize QL searches
(setq org-roam-ql-default-org-roam-buffer-display-function
      #'org-roam-buffer-display-dedicated)

;;; === MAINTENANCE TASKS ===
;; Weekly maintenance
(run-with-timer 0 (* 7 24 60 60) #'my/org-roam-maintenance)

;; Database cleanup on exit
(add-hook 'kill-emacs-hook #'org-roam-db-sync)

;;; === HELPER FUNCTIONS ===
(defun my/org-roam-format-time-string ()
  "Format current time for org-roam templates."
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defun my/org-roam-node-from-cite (keys-list)
  "Create org-roam node from citation KEYS-LIST."
  (dolist (key keys-list)
    (org-roam-capture- :node (org-roam-node-create :title key)
                       :templates '(("r" "reference" plain "* %?"
                                     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                                        "#+title: ${title}\n#+date: %U\n#+created: %U\n#+modified: %U\n#+filetags: reference\n\n")
                                     :unnarrowed t)))))

(defun my/org-roam-stats ()
  "Display org-roam database statistics."
  (interactive)
  (let* ((nodes (org-roam-node-list))
         (files (length (org-roam-list-files)))
         (tags (length (org-roam-tag-list)))
         (aliases (length (org-roam-alias-list)))
         (refs (length (org-roam-ref-list))))
    (message "Org-roam Stats: %d nodes, %d files, %d tags, %d aliases, %d refs"
             (length nodes) files tags aliases refs)))


;;; === SPACEMACS KEYBINDINGS (GENERAL.EL) ===
(my/org-roam-leader-keys
  ;; Core Node Actions
  "f"  '(:ignore t :which-key "find")
  "ff" 'org-roam-node-find
  "fr" 'org-roam-node-random
  "i"  'org-roam-node-insert
  "I"  'my/org-roam-node-insert-immediate
  "c"  'org-roam-capture
  "x"  'org-roam-extract-subtree
  "X"  'my/org-roam-capture-inbox
  "Z"  'my/org-roam-find-inbox

  ;; Navigation and UI
  "g"  'org-roam-graph
  "l"  'org-roam-buffer-toggle
  "u"  'org-roam-ui-open

  ;; Tagging and Aliases
  "a"  'org-roam-alias-add
  "A"  'org-roam-alias-remove
  "t"  '(:ignore t :which-key "tags")
  "ta" 'org-roam-tag-add
  "tr" 'org-roam-tag-remove
  "tn" 'my/org-roam-tag-new

  ;; References and IDs
  "b"  'org-roam-ref-remove
  "B"  'org-roam-ref-add
  "o"  'org-id-get-create

  ;; Refiling
  "R" 'org-roam-refile

  ;; Maintenance
  "s" 'org-roam-db-sync
  "!" 'my/org-roam-maintenance

  ;; Find by Tag
  "p" 'my/org-roam-find-project
  "k" 'my/org-roam-find-book
  "C" 'my/org-roam-find-concept
  "M" 'my/org-roam-find-meeting
  "j" 'my/org-roam-find-idea

  ;; Consult Integration
  "e" 'consult-org-roam-file-find
  "G" 'consult-org-roam-grep
  "S" 'consult-org-roam-search

  ;; QL Searches
  "q"  '(:ignore t :which-key "query")
  "qq" 'org-roam-ql-search
  "qB" 'org-roam-ql-buffer-search
  "q;" 'org-roam-ql-ql-search
  "q:" 'org-roam-ql-ql-buffer-search
  "qr" 'my/org-roam-ql-search-recent
  "qo" 'my/org-roam-ql-search-orphans
  "qh" 'my/org-roam-ql-search-highly-connected
  "qt" 'my/org-roam-ql-search-todos)

;;; === FINAL CONFIGURATION MESSAGE ===
(message "Enhanced Org-roam v2 configuration with advanced packages loaded successfully!")
(message "Keybindings are set with SPC as the leader key via general.el.")
(message "Use SPC n ? for help, SPC n ! for maintenance, and SPC n q for QL searches.")

``````
