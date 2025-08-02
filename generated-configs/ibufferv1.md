``````el
;; Enhanced IBuffer configuration with nerd-icons-ibuffer integration
;; Bufler-style interface with gruvbox theme, perspective, project, and treemacs support
;; Supports: Python, C/C++, CSS, SCSS, LaTeX, Text, and TeX modes

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

``````
