``````el
;; Emacs Dashboard Configuration with doom-dashboard and project.el
;; Requires: dashboard, doom-dashboard, nerd-icons

;; Base dashboard configuration
(use-package dashboard
  :ensure t
  :after nerd-icons
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  
  :custom
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-startup-banner 'logo)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-set-init-info t)
  (dashboard-buffer-name "*dashboard*")
  (dashboard-projects-backend 'project-el)
  
  :config
  (dashboard-setup-startup-hook))

;; Doom dashboard enhancement
(use-package doom-dashboard
  :ensure t
  :after dashboard
  :demand t
  
  :bind
  (:map dashboard-mode-map
   ("j" . widget-forward)
   ("k" . widget-backward)
   ("h" . widget-backward)
   ("l" . widget-forward)
   ("q" . quit-window)
   ("r" . dashboard-refresh-buffer)
   ("g" . dashboard-refresh-buffer)
   ("RET" . widget-button-press)
   ("TAB" . widget-forward)
   ("<backtab>" . widget-backward))
  
  :custom
  (dashboard-banner-logo-title "E M A C S")
  (dashboard-startup-banner 'logo)
  (dashboard-footer-icon
   (nerd-icons-faicon "nf-fa-github_alt" :face 'success :height 1.5))
  
  ;; Dashboard layout
  (dashboard-startupify-list 
   '(dashboard-insert-banner
     dashboard-insert-banner-title
     dashboard-insert-newline
     doom-dashboard-insert-quick-actions
     dashboard-insert-newline
     dashboard-insert-items
     dashboard-insert-newline
     dashboard-insert-init-info
     dashboard-insert-newline
     doom-dashboard-insert-homepage-footer))
  
  ;; Item generators with project.el support
  (dashboard-item-generators
   '((recents   . doom-dashboard-insert-recents-shortmenu)
     (bookmarks . doom-dashboard-insert-bookmark-shortmenu)
     (projects  . doom-dashboard-insert-project-el-shortmenu)
     (agenda    . doom-dashboard-insert-org-agenda-shortmenu)))
  
  (dashboard-items '((projects . 8)
                     (recents . 8)
                     (bookmarks . 6)
                     (agenda . 5)))
  
  :config
  ;; Project.el integration
  (defun doom-dashboard-insert-project-el-shortmenu (&rest _)
    "Insert projects section using project.el backend."
    (dashboard-insert-heading "Projects"
                              (nerd-icons-codicon "nf-cod-folder"))
    (insert "\n")
    (let ((projects (doom-dashboard-get-project-el-projects)))
      (if projects
          (progn
            (dolist (project projects)
              (doom-dashboard-insert-project-el-item project))
            (insert "\n"))
        (insert (propertize "  No projects found\n\n" 
                           'face 'dashboard-no-items-face)))))
  
  (defun doom-dashboard-get-project-el-projects ()
    "Get list of known projects using project.el."
    (when (fboundp 'project-known-project-roots)
      (let ((projects (project-known-project-roots)))
        (seq-take projects (cdr (assoc 'projects dashboard-items))))))
  
  (defun doom-dashboard-insert-project-el-item (project-root)
    "Insert a single project item for PROJECT-ROOT."
    (let* ((project-name (file-name-nondirectory (directory-file-name project-root)))
           (project-path (abbreviate-file-name project-root)))
      (widget-create 'push-button
                     :action `(lambda (&rest _)
                               (let ((default-directory ,project-root))
                                 (cond
                                  ((fboundp 'project-find-file)
                                   (project-find-file))
                                  (t (dired ,project-root)))))
                     :mouse-face 'highlight
                     :follow-link "\C-m"
                     :button-prefix "  "
                     :button-suffix ""
                     :format "%[%t%]"
                     (format "%s %s"
                             (nerd-icons-devicon "nf-dev-git" :face 'dashboard-item-project-face)
                             (propertize project-name 'face 'dashboard-item-project-face)))
      (insert (format " %s" (propertize project-path 'face 'dashboard-text-banner)))
      (insert "\n")))
  
  ;; Custom quick actions with project.el integration
  (defun doom-dashboard-insert-quick-actions ()
    "Insert quick action buttons in doom style."
    (dashboard-insert-heading "Quick Actions"
                              (nerd-icons-faicon "nf-fa-rocket"))
    (insert "\n")
    (doom-dashboard-insert-action-button
     "Open Config" (nerd-icons-codicon "nf-cod-settings_gear")
     (lambda () (find-file user-init-file)))
    (insert "  ")
    (doom-dashboard-insert-action-button
     "Find Project" (nerd-icons-codicon "nf-cod-folder_opened")
     (lambda () (cond
                 ((fboundp 'project-switch-project) (project-switch-project))
                 ((fboundp 'projectile-switch-project) (projectile-switch-project))
                 (t (call-interactively 'find-file)))))
    (insert "  ")
    (doom-dashboard-insert-action-button
     "Recent Files" (nerd-icons-codicon "nf-cod-history")
     (lambda () (cond
                 ((fboundp 'consult-recent-file) (consult-recent-file))
                 ((fboundp 'counsel-recentf) (counsel-recentf))
                 (t (recentf-open-files)))))
    (insert "\n")
    (doom-dashboard-insert-action-button
     "Packages" (nerd-icons-codicon "nf-cod-package")
     (lambda () (if (fboundp 'list-packages) (list-packages) (package-list-packages))))
    (insert "  ")
    (doom-dashboard-insert-action-button
     "Dired Home" (nerd-icons-codicon "nf-cod-home")
     (lambda () (dired "~")))
    (insert "  ")
    (doom-dashboard-insert-action-button
     "Scratch Buffer" (nerd-icons-codicon "nf-cod-edit")
     (lambda () (switch-to-buffer "*scratch*")))
    (insert "\n\n"))
  
  (defun doom-dashboard-insert-action-button (text icon action)
    "Insert an action button with ICON, TEXT, and ACTION."
    (widget-create 'push-button
                   :action (lambda (&rest _) (funcall action))
                   :mouse-face 'highlight
                   :follow-link "\C-m"
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%t%]"
                   (format "%s %s" icon text)))
  
  ;; Homepage footer
  (defun doom-dashboard-insert-homepage-footer ()
    "Insert the doom dashboard homepage footer."
    (let ((project-count (length (if (fboundp 'project-known-project-roots)
                                     (project-known-project-roots)
                                   '()))))
      (insert (propertize 
               (format "Loaded %d packages • %d projects • %s"
                       (length package-activated-list)
                       project-count
                       (emacs-init-time))
               'face 'dashboard-footer))
      (insert "\n")))
  
  ;; Footer messages
  (setq dashboard-footer-messages '("Happy Coding with Doom Dashboard!" 
                                    "Welcome to Enhanced Emacs!"
                                    "Project.el integration enabled!")))

;; Project.el auto-discovery
(with-eval-after-load 'project
  (when (fboundp 'project-remember-projects-under)
    (let ((common-dirs '("~/Projects" "~/Code" "~/Development" "~/Work")))
      (dolist (dir common-dirs)
        (when (file-directory-p dir)
          (project-remember-projects-under dir t))))))

;; Gruvbox theme face customizations
(defun setup-dashboard-gruvbox-faces ()
  "Setup Gruvbox dark theme faces for dashboard."
  (custom-set-faces
   '(dashboard-banner-logo-title 
     ((t (:foreground "#fbf1c7" :weight bold :height 1.4))))
   '(dashboard-heading 
     ((t (:foreground "#fabd2f" :weight bold :height 1.2))))
   '(dashboard-text-banner 
     ((t (:foreground "#ebdbb2" :weight normal))))
   '(dashboard-no-items-face 
     ((t (:foreground "#928374" :slant italic))))
   '(dashboard-items-face 
     ((t (:foreground "#ebdbb2"))))
   '(dashboard-footer 
     ((t (:foreground "#d3869b" :slant italic))))
   '(widget-button 
     ((t (:foreground "#83a598" :weight bold :underline nil))))
   '(widget-button-pressed 
     ((t (:foreground "#458588" :weight bold))))
   '(dashboard-item-file-face 
     ((t (:foreground "#ebdbb2"))))
   '(dashboard-item-project-face 
     ((t (:foreground "#b8bb26"))))
   '(dashboard-item-bookmark-face 
     ((t (:foreground "#83a598"))))
   '(dashboard-item-agenda-face 
     ((t (:foreground "#fe8019"))))))

(with-eval-after-load 'doom-dashboard
  (setup-dashboard-gruvbox-faces))

;; Dashboard mode setup
(defun setup-dashboard-mode ()
  "Setup dashboard mode with clean UI."
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (when (fboundp 'hl-line-mode)
    (hl-line-mode -1))
  (setq buffer-read-only t)
  (setq cursor-type nil)
  (setq-local mode-line-format nil))

(add-hook 'dashboard-mode-hook #'setup-dashboard-mode)

;; Optional: Custom ASCII banner
(defvar doom-dashboard-custom-banner
  "
    ███████╗███╗   ███╗ █████╗  ██████╗███████╗
    ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
    █████╗  ██╔████╔██║███████║██║     ███████╗
    ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
    ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
    ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝

            Enhanced with Doom Dashboard & Project.el
")

(defun doom-dashboard-custom-banner-function ()
  "Display custom ASCII banner."
  (insert (propertize doom-dashboard-custom-banner 
                      'face 'dashboard-banner-logo-title))
  (insert "\n"))

;; Uncomment to use custom banner instead of logo
(setq dashboard-startup-banner #'doom-dashboard-custom-banner-function)

``````
