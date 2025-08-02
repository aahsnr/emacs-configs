;; Core perspective configuration
(use-package perspective
  :init
  ;; Set custom variables before loading
  (setq persp-sort 'access
        persp-show-modestring t
        persp-modestring-short t
        persp-state-default-file (expand-file-name "perspective-session" user-emacs-directory)
        persp-interactive-completion-function #'completing-read
        persp-purge-initial-persp-on-save nil
        persp-frame-global-perspective-name "GLOBAL")
  
  :config
  ;; Enable perspective mode
  (persp-mode 1)
  
  ;; Auto-save perspective state on exit
  (add-hook 'kill-emacs-hook #'persp-state-save)
  
  ;; Display buffer configuration
  (setq display-buffer-base-action
        '((display-buffer-reuse-window display-buffer-same-window)
          (reusable-frames . t))
        even-window-sizes nil))

;; Core perspective helper functions
(defun my/persp-switch-to-buffer-dwim ()
  "Switch to buffer in current perspective, or all buffers with prefix."
  (interactive)
  (if current-prefix-arg
      (call-interactively #'switch-to-buffer)
    (call-interactively #'persp-switch-to-buffer*)))

(defun my/persp-kill-buffer-dwim ()
  "Kill buffer in current perspective, or any buffer with prefix."
  (interactive)
  (if current-prefix-arg
      (call-interactively #'kill-buffer)
    (call-interactively #'persp-kill-buffer*)))

(defun my/persp-kill-current ()
  "Kill current perspective with confirmation."
  (interactive)
  (let ((current-persp (persp-current-name)))
    (when (and current-persp
               (not (string= current-persp persp-initial-frame-name))
               (yes-or-no-p (format "Kill perspective '%s'? " current-persp)))
      (persp-kill current-persp))))

(defun my/persp-list-perspectives ()
  "List all perspectives with enhanced formatting."
  (interactive)
  (let ((perspectives (persp-names))
        (current (persp-current-name)))
    (if perspectives
        (message "Perspectives: %s" 
                 (mapconcat (lambda (p)
                             (if (equal p current)
                                 (propertize p 'face 'bold)
                               p))
                           perspectives 
                           " | "))
      (message "No perspectives available"))))

(defun my/persp-switch-by-number (num)
  "Switch to perspective by number."
  (interactive "nPerspective number: ")
  (let ((perspectives (persp-names)))
    (if (and perspectives (>= num 1) (<= num (length perspectives)))
        (persp-switch (nth (1- num) perspectives))
      (message "Invalid perspective number: %d" num))))

(defun my/persp-reset-layout ()
  "Reset current perspective to single window."
  (interactive)
  (delete-other-windows)
  (let ((scratch-buffer (format "*scratch-%s*" (persp-current-name))))
    (switch-to-buffer (get-buffer-create scratch-buffer))))

(defun my/persp-save-session-as ()
  "Save perspective session to specified file."
  (interactive)
  (let ((file (read-file-name "Save perspective session to: "
                             user-emacs-directory
                             "perspective-session")))
    (persp-state-save file)))

(defun my/persp-load-session ()
  "Load perspective session from file."
  (interactive)
  (let ((file (read-file-name "Load perspective session from: "
                             user-emacs-directory
                             "perspective-session"
                             t)))
    (when (file-exists-p file)
      (persp-state-load file))))

(defun my/persp-cleanup-empty ()
  "Clean up empty perspectives (except main/initial)."
  (interactive)
  (dolist (persp-name (persp-names))
    (when (and (not (string= persp-name persp-initial-frame-name))
               (not (string= persp-name "main"))
               (null (persp-buffers persp-name)))
      (persp-kill persp-name)))
  (message "Cleaned up empty perspectives"))

(defun my/setup-initial-perspectives ()
  "Set up initial perspectives for common workflows."
  (interactive)
  (let ((original-persp (persp-current-name)))
    (persp-switch "config")
    (persp-switch "notes")
    (persp-switch "coding")
    (persp-switch (or original-persp "main"))))

(defun my/perspective-save-and-kill-emacs ()
  "Save perspective state and kill Emacs."
  (interactive)
  (persp-state-save)
  (save-buffers-kill-emacs))

;; Basic keybindings (independent of other packages)
(global-set-key (kbd "C-x C-c") #'my/perspective-save-and-kill-emacs)
(global-set-key (kbd "C-c p s") #'persp-switch)
(global-set-key (kbd "C-c p k") #'my/persp-kill-current)
(global-set-key (kbd "C-c p n") #'persp-next)
(global-set-key (kbd "C-c p p") #'persp-prev)
(global-set-key (kbd "C-c p r") #'persp-rename)
(global-set-key (kbd "C-c p l") #'my/persp-list-perspectives)
(global-set-key (kbd "C-c p b") #'my/persp-switch-to-buffer-dwim)

;; Optional integrations (only loaded if packages are available)

;; Project integration
(defun my/setup-project-integration ()
  "Setup project integration if project.el is available."
  (when (featurep 'project)
    (defun my/persp-switch-to-project ()
      "Switch to project perspective or create one."
      (interactive)
      (if-let ((project (project-current)))
          (let* ((project-root (project-root project))
                 (project-name (file-name-nondirectory) 
                               (directory-file-name project-root)))
            (persp-switch project-name))
        (message "Not in a project")))
    
    (defun my/auto-create-project-perspective ()
      "Automatically create perspective for current project."
      (when-let ((project (project-current)))
        (let* ((project-root (project-root project))
               (project-name (file-name-nondirectory) 
                             (directory-file-name project-root)))
          (unless (member project-name (persp-names))
            (persp-switch project-name)))))
    
    (add-hook 'project-find-file-hook #'my/auto-create-project-perspective)
    (global-set-key (kbd "C-c p P") #'my/persp-switch-to-project)))

;; Ibuffer integration (safer implementation)
(defun my/setup-ibuffer-integration ()
  "Setup ibuffer integration if available."
  (when (featurep 'ibuffer)
    (defun my/persp-ibuffer-dwim ()
      "Open ibuffer filtered by perspective, or all buffers with prefix."
      (interactive)
      (if current-prefix-arg
          (ibuffer t)
        (persp-ibuffer)))
    
    (defun my/persp-ibuffer-hook ()
      "Hook function for ibuffer perspective integration."
      (condition-case err
          (when (fboundp 'persp-ibuffer-set-filter-groups)
            (persp-ibuffer-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic)))
        (error (message "Error in persp-ibuffer-hook: %s" err))))
    
    (add-hook 'ibuffer-hook #'my/persp-ibuffer-hook)
    (global-set-key (kbd "C-c p i") #'my/persp-ibuffer-dwim)))

;; Load integrations after initialization
(add-hook 'after-init-hook
          (lambda ()
            (my/setup-project-integration)
            (my/setup-ibuffer-integration)))

;; Optional: General.el configuration (only if general is available)
(with-eval-after-load 'general
  (when (fboundp 'general-create-definer)
    (general-create-definer space-leader-def
      :prefix "SPC")
    
    (space-leader-def
      :states '(normal visual motion)
      :keymaps 'override
      "l"   '(:ignore t :which-key "layouts")
      "ls"  '(persp-switch :which-key "switch perspective")
      "lc"  '(persp-switch :which-key "create perspective") 
      "lk"  '(my/persp-kill-current :which-key "kill perspective")
      "lr"  '(persp-rename :which-key "rename perspective")
      "ll"  '(my/persp-list-perspectives :which-key "list perspectives")
      "ln"  '(persp-next :which-key "next perspective")
      "lp"  '(persp-prev :which-key "previous perspective")
      "l1"  '((lambda () (interactive) (my/persp-switch-by-number 1)) :which-key "perspective 1")
      "l2"  '((lambda () (interactive) (my/persp-switch-by-number 2)) :which-key "perspective 2")
      "l3"  '((lambda () (interactive) (my/persp-switch-by-number 3)) :which-key "perspective 3")
      "l4"  '((lambda () (interactive) (my/persp-switch-by-number 4)) :which-key "perspective 4")
      "l5"  '((lambda () (interactive) (my/persp-switch-by-number 5)) :which-key "perspective 5")
      "lR"  '(my/persp-reset-layout :which-key "reset layout")
      "bb"  '(my/persp-switch-to-buffer-dwim :which-key "switch buffer")
      "bk"  '(my/persp-kill-buffer-dwim :which-key "kill buffer"))))

;; Optional: Consult integration
(with-eval-after-load 'consult
  (when (boundp 'persp-consult-source)
    (add-to-list 'consult-buffer-sources persp-consult-source)))

;; Optional: Which-key integration
(with-eval-after-load 'which-key
  (when (fboundp 'which-key-add-key-based-replacements)
    (which-key-add-key-based-replacements
      "C-c p" "perspective"
      "SPC l" "layouts")))

;; Optional: Treemacs integration (simplified)
(with-eval-after-load 'treemacs
  (defun my/persp-treemacs-sync ()
    "Sync treemacs workspace with current perspective."
    (condition-case err
        (when (and (bound-and-true-p treemacs--ready)
                   (not (string= (persp-current-name) persp-initial-frame-name)))
          (let ((workspace-name (format "persp-%s" (persp-current-name))))
            (unless (treemacs-workspace->is-name-taken? workspace-name)
              (treemacs--create-workspace workspace-name)
              (treemacs-switch-workspace workspace-name))))
      (error (message "Error in treemacs sync: %s" err))))
  
  (add-hook 'persp-switch-hook #'my/persp-treemacs-sync))

;; Provide the configuration
(provide 'perspective-config)

;;; perspective-config.el ends here
