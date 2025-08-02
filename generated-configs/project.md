``````el
;; Streamlined project.el configuration for Emacs 30
;; Integrates with ibuffer, perspective.el, and treemacs
;; Requires general.el for keybindings

(use-package project
  :ensure nil ;; Built into Emacs 30
  :init
  ;; Project discovery settings
  (setq project-find-functions
        '(project-try-vc
          project-try-local))
  
  ;; Project file listing optimizations
  (setq project-vc-ignores '("target/" "build/" "dist/" ".git/"))
  (setq project-vc-extra-root-markers '(".projectile" ".project"))
  
  ;; Buffer switching behavior
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-dired "Dired")
          (project-vc-dir "VC-Dir")
          (project-shell "Shell")
          (project-compile "Compile")
          (project-switch-to-buffer "Switch buffer")
          (project-kill-buffers "Kill project buffers")))
  
  ;; Kill buffer behavior
  (setq project-kill-buffer-conditions
        '(buffer-file-name
          (major-mode . compilation-mode)
          (major-mode . shell-mode)
          (major-mode . eshell-mode)
          (major-mode . vterm-mode)
          (derived-mode . special-mode)))
  
  ;; Performance optimizations
  (setq project-list-remote-repositories nil)
  (setq project-vc-merge-submodules nil)
  
  :config
  ;; Enhanced project switching with perspective integration
  (defun my/project-switch-with-perspective (project-root)
    "Switch to project and create/switch to corresponding perspective."
    (interactive (list (project-prompt-project-dir)))
    (let* ((project-name (file-name-nondirectory (directory-file-name project-root)))
           (perspective-name (format "Project: %s" project-name)))
      ;; Switch to or create perspective
      (when (fboundp 'persp-switch)
        (persp-switch perspective-name))
      ;; Switch to project
      (project-switch-project project-root)
      ;; Update treemacs if available
      (when (and (fboundp 'treemacs-add-and-display-current-project-exclusively)
                 (project-current))
        (treemacs-add-and-display-current-project-exclusively))))
  
  ;; Project buffer management with ibuffer integration
  (defun my/project-ibuffer ()
    "Open ibuffer filtered to current project buffers."
    (interactive)
    (if-let ((project (project-current)))
        (let ((project-root (project-root project)))
          (ibuffer nil "*Project Buffers*"
                   `((filename . ,(regexp-quote project-root)))))
      (message "Not in a project")))
  
  ;; Enhanced project find file
  (defun my/project-find-file-dwim ()
    "Find file in project with enhanced completion."
    (interactive)
    (if (project-current)
        (call-interactively #'project-find-file)
      (call-interactively #'find-file)))
  
  ;; Project-aware buffer switching
  (defun my/project-switch-buffer ()
    "Switch to buffer within current project."
    (interactive)
    (if (project-current)
        (call-interactively #'project-switch-to-buffer)
      (call-interactively #'switch-to-buffer)))
  
  ;; Project compilation with directory awareness
  (defun my/project-compile ()
    "Compile project from project root."
    (interactive)
    (if-let ((project (project-current)))
        (let ((default-directory (project-root project)))
          (call-interactively #'compile))
      (call-interactively #'compile)))
  
  ;; Project terminal/shell
  (defun my/project-shell ()
    "Open shell in project root."
    (interactive)
    (if-let ((project (project-current)))
        (let* ((default-directory (project-root project))
               (project-name (file-name-nondirectory 
                             (directory-file-name (project-root project)))))
          (cond
           ((fboundp 'vterm)
            (vterm (format "*vterm-%s*" project-name)))
           ((fboundp 'eat)
            (eat (format "*eat-%s*" project-name)))
           (t
            (shell (format "*shell-%s*" project-name)))))
      (cond
       ((fboundp 'vterm) (vterm))
       ((fboundp 'eat) (eat))
       (t (shell)))))
  
  ;; Project magit integration
  (defun my/project-magit ()
    "Open magit for current project."
    (interactive)
    (if-let ((project (project-current)))
        (if (fboundp 'magit-status)
            (magit-status (project-root project))
          (message "Magit not available"))
      (if (fboundp 'magit-status)
          (call-interactively #'magit-status)
        (message "Magit not available"))))
  
  ;; Project treemacs integration
  (defun my/project-treemacs ()
    "Focus treemacs on current project."
    (interactive)
    (if-let ((project (project-current)))
        (if (fboundp 'treemacs-select-window)
            (progn
              (treemacs-select-window)
              (when (fboundp 'treemacs-add-and-display-current-project-exclusively)
                (treemacs-add-and-display-current-project-exclusively)))
          (message "Treemacs not available"))
      (if (fboundp 'treemacs)
          (treemacs)
        (message "Treemacs not available"))))
  
  ;; Project bookmark integration
  (defun my/project-bookmark ()
    "Set bookmark for current project."
    (interactive)
    (if-let ((project (project-current)))
        (let* ((project-name (file-name-nondirectory 
                             (directory-file-name (project-root project))))
               (bookmark-name (format "Project: %s" project-name)))
          (bookmark-set bookmark-name))
      (call-interactively #'bookmark-set)))
  
  ;; Enhanced project discovery
  (defun my/project-discover-and-switch ()
    "Discover projects and switch to one."
    (interactive)
    (let* ((known-projects (project-known-project-roots))
           (discovered-projects (append
                                (my/find-projects-in-directory "~/projects/" 2)
                                (my/find-projects-in-directory "~/work/" 2)
                                (my/find-projects-in-directory "~/code/" 2)
                                (my/find-projects-in-directory "~/dev/" 2)))
           (all-projects (delete-dups (append known-projects discovered-projects))))
      (if all-projects
          (let ((selected (completing-read "Switch to project: " all-projects)))
            (my/project-switch-with-perspective selected))
        (message "No projects found"))))
  
  (defun my/find-projects-in-directory (dir max-depth)
    "Find projects in DIR up to MAX-DEPTH levels deep."
    (when (and (file-directory-p dir) (> max-depth 0))
      (let ((projects '()))
        (dolist (subdir (directory-files dir t "^[^.]" t))
          (when (file-directory-p subdir)
            (if (my/is-project-directory-p subdir)
                (push subdir projects)
              (setq projects (append projects 
                                   (my/find-projects-in-directory subdir (1- max-depth)))))))
        projects)))
  
  (defun my/is-project-directory-p (dir)
    "Check if DIR is a project directory."
    (or (file-exists-p (expand-file-name ".git" dir))
        (file-exists-p (expand-file-name ".projectile" dir))
        (file-exists-p (expand-file-name "pyproject.toml" dir))
        (file-exists-p (expand-file-name "requirements.txt" dir))
        (file-exists-p (expand-file-name "Pipfile" dir))
        (file-exists-p (expand-file-name "Makefile" dir))
        (file-exists-p (expand-file-name "CMakeLists.txt" dir))))
  
  ;; Project cleanup utilities
  (defun my/project-cleanup-buffers ()
    "Kill buffers not related to current project."
    (interactive)
    (if-let ((project (project-current)))
        (let ((project-buffers (project-buffers project))
              (killed-count 0))
          (dolist (buffer (buffer-list))
            (unless (or (member buffer project-buffers)
                        (string-prefix-p " " (buffer-name buffer))
                        (string-prefix-p "*" (buffer-name buffer)))
              (when (buffer-live-p buffer)
                (kill-buffer buffer)
                (setq killed-count (1+ killed-count)))))
          (message "Killed %d non-project buffers" killed-count))
      (message "Not in a project")))
  
  (defun my/project-kill-all-buffers ()
    "Kill all buffers in current project."
    (interactive)
    (if-let ((project (project-current)))
        (let ((killed-count 0))
          (dolist (buffer (project-buffers project))
            (when (buffer-live-p buffer)
              (kill-buffer buffer)
              (setq killed-count (1+ killed-count))))
          (message "Killed %d project buffers" killed-count))
      (message "Not in a project")))
  
  ;; Project search and replace functions
  (defun my/project-search-dwim ()
    "Search in project with smart defaults."
    (interactive)
    (if (project-current)
        (if (use-region-p)
            (project-find-regexp (buffer-substring-no-properties 
                                 (region-beginning) (region-end)))
          (call-interactively #'project-find-regexp))
      (call-interactively #'grep)))
  
  (defun my/project-replace-dwim ()
    "Replace in project with smart defaults."
    (interactive)
    (if (project-current)
        (if (use-region-p)
            (let ((search-term (buffer-substring-no-properties 
                               (region-beginning) (region-end))))
              (project-query-replace-regexp 
               search-term 
               (read-string (format "Replace '%s' with: " search-term))))
          (call-interactively #'project-query-replace-regexp))
      (call-interactively #'query-replace-regexp)))
  
  ;; Project file operations
  (defun my/project-copy-file-path ()
    "Copy the current file's path relative to project root."
    (interactive)
    (if-let* ((project (project-current))
              (file (buffer-file-name))
              (project-root (project-root project)))
        (let ((relative-path (file-relative-name file project-root)))
          (kill-new relative-path)
          (message "Copied: %s" relative-path))
      (message "Not in a project or not visiting a file")))
  
  (defun my/project-copy-file-path-absolute ()
    "Copy the current file's absolute path."
    (interactive)
    (if-let ((file (buffer-file-name)))
        (progn
          (kill-new file)
          (message "Copied: %s" file))
      (message "Not visiting a file")))
  
  (defun my/project-find-other-file ()
    "Find related file (header/source, test/implementation, etc.)."
    (interactive)
    (if-let ((project (project-current)))
        (let* ((current-file (buffer-file-name))
               (file-name (file-name-nondirectory current-file))
               (file-base (file-name-sans-extension file-name))
               (file-ext (file-name-extension file-name))
               (project-files (project-files project))
               (other-candidates (my/get-related-file-candidates file-base file-ext))
               (found-files '()))
          (dolist (candidate other-candidates)
            (when-let ((found (cl-find-if (lambda (f) (string-suffix-p candidate f)) project-files)))
              (push found found-files)))
          (if found-files
              (find-file (if (= (length found-files) 1)
                            (car found-files)
                          (completing-read "Choose file: " found-files)))
            (message "No related file found")))
      (message "Not in a project")))
  
  (defun my/get-related-file-candidates (file-base file-ext)
    "Get list of related file candidates for FILE-BASE with FILE-EXT."
    (cond
     ;; C/C++ header/source files
     ((string= file-ext "c") 
      (list (concat file-base ".h")))
     ((string= file-ext "h") 
      (list (concat file-base ".c")
            (concat file-base ".cpp")
            (concat file-base ".cc")
            (concat file-base ".cxx")))
     ((member file-ext '("cpp" "cc" "cxx"))
      (list (concat file-base ".h")
            (concat file-base ".hpp")
            (concat file-base ".hxx")))
     ((member file-ext '("hpp" "hxx"))
      (list (concat file-base ".cpp")
            (concat file-base ".cc")
            (concat file-base ".cxx")))
     ;; Test files
     ((string-match-p "_test\\|Test\\|_spec\\|Spec" file-base)
      (let ((clean-base (replace-regexp-in-string "_test\\|Test\\|_spec\\|Spec" "" file-base)))
        (list (concat clean-base "." file-ext))))
     ;; Regular files to test files
     (t (list (concat file-base "_test." file-ext)
              (concat file-base "Test." file-ext)
              (concat file-base "_spec." file-ext)
              (concat file-base "Spec." file-ext)))))
  
  ;; Basic project task runner
  (defun my/project-run-task ()
    "Run project task based on project type."
    (interactive)
    (if-let ((project (project-current)))
        (let ((default-directory (project-root project)))
          (cond
           ((file-exists-p "Makefile") (my/project-make-run))
           ((file-exists-p "pyproject.toml") (my/project-python-run))
           (t (call-interactively #'compile))))
      (message "Not in a project")))
  
  (defun my/project-make-run ()
    "Run make target in current project."
    (let ((targets (my/get-make-targets)))
      (if targets
          (let ((target (completing-read "Make target: " targets)))
            (compile (format "make %s" target)))
        (compile "make"))))
  
  (defun my/get-make-targets ()
    "Get available make targets from Makefile."
    (when (file-exists-p "Makefile")
      (with-temp-buffer
        (insert-file-contents "Makefile")
        (let ((targets '()))
          (while (re-search-forward "^\\([a-zA-Z0-9_-]+\\):" nil t)
            (push (match-string 1) targets))
          (nreverse targets)))))
  
  (defun my/project-python-run ()
    "Run python command in current project."
    (let ((command (completing-read "Python command: " 
                                   '("python -m pytest" "python setup.py test" 
                                     "python -m flake8" "python -m black ."))))
      (compile command))))

;; Enhanced project discovery for specific project types
(with-eval-after-load 'project
  ;; Project type detection functions
  (defun project-try-python (dir)
    "Try to find a Python project at DIR."
    (when-let ((root (or (locate-dominating-file dir "pyproject.toml")
                         (locate-dominating-file dir "setup.py")
                         (locate-dominating-file dir "requirements.txt"))))
      (cons 'python root)))
  
  (defun project-try-make (dir)
    "Try to find a project with Makefile at DIR."
    (when-let ((root (locate-dominating-file dir "Makefile")))
      (cons 'make root)))
  
  (defun project-try-cmake (dir)
    "Try to find a CMake project at DIR."
    (when-let ((root (locate-dominating-file dir "CMakeLists.txt")))
      (cons 'cmake root)))
  
  ;; Add project type detection functions
  (cl-pushnew #'project-try-python project-find-functions)
  (cl-pushnew #'project-try-make project-find-functions)
  (cl-pushnew #'project-try-cmake project-find-functions))

;; Keybindings using general.el
(use-package general
  :ensure t
  :demand t
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  
  ;; Project keybindings under SPC p
  (my/leader-keys
    "p" '(:ignore t :which-key "project")
    
    ;; Core project operations
    "pp" '(my/project-switch-with-perspective :which-key "switch project")
    "pf" '(my/project-find-file-dwim :which-key "find file")
    "pb" '(my/project-switch-buffer :which-key "switch buffer")
    "pd" '(project-dired :which-key "project dired")
    "pk" '(project-kill-buffers :which-key "kill project buffers")
    "pK" '(my/project-cleanup-buffers :which-key "cleanup non-project buffers")
    "px" '(my/project-kill-all-buffers :which-key "kill all project buffers")
    
    ;; Search and navigation
    "ps" '(my/project-search-dwim :which-key "search in project")
    "pr" '(my/project-replace-dwim :which-key "replace in project")
    "po" '(my/project-find-other-file :which-key "find other file")
    
    ;; Project tools
    "pc" '(my/project-compile :which-key "compile project")
    "pR" '(my/project-run-task :which-key "run project task")
    "pt" '(my/project-shell :which-key "project terminal")
    "pg" '(my/project-magit :which-key "project magit")
    "pT" '(my/project-treemacs :which-key "project treemacs")
    
    ;; Project management
    "pi" '(my/project-ibuffer :which-key "project ibuffer")
    "pD" '(my/project-discover-and-switch :which-key "discover projects")
    "pB" '(my/project-bookmark :which-key "bookmark project")
    
    ;; File operations
    "py" '(my/project-copy-file-path :which-key "copy relative path")
    "pY" '(my/project-copy-file-path-absolute :which-key "copy absolute path")
    
    ;; Project admin
    "pa" '(:ignore t :which-key "admin")
    "paa" '(project-remember-project :which-key "add project")
    "pad" '(project-forget-project :which-key "remove project")
    "pal" '(project-switch-project :which-key "list projects")
    "pac" '(project-forget-projects-under :which-key "forget projects under")
    
    ;; Version control integration
    "pv" '(:ignore t :which-key "version control")
    "pvd" '(project-vc-dir :which-key "vc directory")
    "pvg" '(my/project-magit :which-key "magit status")))

;; Ibuffer integration
(use-package ibuffer
  :ensure nil
  :config
  (defun my/ibuffer-project-group ()
    "Generate ibuffer groups based on projects."
    (let ((groups '()))
      ;; Current project group
      (when-let ((current-project (project-current)))
        (let ((current-root (project-root current-project)))
          (push `("Current Project" 
                  (predicate . (lambda ()
                                (when-let ((buf-file (buffer-file-name)))
                                  (string-prefix-p ,current-root buf-file)))))
                groups)))
      ;; Other projects group
      (push `("Other Projects" 
              (predicate . (lambda ()
                            (and (buffer-file-name)
                                 (project-current nil (file-name-directory (buffer-file-name)))
                                 (not (when-let ((current-project (project-current)))
                                       (string-prefix-p (project-root current-project)
                                                      (buffer-file-name))))))))
            groups)
      ;; Non-project buffers
      (push `("Non-Project" 
              (predicate . (lambda ()
                            (not (and (buffer-file-name)
                                     (project-current nil (file-name-directory (buffer-file-name))))))))
            groups)
      (nreverse groups))))

;; Perspective.el integration
(use-package perspective
  :ensure t
  :init
  (persp-mode 1)
  :config
  (defun my/project-perspective-hook ()
    "Create perspective when switching projects."
    (when-let ((project (project-current)))
      (let ((project-name (file-name-nondirectory 
                          (directory-file-name (project-root project)))))
        (unless (member project-name (persp-names))
          (persp-switch project-name)))))
  
  (add-hook 'project-switch-project-hook #'my/project-perspective-hook))

;; Treemacs integration
(use-package treemacs
  :ensure t
  :defer t
  :config
  (defun my/project-treemacs-hook ()
    "Update treemacs when switching projects."
    (when (and (fboundp 'treemacs-current-workspace)
               (project-current))
      (treemacs-add-and-display-current-project-exclusively)))
  
  (add-hook 'project-switch-project-hook #'my/project-treemacs-hook))

;; Completion enhancement
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1)
  :config
  (add-to-list 'marginalia-command-categories
               '(project-switch-project . project)))

;; Bookmark integration
(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-save-flag 1))

;; Provide the feature
(provide 'my-project-config)

``````
