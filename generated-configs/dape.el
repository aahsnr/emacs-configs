(use-package dape
  :ensure t
  :config
  
  ;; Basic settings
  (setq dape-key-prefix "\C-x\C-a"
        dape-buffer-window-arrangement 'right
        dape-inlay-hints t
        dape-read-memory-default-count 32)

  ;; Enable eldoc integration
  (add-hook 'dape-mode-hook #'eldoc-mode)

  ;; Gruvbox dark theme faces
  (custom-set-faces
   '(dape-breakpoint-face
     ((t (:background "#cc241d" :foreground "#fbf1c7" :weight bold))))
   '(dape-exception-face
     ((t (:background "#fb4934" :foreground "#fbf1c7" :weight bold))))
   '(dape-source-line-stopped-face
     ((t (:background "#458588" :foreground "#fbf1c7" :weight bold))))
   '(dape-repl-prompt-face
     ((t (:foreground "#b8bb26" :weight bold))))
   '(dape-ui-locals-name
     ((t (:foreground "#8ec07c" :weight bold))))
   '(dape-ui-locals-value
     ((t (:foreground "#fe8019"))))
   '(dape-ui-locals-type
     ((t (:foreground "#d3869b"))))
   '(dape-ui-breakpoints-file
     ((t (:foreground "#83a598"))))
   '(dape-ui-breakpoints-line
     ((t (:foreground "#fabd2f"))))
   '(dape-ui-memory-address
     ((t (:foreground "#fabd2f"))))
   '(dape-ui-memory-value
     ((t (:foreground "#fe8019")))))

  ;; Flymake integration - disable during debugging
  (add-hook 'dape-mode-hook
            (lambda ()
              (when (bound-and-true-p flymake-mode)
                (setq-local dape--flymake-was-enabled t)
                (flymake-mode -1))))
  
  (add-hook 'dape-mode-exit-hook
            (lambda ()
              (when (and (boundp 'dape--flymake-was-enabled)
                         dape--flymake-was-enabled)
                (flymake-mode 1))))

  ;; Eldoc integration for debugging context
  (defun dape-eldoc-function (callback &rest _)
    "Provide debugging context to eldoc."
    (when (and (boundp 'dape--process) 
               dape--process 
               (dape--stopped-threads dape--process))
      (when-let ((info (ignore-errors (dape--variable-at-point))))
        (funcall callback (format "%s: %s" (car info) (cdr info))))))
  
  (add-hook 'dape-mode-hook
            (lambda ()
              (add-hook 'eldoc-documentation-functions #'dape-eldoc-function nil t)))

  ;; Corfu integration for REPL completion
  (with-eval-after-load 'corfu
    (add-hook 'dape-repl-mode-hook
              (lambda ()
                (setq-local corfu-auto t)
                (corfu-mode 1))))

  ;; Cape integration for enhanced REPL completion
  (with-eval-after-load 'cape
    (add-hook 'dape-repl-mode-hook
              (lambda ()
                (setq-local completion-at-point-functions
                           (list (cape-capf-super
                                  #'dape-repl-completion-at-point
                                  #'cape-file))))))

  ;; Tree-sitter integration for function detection
  (with-eval-after-load 'treesit
    (defun dape-treesit-current-function ()
      "Get current function name using tree-sitter."
      (when (treesit-parser-list)
        (let ((node (treesit-node-at (point))))
          (while (and node
                      (not (member (treesit-node-type node)
                                   '("function_definition" "method_definition"
                                     "function_declaration" "method_declaration"))))
            (setq node (treesit-node-parent node)))
          (when node
            (let ((name-node (treesit-node-child-by-field-name node "name")))
              (when name-node
                (treesit-node-text name-node)))))))
    
    (add-hook 'dape-mode-hook
              (lambda ()
                (when (treesit-parser-list)
                  (setq-local which-func-functions '(dape-treesit-current-function))))))

  ;; Project integration for workspace detection
  (with-eval-after-load 'project
    (defun dape-cwd ()
      "Get current working directory for debugging."
      (if-let ((project (project-current)))
          (project-root project)
        default-directory)))

  ;; ;; Language Configurations
  ;; Python debugging with debugpy
  (add-to-list 'dape-configs
               `(debugpy
                 modes (python-mode python-ts-mode)
                 command "python"
                 command-args ("-m" "debugpy.adapter")
                 :request "launch"
                 :type "executable"
                 :cwd dape-cwd
                 :program dape-find-file-buffer-default
                 :console "integratedTerminal"
                 :justMyCode :json-false
                 :showReturnValue t))

  ;; C/C++ debugging with GDB
  (add-to-list 'dape-configs
               `(gdb
                 modes (c-mode c-ts-mode c++-mode c++-ts-mode)
                 command "gdb"
                 command-args ("--interpreter=dap")
                 :request "launch"
                 :type "gdb"
                 :cwd dape-cwd
                 :program (lambda () 
                           (expand-file-name
                            (read-file-name "Program: " 
                                          (or (dape-cwd) default-directory))))
                 :args []))

  ;; UI and Window Management
  ;; Enhanced variable display formatting
  (setq dape-ui-variable-format-function
        (lambda (name value type)
          (format "%s %s = %s"
                  (propertize type 'face 'dape-ui-locals-type)
                  (propertize name 'face 'dape-ui-locals-name)
                  (propertize value 'face 'dape-ui-locals-value))))

  ;; Auto-open debug info window
  (add-hook 'dape-start-hook
            (lambda ()
              (save-selected-window (dape-info))))

  ;; Cleanup debug buffers on termination
  (add-hook 'dape-termination-hook
            (lambda ()
              (dolist (buffer-name '("*dape-info*" "*dape-repl*"))
                (when-let ((buffer (get-buffer buffer-name)))
                  (kill-buffer buffer)))))

  ;; Keybindings
  :bind (;; Core debugging commands
         ("C-x C-a C-d" . dape)
         ("C-x C-a C-k" . dape-kill)
         ("C-x C-a C-q" . dape-quit)
         ("C-x C-a C-R" . dape-restart)
         
         ;; Breakpoint management
         ("C-x C-a C-b" . dape-breakpoint-toggle)
         ("C-x C-a C-l" . dape-breakpoint-log)
         ("C-x C-a C-e" . dape-breakpoint-expression)
         ("C-x C-a C-r" . dape-breakpoint-remove-all)
         
         ;; Stepping commands
         ("C-x C-a C-n" . dape-next)
         ("C-x C-a C-s" . dape-step-in)
         ("C-x C-a C-o" . dape-step-out)
         ("C-x C-a C-c" . dape-continue)
         ("C-x C-a C-p" . dape-pause)
         
         ;; Stack navigation
         ("C-x C-a C-u" . dape-stack-select-up)
         ("C-x C-a C-D" . dape-stack-select-down)
         
         ;; Information and utilities
         ("C-x C-a C-i" . dape-info)
         ("C-x C-a C-S" . dape-repl)
         ("C-x C-a C-w" . dape-watch-dwim)
         ("C-x C-a C-m" . dape-read-memory)))

;; Helper Functions
(defun dape-python-with-args ()
  "Debug current Python script with custom arguments."
  (interactive)
  (let* ((args (split-string (read-string "Arguments: ") " " t))
         (dape-configs 
          (cons `(debugpy-args
                  modes (python-mode python-ts-mode)
                  command "python"
                  command-args ("-m" "debugpy.adapter")
                  :request "launch"
                  :type "executable"
                  :cwd ,(or (dape-cwd) default-directory)
                  :program ,(buffer-file-name)
                  :args ,args
                  :console "integratedTerminal"
                  :justMyCode :json-false
                  :showReturnValue t)
                dape-configs)))
    (dape 'debugpy-args)))

(defun dape-gdb-with-args ()
  "Debug C/C++ executable with custom arguments."
  (interactive)
  (let* ((executable (read-file-name "Executable: " 
                                    (or (dape-cwd) default-directory)))
         (args (split-string (read-string "Arguments: ") " " t))
         (dape-configs 
          (cons `(gdb-args
                  modes (c-mode c-ts-mode c++-mode c++-ts-mode)
                  command "gdb"
                  command-args ("--interpreter=dap")
                  :request "launch"
                  :type "gdb"
                  :cwd ,(or (dape-cwd) default-directory)
                  :program ,executable
                  :args ,args)
                dape-configs)))
    (dape 'gdb-args)))

;; Bind helper functions
(with-eval-after-load 'dape
  (define-key dape-mode-map (kbd "C-x C-a C-A") #'dape-python-with-args)
  (define-key dape-mode-map (kbd "C-x C-a C-G") #'dape-gdb-with-args))


