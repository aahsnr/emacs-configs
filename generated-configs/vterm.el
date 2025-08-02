;; VTerm Configuration - Clean and Optimized
;; Prerequisites: general.el, use-package

(use-package vterm
  :ensure t
  :defer t
  :init
  ;; Core settings
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 10000
        vterm-shell "/bin/zsh"
        vterm-timer-delay 0.01)
  
  ;; Environment variables for zsh integration
  (setq vterm-environment
        '("TERM=xterm-256color"
          "EMACS_VTERM=t"))
  
  :config
  ;; Enable find-file and message commands from vterm
  (setq vterm-eval-cmds
        '(("find-file" find-file)
          ("message" message)))
  
  ;; Buffer setup hook
  (defun my/vterm-setup ()
    "Configure vterm buffer settings."
    (setq-local truncate-lines nil
                scroll-conservatively 101
                scroll-margin 0))
  
  (add-hook 'vterm-mode-hook #'my/vterm-setup)
  
  ;; Key bindings for vterm mode
  :bind (:map vterm-mode-map
              ("C-q" . vterm-send-next-key)
              ("C-c C-t" . vterm-copy-mode)))

(use-package vterm-toggle
  :ensure t
  :after vterm
  :init
  ;; Configure vterm-toggle behavior
  (setq vterm-toggle-fullscreen-p nil
        vterm-toggle-scope 'project)
  
  :config
  ;; Display vterm at bottom with proper sizing
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  
  ;; Project integration functions
  (defun my/vterm-project-root ()
    "Get project root directory."
    (if (fboundp 'project-root)
        (when-let ((project (project-current)))
          (project-root project))
      default-directory))
  
  (defun my/vterm-toggle-project ()
    "Toggle vterm in project root."
    (interactive)
    (let* ((project-root (my/vterm-project-root))
           (default-directory project-root))
      (vterm-toggle)))
  
  (defun my/vterm-new-project ()
    "Create new vterm buffer in project root."
    (interactive)
    (let* ((project-root (my/vterm-project-root))
           (project-name (file-name-nondirectory) 
                         (directory-file-name project-root))
           (buffer-name (format "*vterm-%s*" project-name))
           (default-directory project-root))
      (vterm buffer-name)))
  
  ;; Utility functions
  (defun my/vterm-send-clear ()
    "Clear vterm buffer."
    (interactive)
    (vterm-send-string "clear")
    (vterm-send-return))
  
  (defun my/vterm-send-cd-project ()
    "Change directory to project root in vterm."
    (interactive)
    (when-let ((project-root (my/vterm-project-root)))
      (vterm-send-string (format "cd %s" (shell-quote-argument project-root)))
      (vterm-send-return))))

;; General.el keybindings
(use-package general
  :config
  ;; Assuming you have a leader key definer already set up
  ;; If not, uncomment and modify the following:
  ;; (general-create-definer my/leader-keys
  ;;   :states '(normal insert visual emacs)
  ;;   :keymaps 'override
  ;;   :prefix "SPC"
  ;;   :global-prefix "M-SPC")
  
  ;; Main vterm keybindings
  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps 'override
   :prefix "SPC"
   :global-prefix "M-SPC"
   
   ;; Primary toggle
   "'" 'vterm-toggle
   
   ;; Terminal submenu
   "t" '(:ignore t :wk "terminal")
   "tt" 'vterm-toggle
   "tn" 'vterm
   "tp" 'my/vterm-toggle-project
   "tN" 'my/vterm-new-project
   "tc" 'my/vterm-send-clear
   "tr" 'my/vterm-send-cd-project
   "ty" 'vterm-copy-mode)
  
  ;; Additional bindings for vterm mode
  (general-define-key
   :states '(normal insert)
   :keymaps 'vterm-mode-map
   "C-c C-c" 'vterm-send-C-c
   "C-c C-d" 'vterm-send-C-d
   "C-c C-l" 'my/vterm-send-clear
   "C-c C-r" 'my/vterm-send-cd-project))

;; Optional: Zsh integration setup
(defun my/create-zsh-vterm-config ()
  "Create zsh configuration for vterm integration."
  (interactive)
  (let ((config-content "# Vterm integration for zsh
if [[ \"$EMACS_VTERM\" == \"t\" ]]; then
    # Directory tracking
    vterm_printf() {
        if [ -n \"$TMUX\" ] && ([ \"${TERM%%-*}\" = \"tmux\" ] || [ \"${TERM%%-*}\" = \"screen\" ]); then
            printf \"\\ePtmux;\\e\\e]%s\\007\\e\\\\\" \"$1\"
        elif [ \"${TERM%%-*}\" = \"screen\" ]; then
            printf \"\\eP\\e]%s\\007\\e\\\\\" \"$1\"
        else
            printf \"\\e]%s\\e\\\\\" \"$1\"
        fi
    }
    
    vterm_prompt_end() {
        vterm_printf \"51;A$(whoami)@$(hostname):$(pwd)\"
    }
    
    # Add to prompt
    setopt PROMPT_SUBST
    PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
    
    # Clear function
    vterm_clear() {
        vterm_printf \"51;Evterm-clear-scrollback\"
        clear
    }
    
    # Directory sync on cd
    chpwd() {
        vterm_printf \"51;A$(whoami)@$(hostname):$(pwd)\"
    }
    
    # Alias for clear
    alias clear='vterm_clear'
fi
"))
    (write-region config-content nil (expand-file-name "~/.zshrc.vterm"))
    (message "Zsh vterm config created at ~/.zshrc.vterm")
    (message "Add 'source ~/.zshrc.vterm 2>/dev/null || true' to your ~/.zshrc")))

;; Create the zsh config file
(when (and (executable-find "zsh") 
           (not (file-exists-p "~/.zshrc.vterm")))
  (my/create-zsh-vterm-config))
