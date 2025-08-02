``````el
;; Which-key for keybinding discovery (complements helpful)
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode 1)
  :config
  (setq which-key-idle-delay 0.5
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-add-column-padding 1
        which-key-min-display-lines 6))

;; Elisp-demos for better function examples (complements helpful)
(use-package elisp-demos
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; Main helpful package configuration
(use-package helpful
  :commands (helpful-callable
             helpful-variable
             helpful-command
             helpful-key
             helpful-macro
             helpful-function
             helpful-at-point)
  :init
  ;; Variables for enhanced functionality
  (defvar my-helpful-history nil
    "History of helpful queries.")
  
  ;; Custom functions
  (defun my-helpful-kill-all-buffers ()
    "Kill all helpful buffers."
    (interactive)
    (dolist (buffer (buffer-list))
      (when (string-match-p "\\*helpful" (buffer-name buffer))
        (kill-buffer buffer)))
    (message "All helpful buffers killed"))
  
  (defun my-helpful-add-to-history (symbol type)
    "Add SYMBOL and TYPE to helpful history."
    (push (cons symbol type) my-helpful-history)
    ;; Keep history to reasonable size
    (when (> (length my-helpful-history) 50)
      (setq my-helpful-history (butlast my-helpful-history 10))))
  
  (defun my-helpful-history-back ()
    "Go back in helpful history."
    (interactive)
    (when my-helpful-history
      (let* ((entry (pop my-helpful-history))
             (symbol (car entry))
             (type (cdr entry)))
        (pcase type
          ('callable (helpful-callable symbol))
          ('variable (helpful-variable symbol))
          ('command (helpful-command symbol))
          ('function (helpful-function symbol))
          ('macro (helpful-macro symbol))))))
  
  (defun my-helpful-at-point-dwim ()
    "Show helpful info for symbol at point, with intelligent fallback."
    (interactive)
    (let ((symbol (symbol-at-point)))
      (cond
       ((null symbol)
        (call-interactively #'helpful-callable))
       ((commandp symbol)
        (helpful-command symbol))
       ((fboundp symbol)
        (helpful-callable symbol))
       ((boundp symbol)
        (helpful-variable symbol))
       (t
        (helpful-callable symbol)))))
  
  (defun my-helpful-search-documentation ()
    "Search through current helpful buffer documentation."
    (interactive)
    (if (derived-mode-p 'helpful-mode)
        (let ((term (read-string "Search in buffer for: ")))
          (when (not (string-empty-p term))
            (occur term)))
      (message "Not in a helpful buffer")))
  
  :config
  ;; Add history tracking
  (advice-add 'helpful-callable :before
              (lambda (symbol) (my-helpful-add-to-history symbol 'callable)))
  (advice-add 'helpful-variable :before
              (lambda (symbol) (my-helpful-add-to-history symbol 'variable)))
  (advice-add 'helpful-command :before
              (lambda (symbol) (my-helpful-add-to-history symbol 'command)))
  (advice-add 'helpful-function :before
              (lambda (symbol) (my-helpful-add-to-history symbol 'function)))
  (advice-add 'helpful-macro :before
              (lambda (symbol) (my-helpful-add-to-history symbol 'macro)))
  
  ;; Enhance helpful buffers
  (add-hook 'helpful-mode-hook
            (lambda ()
              (visual-line-mode 1)
              (hl-line-mode 1)))
  
  ;; Set up keybindings with general.el
  (my-leader-def
    "h" '(:ignore t :which-key "helpful")
    "hf" '(helpful-callable :which-key "function/macro/command")
    "hv" '(helpful-variable :which-key "variable")
    "hk" '(helpful-key :which-key "key")
    "hc" '(helpful-command :which-key "command")
    "hm" '(helpful-macro :which-key "macro")
    "hF" '(helpful-function :which-key "function")
    "hp" '(helpful-at-point :which-key "at point")
    "h." '(my-helpful-at-point-dwim :which-key "at point (smart)")
    "hb" '(my-helpful-history-back :which-key "history back")
    "hK" '(my-helpful-kill-all-buffers :which-key "kill all helpful buffers")
    "hs" '(my-helpful-search-documentation :which-key "search in buffer"))
  
  ;; Override default Emacs help bindings
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   ([remap describe-command] . helpful-command)
   ([remap describe-symbol] . helpful-at-point)
   :map helpful-mode-map
   ("q" . quit-window)
   ("g" . helpful-update)
   ("TAB" . forward-button)
   ("S-TAB" . backward-button)
   ("<backtab>" . backward-button)))

;; Optional: Embark integration for helpful (only if embark is available)
(use-package embark
  :defer t
  :config
  (when (fboundp 'embark-define-keymap)
    (defun embark-helpful-function (symbol)
      "Show helpful info for function SYMBOL."
      (helpful-callable symbol))
    
    (defun embark-helpful-variable (symbol)
      "Show helpful info for variable SYMBOL."
      (helpful-variable symbol))
    
    (with-eval-after-load 'embark
      (define-key embark-symbol-map "h" #'embark-helpful-function)
      (define-key embark-variable-map "h" #'embark-helpful-variable)
      (define-key embark-function-map "h" #'embark-helpful-function)
      (define-key embark-command-map "h" #'embark-helpful-function))))

;; Eldoc integration for better in-buffer help
(use-package eldoc
  :ensure nil ; Built-in package
  :diminish eldoc-mode
  :config
  (global-eldoc-mode 1)
  (setq eldoc-idle-delay 0.2))

;; Provide feature
(provide 'helpful-config)

``````
