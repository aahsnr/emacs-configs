``````el
;; Automatic tree-sitter mode management
(use-package treesit-auto
  :when (treesit-available-p)
  :config
  ;; Configure which modes to auto-enable
  (setq treesit-auto-langs '(bash c cpp css json python rust toml yaml))
  
  ;; Install and configure tree-sitter modes automatically
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Basic tree-sitter configuration
(use-package treesit
  :ensure nil 
  :when (treesit-available-p)
  :config
  (setq treesit-font-lock-level 4) ; Maximum syntax highlighting
  
  ;; Performance optimizations for tree-sitter
  (setq treesit-max-buffer-size (* 1024 1024)) ; 1MB limit
  
  ;; Function to toggle tree-sitter debugging
  (defun my/treesit-debug-toggle ()
    "Toggle tree-sitter debugging."
    (interactive)
    (if (bound-and-true-p treesit--indent-verbose)
        (progn
          (setq treesit--indent-verbose nil)
          (message "Tree-sitter debugging disabled"))
      (setq treesit--indent-verbose t)
      (message "Tree-sitter debugging enabled")))
  
  ;; Function to show tree-sitter information
  (defun my/treesit-info ()
    "Show tree-sitter information for current buffer."
    (interactive)
    (if (treesit-parser-list)
        (let ((parsers (treesit-parser-list)))
          (message "Tree-sitter parsers: %s"
                   (mapconcat (lambda (parser)
                               (symbol-name (treesit-parser-language parser)))
                             parsers ", ")))
      (message "No tree-sitter parsers in current buffer")))
  
  ;; Keybindings
  (global-set-key (kbd "C-c t d") #'my/treesit-debug-toggle)
  (global-set-key (kbd "C-c t i") #'my/treesit-info))

;; Language-specific configurations for tree-sitter modes
;; treesit-auto handles mode associations automatically

;; Python setup
(defun my/python-ts-mode-setup ()
  "Setup for Python tree-sitter mode."
  (setq-local indent-tabs-mode nil)
  (setq-local python-indent-offset 4)
  (setq-local tab-width 4))

;; C/C++ setup
(defun my/c-ts-mode-setup ()
  "Setup for C tree-sitter mode."
  (setq-local c-ts-mode-indent-offset 4)
  (setq-local tab-width 4))

(defun my/c++-ts-mode-setup ()
  "Setup for C++ tree-sitter mode."
  (setq-local c-ts-mode-indent-offset 4)
  (setq-local tab-width 4))

(defun my/json-ts-mode-setup ()
  "Setup for JSON tree-sitter mode."
  (setq-local js-indent-level 2)
  (setq-local tab-width 2))

(defun my/yaml-ts-mode-setup ()
  "Setup for YAML tree-sitter mode."
  (setq-local yaml-indent-offset 2)
  (setq-local tab-width 2))

(defun my/css-ts-mode-setup ()
  "Setup for CSS tree-sitter mode."
  (setq-local css-indent-offset 2)
  (setq-local tab-width 2))

(defun my/bash-ts-mode-setup ()
  "Setup for Bash tree-sitter mode."
  (setq-local sh-basic-offset 2)
  (setq-local tab-width 2))

;; Apply hooks for language-specific setups
(add-hook 'python-ts-mode-hook #'my/python-ts-mode-setup)
(add-hook 'c-ts-mode-hook #'my/c-ts-mode-setup)
(add-hook 'c++-ts-mode-hook #'my/c++-ts-mode-setup)
(add-hook 'json-ts-mode-hook #'my/json-ts-mode-setup)
(add-hook 'yaml-ts-mode-hook #'my/yaml-ts-mode-setup)
(add-hook 'css-ts-mode-hook #'my/css-ts-mode-setup)
(add-hook 'bash-ts-mode-hook #'my/bash-ts-mode-setup)

;; Enhanced electric pair behavior for tree-sitter modes
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-local-mode)
  :config
  ;; Enhanced electric pair behavior for tree-sitter modes
  (defun my/treesit-electric-pair-inhibit (char)
    "Inhibit electric pairing in certain tree-sitter contexts."
    (when (treesit-parser-list)
      (let ((node (treesit-node-at (point))))
        (and node
             (or (string-match-p "string\\|comment" 
                                (treesit-node-type node))
                 (and (eq char ?\")
                      (string-match-p "string" 
                                     (treesit-node-type node))))))))
  
  (setq electric-pair-inhibit-predicate #'my/treesit-electric-pair-inhibit))

;; Optional: Tree-sitter folding (requires external package)
(use-package treesit-fold
  :ensure t
  :when (treesit-available-p)
  :hook ((prog-mode . treesit-fold-mode))
  :bind (("C-c C-f" . treesit-fold-toggle)
         ("C-c C-o" . treesit-fold-open-all)
         ("C-c C-c" . treesit-fold-close-all)))

;; Optional: Structural navigation and editing (requires external package)
(use-package combobulate
  :ensure t
  :when (treesit-available-p)
  :hook ((python-ts-mode
          js-ts-mode
          typescript-ts-mode
          tsx-ts-mode
          css-ts-mode
          yaml-ts-mode
          json-ts-mode) . combobulate-mode)
  :bind (:map combobulate-key-map
              ("C-c o u" . combobulate-splice-up)
              ("C-c o d" . combobulate-splice-down)
              ("C-c o c" . combobulate-clone-node-dwim)
              ("C-c o k" . combobulate-kill-node-dwim)
              ("C-c o f" . combobulate-navigate-next)
              ("C-c o b" . combobulate-navigate-previous)))
``````
