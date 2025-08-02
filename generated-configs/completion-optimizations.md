``````org
* TODO Completion System - Performance Optimized
** Core Completion Framework
*** Emacs Base Completion 
#+begin_src emacs-lisp
(use-package emacs
  :ensure nil
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Reduce cycle threshold for faster completion cycling
  (completion-cycle-threshold 2)
  (completions-detailed t)
  ;; Disable auto-help for faster completion - enable only on demand
  (completion-auto-help nil)
  ;; Faster auto-select behavior
  (completion-auto-select t)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  ;; Emacs 30 and newer: Disable Ispell completion function for speed
  (text-mode-ispell-word-completion nil)
  ;; Hide commands in M-x which do not work in the current mode for speed
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Performance: Use historical sorting for better performance in Emacs 30+
  (completions-sort 'historical)
  ;; Reduce GC threshold temporarily during completion
  (completion-styles-alist completion-styles-alist))
#+end_src

*** Vertico - Vertical completion interface (Optimized)

#+begin_src emacs-lisp
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-scroll-margin 2)
  ;; Disable resize for better performance
  (vertico-resize nil)
  ;; Reduce count for faster rendering
  (vertico-count 8)
  ;; Speed up vertico by reducing delay
  (vertico-preselect 'directory)
  :config
  ;; Lightweight theme customization
  (custom-set-faces
   '(vertico-current ((t (:background "#504945" :foreground "#ebdbb2" :weight bold))))
   '(vertico-group-title ((t (:foreground "#d3869b" :weight bold))))
   '(vertico-group-separator ((t (:foreground "#7c6f64"))))
   '(vertico-multiline ((t (:foreground "#83a598"))))))
#+end_src

*** Corfu - In-buffer completion (Performance Optimized)
#+begin_src emacs-lisp
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  ;; Increase delay to reduce CPU usage and improve responsiveness
  (corfu-auto-delay 0.2)
  ;; Increase prefix for fewer false completions
  (corfu-auto-prefix 3)
  ;; Optimize quit behavior for better performance
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  ;; Disable preview for better performance
  (corfu-preview-current nil)
  ;; Faster preselection
  (corfu-preselect 'directory)
  ;; Disable exact match for speed
  (corfu-on-exact-match nil)
  ;; Reduce scroll margin for faster navigation
  (corfu-scroll-margin 2)
  ;; Reduce candidate count for faster rendering
  (corfu-count 8)
  ;; Disable terminal support for GUI performance
  (corfu-terminal-disable t)
  :init
  (global-corfu-mode)
  ;; Performance: Enable only in specific modes to reduce overhead
  :hook
  ;; Enable corfu only in programming modes for better performance
  ((prog-mode . corfu-mode)
   (text-mode . corfu-mode))
  :config
  ;; Lightweight theme customization
  (custom-set-faces
   '(corfu-default ((t (:background "#282828" :foreground "#ebdbb2"))))
   '(corfu-current ((t (:background "#504945" :foreground "#ebdbb2" :weight bold))))
   '(corfu-bar ((t (:background "#b16286"))))
   '(corfu-border ((t (:background "#7c6f64"))))
   '(corfu-annotations ((t (:foreground "#a89984" :italic t))))
   '(corfu-deprecated ((t (:foreground "#7c6f64" :strike-through t))))))
#+end_src

*** Corfu Extensions (Optimized)
#+begin_src emacs-lisp
(use-package corfu-history
  :ensure nil
  :after (corfu savehist)
  :init (corfu-history-mode 1)
  :config 
  ;; Limit history size for better performance
  (setq corfu-history-length 100)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  ;; Delay popupinfo initialization to improve startup
  :defer 1
  :init (corfu-popupinfo-mode 1)
  ;; Increase delay to reduce CPU usage
  :custom (corfu-popupinfo-delay '(1.0 . 0.5))
  :config
  (custom-set-faces
   '(corfu-popupinfo ((t (:background "#282828" :foreground "#ebdbb2"))))
   '(corfu-popupinfo-documentation ((t (:foreground "#a89984" :italic t))))))
#+end_src

** Annotations, Filtering, and Search (Performance Optimized)
*** Marginalia - Rich annotations in minibuffer

#+begin_src emacs-lisp
(use-package marginalia
  :after vertico
  :init (marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  ;; Reduce annotation detail for speed
  (marginalia-field-width 80)
  :config
  (add-to-list 'marginalia-command-categories
               '(project-switch-project . project))
  ;; Simplified theme faces for better performance
  (custom-set-faces
   '(marginalia-documentation ((t (:foreground "#a89984" :italic t))))
   '(marginalia-file-name ((t (:foreground "#ebdbb2"))))
   '(marginalia-function ((t (:foreground "#83a598"))))
   '(marginalia-variable ((t (:foreground "#8ec07c"))))))
#+end_src

*** Orderless - Optimized fuzzy matching

#+begin_src emacs-lisp
(use-package orderless
  :custom
  ;; Use basic completion for files to improve performance
  (completion-styles '(orderless basic))
  (completion-category-overrides 
   '((file (styles basic partial-completion))
     (buffer (styles orderless))
     (command (styles orderless))
     (variable (styles orderless))
     (symbol (styles orderless))))
  ;; Optimize orderless for better performance
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  :config
  ;; Reduce face count for better performance
  (custom-set-faces
   '(orderless-match-face-0 ((t (:foreground "#d3869b" :weight bold))))
   '(orderless-match-face-1 ((t (:foreground "#83a598" :weight bold))))))
#+end_src

*** Corfu/Orderless Integration (Optimized)
#+begin_src emacs-lisp
(with-eval-after-load 'corfu
  (with-eval-after-load 'orderless
    ;; Optimize orderless for corfu
    (defun corfu-orderless-setup ()
      (setq-local completion-styles '(orderless basic)
                  completion-category-defaults nil
                  completion-category-overrides nil))
    (add-hook 'corfu-mode-hook #'corfu-orderless-setup)))
#+end_src

*** Consult - Enhanced search commands (Performance Optimized)

#+begin_src emacs-lisp
(use-package consult
  :after vertico
  :custom
  ;; Basic consult settings
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  ;; Increase min input to reduce CPU usage
  (consult-async-min-input 3)
  ;; Optimize async timings for better performance
  (consult-async-refresh-delay 0.3)
  (consult-async-input-throttle 0.4)
  (consult-async-input-debounce 0.2)
  
  ;; Optimized external tool configurations
  (consult-find-args "fd --color=never --type f --strip-cwd-prefix")
  (consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=500 --path-separator / --smart-case --no-heading --with-filename --line-number --hidden --glob '!.git/' --max-count=200")
  (consult-grep-args "rg --null --line-buffered --color=never --max-columns=500 --path-separator / --smart-case --no-heading --with-filename --line-number --max-count=200")
  
  :config
  ;; Optimize xref settings
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Faster project detection
  (when (fboundp 'project-current)
    (setq consult-project-function
          (lambda (_)
            (when-let (project (project-current))
              (project-root project)))))

  ;; Disable preview by default for better performance
  (setq consult-preview-key nil)
  
  ;; Simplified theme faces
  (custom-set-faces
   '(consult-buffer ((t (:foreground "#ebdbb2"))))
   '(consult-file ((t (:foreground "#8ec07c"))))
   '(consult-line-number ((t (:foreground "#7c6f64"))))))
#+end_src

*** Consult Extensions (Minimal)

#+begin_src emacs-lisp
(use-package consult-flymake :ensure nil :after (consult flymake))
#+end_src

*** Wgrep - Editable grep buffers (Optimized)

#+begin_src emacs-lisp
(use-package wgrep
  :after consult
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-enable-key "r")
  (wgrep-change-readonly-file t)
  ;; Reduce face customization for better performance
  :config
  (custom-set-faces
   '(wgrep-face ((t (:background "#504945" :foreground "#ebdbb2"))))))
#+end_src

** Actions and Completion-at-Point (Performance Optimized)
*** Embark - Context-aware actions (Lazy Loading)

#+begin_src emacs-lisp
(use-package embark
  :ensure t
  :defer 2  ; Delay loading for better startup performance
  :after (vertico consult)
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim))
  :init
  ;; Use Embark for prefix help
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Minimal display configuration
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
#+end_src

*** Embark-Consult Integration (Lazy)

#+begin_src emacs-lisp
(use-package embark-consult
  :after (embark consult)
  :defer 3
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
#+end_src

*** Cape - Completion at point extensions (Optimized)

#+begin_src emacs-lisp
(use-package cape
  :after corfu
  :defer 1
  :bind (("C-c p p" . completion-at-point)
         ("C-c p d" . cape-dabbrev)
         ("C-c p f" . cape-file)
         ("C-c p s" . cape-elisp-symbol))
  :init
  ;; Add only essential completion functions to reduce overhead
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; Enable elisp symbol completion only in emacs-lisp-mode
  :hook
  (emacs-lisp-mode . (lambda () 
                       (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)))
  :config
  ;; Optimize cape for better performance
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
#+end_src

** Performance Optimizations

#+begin_src emacs-lisp
;; General performance optimizations for completion
(setq read-process-output-max (* 1024 1024)) ; 1MB for better LSP performance
(setq gc-cons-threshold (* 100 1024 1024)) ; 100MB during completion
(setq gc-cons-percentage 0.1)

;; Optimize minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024))))

(add-hook 'minibuffer-exit-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024))))

;; Reduce font-lock delay for better typing responsiveness
(setq jit-lock-defer-time 0.05)
(setq jit-lock-stealth-time 0.1)

;; Optimize completion for large datasets
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
#+end_src

** Custom Functions (Performance Optimized)

#+begin_src emacs-lisp
;;; Optimized search functions
(defun my/consult-line-symbol-at-point ()
  "Run consult-line with symbol at point as initial input."
  (interactive)
  (when-let ((symbol (thing-at-point 'symbol)))
    (consult-line symbol)))

(defun my/consult-ripgrep-symbol-at-point ()
  "Run consult-ripgrep with symbol at point as initial input."
  (interactive)
  (when-let ((symbol (thing-at-point 'symbol)))
    (consult-ripgrep nil symbol)))

;; Performance monitoring function
(defun my/completion-benchmark ()
  "Benchmark completion performance."
  (interactive)
  (let ((start-time (current-time)))
    (call-interactively #'completion-at-point)
    (message "Completion took: %f seconds" 
             (float-time (time-subtract (current-time) start-time)))))
#+end_src

``````
