``````el
;;; Enhanced Evil Configuration for Emacs with Doom Themes
;;; Advanced Vim emulation setup (package management assumed)

;;; Doom Themes Configuration
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  
  ;; Load the theme (doom-one, doom-molokai, etc)
  (load-theme 'doom-tokyo-night t)
  
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; All-the-icons for better visual experience
(use-package all-the-icons
  :if (display-graphic-p))

;;; Doom Modeline for enhanced status bar with Evil integration
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-vcs-max-length 12)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-irc-stylize 'identity)
  (setq doom-modeline-github-timer nil)
  (setq doom-modeline-gnus-timer nil)
  
  ;; Evil state integration - show Evil state in modeline
  (setq doom-modeline-modal t)
  (setq doom-modeline-modal-icon t))

;;; Core Evil Configuration
(use-package evil
  :init
  ;; Pre-configuration settings
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-w-delete t)
  (setq evil-want-C-g-bindings t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-fine-undo t)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-d-scroll t)
  
  :config
  (evil-mode 1)
  
  ;; Enhanced visual line motions
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion "gj" 'evil-next-line)
  (evil-global-set-key 'motion "gk" 'evil-previous-line)
  
  ;; Better beginning/end of line
  (evil-global-set-key 'motion "0" 'evil-beginning-of-visual-line)
  (evil-global-set-key 'motion "$" 'evil-end-of-visual-line)
  (evil-global-set-key 'motion "g0" 'evil-beginning-of-line)
  (evil-global-set-key 'motion "g$" 'evil-end-of-line)
  
  ;; Enhanced word motions
  (evil-global-set-key 'motion "W" 'evil-forward-WORD-begin)
  (evil-global-set-key 'motion "E" 'evil-forward-WORD-end)
  (evil-global-set-key 'motion "B" 'evil-backward-WORD-begin)
  (evil-global-set-key 'motion "gE" 'evil-backward-WORD-end)
  
  ;; Paragraph motions with better logic
  (evil-global-set-key 'motion "{" 'evil-backward-paragraph)
  (evil-global-set-key 'motion "}" 'evil-forward-paragraph)
  
  ;; Enhanced search behavior
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-ex-search-persistent-highlight nil)
  (setq evil-ex-search-case 'smart)
  (setq evil-ex-search-vim-style-regexp t)
  
  ;; Better repeat behavior
  (setq evil-repeat-move-cursor t)
  
  ;; Enhanced cursor behavior
  (setq evil-move-cursor-back nil)
  (setq evil-move-beyond-eol nil)
  (setq evil-cross-lines t)
  
  ;; Initial states for various modes
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'debugger-mode 'normal)
  (evil-set-initial-state 'pdf-view-mode 'normal)
  (evil-set-initial-state 'tabulated-list-mode 'normal)
  (evil-set-initial-state 'profiler-report-mode 'normal)
  (evil-set-initial-state 'dired-mode 'normal)
  (evil-set-initial-state 'compilation-mode 'normal)
  (evil-set-initial-state 'help-mode 'normal)
  (evil-set-initial-state 'woman-mode 'normal)
  (evil-set-initial-state 'calc-mode 'normal)
  (evil-set-initial-state 'Man-mode 'normal)
  (evil-set-initial-state 'calendar-mode 'normal)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  
  ;; Advanced visual state enhancements
  (setq evil-visual-update-x-selection-p nil)
  (setq evil-kill-on-visual-paste nil)
  
  ;; Better ex command completion
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-complete-all-buffers nil)
  
  ;; Enhanced cursor configuration with Tokyo Night colors
  (setq evil-default-cursor t)
  (setq evil-normal-state-cursor '(box "#7aa2f7"))      ; Tokyo Night blue
  (setq evil-insert-state-cursor '((bar . 2) "#9ece6a")) ; Tokyo Night green
  (setq evil-visual-state-cursor '(box "#bb9af7"))       ; Tokyo Night purple
  (setq evil-replace-state-cursor '(box "#f7768e"))      ; Tokyo Night red
  (setq evil-operator-state-cursor '(hollow "#e0af68"))  ; Tokyo Night yellow
  (setq evil-motion-state-cursor '(box "#7dcfff"))       ; Tokyo Night cyan
  (setq evil-emacs-state-cursor '(box "#73daca"))        ; Tokyo Night teal
  
  ;; Enhanced Evil state tags for modeline integration
  (setq evil-normal-state-tag   " 󰕷 NORMAL ")
  (setq evil-emacs-state-tag    " 󰰤 EMACS ")
  (setq evil-insert-state-tag   " 󰙞 INSERT ")
  (setq evil-replace-state-tag  " 󰯸 REPLACE ")
  (setq evil-motion-state-tag   " 󰺅 MOTION ")
  (setq evil-visual-state-tag   " 󰈊 VISUAL ")
  (setq evil-operator-state-tag " 󰛂 OPERATOR "))

;;; Evil Collection - Comprehensive mode bindings
(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t)
  (setq evil-collection-setup-debugger-keys t)
  :config
  (evil-collection-init))

;;; Evil Nerd Commenter - Advanced commenting
(use-package evil-nerd-commenter
  :after evil
  :config
  ;; Enhanced commenting configuration
  (setq evilnc-invert-comment-line-by-line t)
  (setq evilnc-comment-empty-lines t)
  
  ;; Key bindings for evil-nerd-commenter
  (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
  (define-key evil-visual-state-map "gc" 'evilnc-comment-operator)
  (define-key evil-normal-state-map "gcc" 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map "gcp" 'evilnc-comment-or-uncomment-paragraphs)
  (define-key evil-normal-state-map "gcr" 'comment-or-uncomment-region)
  (define-key evil-normal-state-map "gcv" 'evilnc-toggle-invert-comment-line-by-line)
  (define-key evil-normal-state-map "gc$" 'evilnc-comment-to-the-line)
  (define-key evil-normal-state-map "gcy" 'evilnc-copy-and-comment-lines)
  (define-key evil-normal-state-map "gcY" 'evilnc-copy-and-comment-operator)
  
  ;; Quick comment/uncomment
  (define-key evil-normal-state-map (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "M-;") 'evilnc-comment-or-uncomment-lines))

;;; Evil Surround - Enhanced surrounding operations
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)
  
  ;; Add custom surrounds
  (setq-default evil-surround-pairs-alist
                (append evil-surround-pairs-alist
                        '((?` . ("`" . "`"))
                          (?~ . ("~" . "~"))
                          (?= . ("=" . "="))
                          (?* . ("*" . "*"))
                          (?_ . ("_" . "_"))
                          (?/ . ("/" . "/"))
                          (?| . ("|" . "|")))))
  
  ;; Programming language specific surrounds
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
  
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (push '(?m . ("\\(" . "\\)")) evil-surround-pairs-alist)
              (push '(?M . ("\\[" . "\\]")) evil-surround-pairs-alist))))

;;; Evil Numbers - Advanced number manipulation
(use-package evil-numbers
  :after evil
  :config
  ;; Enhanced number recognition
  (setq evil-numbers-use-cursor-at-end-of-number t)
  (setq evil-numbers-pad-default nil)
  (setq evil-numbers-separator-chars ".,")
  
  ;; Key bindings
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-visual-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))

;;; Evil Exchange - Text exchange operations
(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install)
  
  ;; Visual exchange enhancements
  (setq evil-exchange-key (kbd "gx"))
  (setq evil-exchange-cancel-key (kbd "gX")))

;;; Evil Matchit - Enhanced bracket matching
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1)
  
  ;; Enhanced matching rules (removed invalid evilmi-shortcut-chars)
  (setq evilmi-may-jump-by-percentage nil))

;;; Evil Args - Function argument manipulation
(use-package evil-args
  :after evil
  :config
  ;; Enhanced argument detection
  (setq evil-args-closers '(")" "]" "}"))
  (setq evil-args-openers '("(" "[" "{"))
  (setq evil-args-delimiters '("," ";"))
  
  ;; Key bindings
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg))

;;; Evil Indent Plus - Indentation text objects
(use-package evil-indent-plus
  :after evil
  :config
  (evil-indent-plus-default-bindings)
  
  ;; Enhanced indentation behavior
  (setq evil-indent-plus-modes
        '(python-mode yaml-mode haskell-mode nim-mode)))

;;; Evil Visualstar - Visual selection search
(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode)
  
  ;; Enhanced visual search
  (setq evil-visualstar/persistent nil))

;;; Evil Lion - Alignment operator
(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode)
  
  ;; Custom alignment rules
  (setq evil-lion-squeeze-spaces t))

;;; Evil Quickscope - f/F/t/T enhancement
(use-package evil-quickscope
  :after evil
  :config
  (global-evil-quickscope-mode 1)
  
  ;; Quickscope configuration
  (setq evil-quickscope-cross-lines t)
  (setq evil-quickscope-bidirectional t))

;;; Evil Snipe - Enhanced f/F/t/T with 2-char search
(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  
  ;; Snipe configuration
  (setq evil-snipe-scope 'visible)
  (setq evil-snipe-repeat-scope 'whole-visible)
  (setq evil-snipe-char-fold t)
  (setq evil-snipe-smart-case t)
  (setq evil-snipe-use-vim-sneak-bindings t))

;;; Evil Escape - Better escape sequences
(use-package evil-escape
  :after evil
  :config
  (evil-escape-mode 1)
  
  ;; Escape configuration
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion))
  (setq evil-escape-excluded-major-modes '(neotree-mode treemacs-mode)))

;;; Evil Goggles - Visual feedback for operations
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  
  ;; Goggles configuration
  (setq evil-goggles-duration 0.200)
  (setq evil-goggles-pulse t)
  (setq evil-goggles-enable-delete t)
  (setq evil-goggles-enable-indent t)
  (setq evil-goggles-enable-yank t)
  (setq evil-goggles-enable-join t)
  (setq evil-goggles-enable-fill-and-move t)
  (setq evil-goggles-enable-paste t)
  (setq evil-goggles-enable-shift t)
  (setq evil-goggles-enable-surround t)
  (setq evil-goggles-enable-commentary t)
  (setq evil-goggles-enable-nerd-commenter t)
  (setq evil-goggles-enable-replace-with-register t)
  (setq evil-goggles-enable-set-marker t))

;;; Advanced Evil Text Objects
(defmacro define-evil-text-object (key start-regex end-regex)
  "Define inner and outer text objects for KEY with START-REGEX and END-REGEX."
  (let ((inner-name (make-symbol (concat "evil-inner-" key)))
        (outer-name (make-symbol (concat "evil-outer-" key))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;; Enhanced text objects
(define-evil-text-object "l" "^\\s-*" "$")                    ; line
(define-evil-text-object "e" "\\\\begin{.*}" "\\\\end{.*}")  ; LaTeX environment
(define-evil-text-object "f" "\\w\\(\\w\\|\\s_\\)*(" ")")    ; function call
(define-evil-text-object "h" "<[^/>]*>" "</[^>]*>")          ; HTML/XML tag
(define-evil-text-object "/" "/" "/")                        ; forward slashes
(define-evil-text-object "|" "|" "|")                        ; pipes
(define-evil-text-object "=" "=" "=")                        ; equals
(define-evil-text-object "_" "_" "_")                        ; underscores
(define-evil-text-object "*" "\\*" "\\*")                    ; asterisks

;;; Advanced Evil Operators
(evil-define-operator evil-fill-paragraph (beg end)
  "Fill text from BEG to END."
  :move-point nil
  (save-excursion
    (condition-case nil
        (fill-region beg end)
      (error nil))))

(evil-define-operator evil-sort-lines (beg end)
  "Sort lines from BEG to END."
  :move-point nil
  :type line
  (sort-lines nil beg end))

(evil-define-operator evil-downcase (beg end type)
  "Downcase text from BEG to END."
  (if (eq type 'block)
      (evil-apply-on-block #'downcase-region beg end nil)
    (downcase-region beg end)))

(evil-define-operator evil-upcase (beg end type)
  "Upcase text from BEG to END."
  (if (eq type 'block)
      (evil-apply-on-block #'upcase-region beg end nil)
    (upcase-region beg end)))

;; Key bindings for custom operators
(define-key evil-normal-state-map "gq" 'evil-fill-paragraph)
(define-key evil-normal-state-map "gs" 'evil-sort-lines)
(define-key evil-normal-state-map "gu" 'evil-downcase)
(define-key evil-normal-state-map "gU" 'evil-upcase)

;;; Advanced Evil Commands
(evil-define-command evil-write-all ()
  "Save all modified buffers."
  (save-some-buffers t))

(evil-define-command evil-quit-all-force ()
  "Force quit all buffers."
  (kill-emacs))

(evil-define-command evil-buffer-delete ()
  "Delete current buffer."
  (kill-buffer))

(evil-define-command evil-buffer-delete-force ()
  "Force delete current buffer."
  (kill-buffer (current-buffer)))

;;; Evil Ex Command Aliases
(evil-ex-define-cmd "wa[ll]" 'evil-write-all)
(evil-ex-define-cmd "qa[ll]!" 'evil-quit-all-force)
(evil-ex-define-cmd "bd[elete]" 'evil-buffer-delete)
(evil-ex-define-cmd "bd[elete]!" 'evil-buffer-delete-force)
(evil-ex-define-cmd "W" 'evil-write)
(evil-ex-define-cmd "Q" 'evil-quit)
(evil-ex-define-cmd "Wq" 'evil-save-and-close)
(evil-ex-define-cmd "WQ" 'evil-save-and-close)

;;; Advanced Motion Configuration
;; Enhanced paragraph motions
(setq paragraph-start "\\|[ \t]*$")
(setq paragraph-separate "[ \t\f]*$\\|[ \t]*$")

;; Better sentence motions  
(setq sentence-end-double-space nil)

;;; Search and Replace Enhancements
;; Better search defaults
(setq evil-symbol-word-search t)
(setq evil-ex-search-highlight-all t)
(setq evil-ex-substitute-highlight-all t)
(setq evil-ex-substitute-global t)

;; Enhanced replace behavior
(setq evil-ex-substitute-case 'smart)

;;; Evil Integration Hooks
;; Automatic visual line mode for long lines
(add-hook 'evil-local-mode-hook
          (lambda ()
            (when (and (bound-and-true-p evil-local-mode)
                       (not (bound-and-true-p visual-line-mode))
                       (> (buffer-size) 1000))
              (visual-line-mode 1))))

;; Better evil integration with common modes
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local evil-shift-width tab-width)))

;; Enhanced evil for text modes
(add-hook 'text-mode-hook
          (lambda ()
            (setq-local evil-auto-indent nil)))

;;; Performance Optimizations
;; Reduce command logging for performance
(setq evil-command-window-height 4)

;; Optimize evil for large buffers
(setq evil-bigword "^ \t\r\n")
(setq evil-word "^ \t\r\n")

;; Better undo granularity
(setq evil-want-fine-undo t)

;;; Custom Doom Modeline Integration for Evil States
(defun my/doom-modeline-evil-state-color ()
  "Return color for current Evil state."
  (cond
   ((evil-normal-state-p) "#7aa2f7")   ; Tokyo Night blue
   ((evil-insert-state-p) "#9ece6a")   ; Tokyo Night green
   ((evil-visual-state-p) "#bb9af7")   ; Tokyo Night purple
   ((evil-replace-state-p) "#f7768e")  ; Tokyo Night red
   ((evil-operator-state-p) "#e0af68") ; Tokyo Night yellow
   ((evil-motion-state-p) "#7dcfff")   ; Tokyo Night cyan
   ((evil-emacs-state-p) "#73daca")    ; Tokyo Night teal
   (t "#c0caf5")))                     ; Tokyo Night foreground

(defun my/doom-modeline-evil-state-icon ()
  "Return icon for current Evil state."
  (cond
   ((evil-normal-state-p) "󰕷")
   ((evil-insert-state-p) "󰙞")
   ((evil-visual-state-p) "󰈊")
   ((evil-replace-state-p) "󰯸")
   ((evil-operator-state-p) "󰛂")
   ((evil-motion-state-p) "󰺅")
   ((evil-emacs-state-p) "󰰤")
   (t "󰠠")))

;; Custom modeline segment for Evil state
(doom-modeline-def-segment my-evil-state
  "The current evil state. Requires `evil-mode' to be enabled."
  (when (bound-and-true-p evil-local-mode)
    (let ((state (my/doom-modeline-evil-state-icon))
          (color (my/doom-modeline-evil-state-color)))
      (propertize (concat " " state " ")
                  'face `(:foreground ,color :weight bold)))))

;; Add evil state to modeline format (you may need to customize this based on your modeline setup)
(setq doom-modeline-format
      '(:eval
        (doom-modeline-format--main)))

;;; Additional Doom Theme Integration
;; Custom faces for better Evil integration with Tokyo Night
(with-eval-after-load 'doom-themes
  (custom-set-faces
   '(evil-goggles-default-face ((t (:inherit region :background "#364A82"))))
   '(evil-goggles-delete-face ((t (:inherit region :background "#914c54"))))
   '(evil-goggles-paste-face ((t (:inherit region :background "#394b70"))))
   '(evil-goggles-undo-redo-add-face ((t (:inherit region :background "#394b70"))))
   '(evil-goggles-undo-redo-remove-face ((t (:inherit region :background "#914c54"))))
   '(evil-goggles-undo-redo-change-face ((t (:inherit region :background "#364A82"))))
   '(evil-goggles-yank-face ((t (:inherit region :background "#69ff94" :foreground "#16161e"))))
   '(evil-goggles-surround-face ((t (:inherit region :background "#bb9af7" :foreground "#16161e"))))))

;;; Line Numbers Configuration
;; Enable relative line numbers for better Evil navigation
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Disable line numbers in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook
                dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Highlight Line Configuration
;; Enable highlight line mode
(global-hl-line-mode +1)

;;; Better Scrolling
(setq scroll-margin 8
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;;; Final Configuration Messages
(message "Enhanced Evil configuration with Doom Themes loaded successfully!")
(message "Theme: Doom Tokyo Night")
(message "Evil extensions: Collection, Nerd-Commenter, Surround, Numbers, Exchange, Matchit, Args, Indent-Plus, Visualstar, Lion, Quickscope, Snipe, Escape, Goggles")
(message "Custom text objects: l (line), e (LaTeX env), f (function), h (HTML tag), /, |, =, _, *")
(message "Custom operators: gq (fill), gs (sort), gu (downcase), gU (upcase)")
(message "Visual enhancements: Doom modeline with Evil integration, relative line numbers, cursor colors, Evil goggles")

;;; End of Enhanced Evil Configuration

``````
