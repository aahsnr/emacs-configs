``````el

;;; init.el --- Refined Evil Emacs 30 Configuration with Gruvbox Theme
;;; Commentary:
;;; A streamlined and comprehensive Evil mode configuration.
;;; Features evil-nerd-commenter, redundancy removal, and clean keybindings.

;;; Code:

;;; General.el Configuration

;; General - More convenient key definitions
(use-package general
  :config
  (general-evil-setup t)

  ;; Set up leader key
  (general-create-definer my/leader-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Set up local leader key
  (general-create-definer my/local-leader-def
    :keymaps '(normal visual)
    :prefix ","
    :global-prefix "SPC m")

  ;; Set up alternate leader for quick access
  (general-create-definer my/alt-leader-def
    :keymaps '(normal visual)
    :prefix ";"
    :global-prefix "C-;"))

;;; Evil Configuration

;; Evil mode - The extensible vi layer
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-want-Y-yank-to-eol t)
  (evil-mode 1)
  :config
  ;; Evil settings
  (setq evil-respect-visual-line-mode t
        evil-undo-system 'undo-redo
        evil-search-module 'evil-search
        evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-shift-round nil
        evil-want-C-d-scroll t
        evil-want-fine-undo t
        evil-kill-on-visual-paste nil
        evil-move-cursor-back nil
        evil-want-minibuffer-navigation t
        evil-ex-visual-char-range t
        evil-shift-width 2
        evil-symbol-word-search t
        evil-cross-lines t
        evil-auto-indent t
        evil-ex-substitute-global t)

  ;; Enhanced cursor appearance
  (setq evil-normal-state-cursor '(box "#fe8019")
        evil-insert-state-cursor '(bar "#fb4934")
        evil-visual-state-cursor '(hollow "#fe8019")
        evil-replace-state-cursor '(hbar "#fb4934")
        evil-operator-state-cursor '(evil-half-cursor "#fb4934")
        evil-motion-state-cursor '(box "#b8bb26")
        evil-emacs-state-cursor '(hbar "#d3869b"))

  ;; Set initial states for various modes
  (dolist (mode '(messages-buffer-mode dashboard-mode compilation-mode
                  grep-mode occur-mode help-mode Info-mode woman-mode
                  man-mode package-menu-mode))
    (evil-set-initial-state mode 'normal))

  (dolist (mode '(term-mode shell-mode eshell-mode))
    (evil-set-initial-state mode 'emacs))

  ;; Custom functions
  (defun my/save-and-kill-buffer ()
    "Save and kill current buffer."
    (interactive)
    (save-buffer)
    (kill-current-buffer))

  (defun my/evil-scroll-down-center ()
    "Scroll down and center cursor."
    (interactive)
    (evil-scroll-down nil)
    (evil-scroll-line-to-center nil))

  (defun my/evil-scroll-up-center ()
    "Scroll up and center cursor."
    (interactive)
    (evil-scroll-up nil)
    (evil-scroll-line-to-center nil))

  ;; Keybindings via General.el
  (general-define-key
   :states 'normal
   ;; Use visual lines for j/k, logical lines for gj/gk
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line
   "gj" 'evil-next-line
   "gk" 'evil-previous-line
   ;; Centered scrolling
   "C-d" 'my/evil-scroll-down-center
   "C-u" 'my/evil-scroll-up-center

   :states 'insert
   ;; Escape with "jk"
   "jk" 'evil-normal-state)

  ;; Define evil ex commands using general.el
  (general-evil-define-ex
    "q"    'kill-current-buffer
    "quit" 'kill-current-buffer
    "wq"   'my/save-and-kill-buffer
    "x"    'my/save-and-kill-buffer
    "qa"   'save-buffers-kill-terminal
    "wqa"  'save-buffers-kill-terminal
    "xa"   'save-buffers-kill-terminal
    "wa"   'save-some-buffers
    "bd"   'kill-current-buffer
    "bD"   'kill-buffer
    "e!"   'revert-buffer))


;; Evil Collection - Evil bindings for many modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Evil Surround - Surround text objects
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; evil-nerd-commenter - Smart code commenting
(use-package evil-nerd-commenter
  :after evil
  :config
  (general-define-key
   :states '(normal visual)
   "gc" 'evilnc-comment-or-uncomment-lines))


;; Evil Numbers - Increment/decrement numbers
(use-package evil-numbers
  :after evil
  :config
  (general-define-key
   :states '(normal visual)
   "C-a" 'evil-numbers/inc-at-pt
   "C-x" 'evil-numbers/dec-at-pt))

;; Evil Args - Motions and text objects for arguments
(use-package evil-args
  :after evil
  :config
  (general-define-key
    :keymaps 'evil-inner-text-objects-map
    "a" 'evil-inner-arg
    :keymaps 'evil-outer-text-objects-map
    "a" 'evil-outer-arg
    :states 'normal
    "]a" 'evil-forward-arg
    "[a" 'evil-backward-arg))

;; Evil Exchange - Exchange text regions
(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))

;; Evil Indent Plus - Indent text objects
(use-package evil-indent-plus
  :after evil
  :config
  (evil-indent-plus-default-bindings))

;; Evil Visualstar - Search for selected text
(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode))

;; Evil Matchit - Jump between matching tags/parentheses
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; Evil Snipe - Enhanced f/t motions
(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  (setq evil-snipe-smart-case t))

;; Evil Lion - Align text
(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))

;; Evil Multiedit - Multiple cursors for Evil
(use-package evil-multiedit
  :after evil
  :config
  (evil-multiedit-default-keybinds))

;; Evil Goggles - Visual feedback for Evil operations
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.1))

;;; Leader Keybindings

(my/leader-def
  ;; Evil-specific operations
  "e"   '(:ignore t :which-key "evil")
  "eh"  '(evil-ex-nohighlight :which-key "clear search highlight")
  "er"  '(evil-show-registers :which-key "show registers")
  "em"  '(evil-show-marks :which-key "show marks")
  "ej"  '(evil-join :which-key "join lines")
  "ex"  '(evil-exchange :which-key "exchange")
  "eX"  '(evil-exchange-cancel :which-key "cancel exchange")

  ;; Buffer operations
  "b"   '(:ignore t :which-key "buffers")
  "bb"  '(switch-to-buffer :which-key "switch buffer")
  "bd"  '(kill-current-buffer :which-key "delete buffer")
  "br"  '(revert-buffer :which-key "revert buffer")
  "bn"  '(next-buffer :which-key "next buffer")
  "bp"  '(previous-buffer :which-key "previous buffer")
  "bs"  '(save-buffer :which-key "save buffer")

  ;; Window operations
  "w"   '(:ignore t :which-key "windows")
  "wh"  '(evil-window-left :which-key "window left")
  "wj"  '(evil-window-down :which-key "window down")
  "wk"  '(evil-window-up :which-key "window up")
  "wl"  '(evil-window-right :which-key "window right")
  "ws"  '(evil-window-split :which-key "split below")
  "wv"  '(evil-window-vsplit :which-key "split right")
  "wd"  '(evil-window-delete :which-key "delete window")
  "wo"  '(delete-other-windows :which-key "delete other windows")
  "w="  '(balance-windows :which-key "balance windows")
  "wx"  '(evil-window-exchange :which-key "exchange windows")

  ;; Search operations
  "s"   '(:ignore t :which-key "search")
  "ss"  '(evil-ex-search-forward :which-key "search forward")
  "sS"  '(evil-ex-search-backward :which-key "search backward")
  "sr"  '(query-replace :which-key "replace")
  "sR"  '(query-replace-regexp :which-key "replace regexp")

  ;; File operations
  "f"   '(:ignore t :which-key "files")
  "ff"  '(find-file :which-key "find file")
  "fs"  '(save-buffer :which-key "save file")
  "fr"  '(recentf-open-files :which-key "recent files")
  "fd"  '(dired :which-key "dired")

  ;; Toggle operations
  "t"   '(:ignore t :which-key "toggles")
  "tn"  '(display-line-numbers-mode :which-key "line numbers")
  "tw"  '(whitespace-mode :which-key "whitespace")
  "th"  '(hl-line-mode :which-key "highlight line")
  "tt"  '(toggle-truncate-lines :which-key "truncate lines")
  "tv"  '(visual-line-mode :which-key "visual line mode"))

(provide 'init)
;;; init.el ends here


``````
