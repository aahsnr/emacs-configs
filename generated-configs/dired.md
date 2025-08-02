``````el
;; Enhanced dired configuration - must be loaded before dirvish
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-hide-details-hide-symlink-targets nil)
  (dired-hide-details-hide-information-lines nil)
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t)
  (dired-do-revert-buffer t)
  (delete-by-moving-to-trash t)
  :config
  ;; Gruvbox-themed dired faces with direct hex codes
  (custom-set-faces
   '(dired-directory ((t (:foreground "#83a598" :weight bold))))
   '(dired-header ((t (:foreground "#fabd2f" :weight bold))))
   '(dired-symlink ((t (:foreground "#8ec07c"))))
   '(dired-marked ((t (:foreground "#fb4934" :weight bold))))
   '(dired-flagged ((t (:foreground "#fb4934" :background "#3c3836"))))
   '(dired-warning ((t (:foreground "#fe8019" :weight bold))))
   '(dired-perm-write ((t (:foreground "#b8bb26"))))
   '(dired-special ((t (:foreground "#d3869b"))))
   '(dired-ignored ((t (:foreground "#928374")))))

  ;; Dired keybindings
  (global-leader-key
    "d"   '(:ignore t :which-key "dired")
    "dd"  '(dired :which-key "dired")
    "dj"  '(dired-jump :which-key "dired jump")
    "dD"  '(dired-other-window :which-key "dired other window")
    "dJ"  '(dired-jump-other-window :which-key "dired jump other window"))

  ;; Dired-specific keybindings
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "h" 'dired-up-directory
   "l" 'dired-find-file
   "j" 'dired-next-line
   "k" 'dired-previous-line
   "gg" 'beginning-of-buffer
   "G" 'end-of-buffer
   "q" 'quit-window
   "R" 'dired-do-rename
   "D" 'dired-do-delete
   "C" 'dired-do-copy
   "+" 'dired-create-directory
   "m" 'dired-mark
   "u" 'dired-unmark
   "U" 'dired-unmark-all-marks
   "t" 'dired-toggle-marks
   "%" 'dired-mark-files-regexp
   "s" 'dired-sort-toggle-or-edit
   "gr" 'revert-buffer))

;; Dired-x for additional functionality - must be loaded before dirvish
(use-package dired-x
  :ensure nil
  :after dired
  :custom
  (dired-x-hands-off-my-keys nil)
  :config
  ;; Define dired-omit-files to prevent void-variable errors
  (setq dired-omit-files "^\\.[^.]\\|^#\\|^\\.$\\|^\\.\\.$\\|\\.pyc$\\|\\.o$")
  (setq dired-omit-verbose nil)
  
  ;; Additional dired-x keybindings
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "o" 'dired-omit-mode
   "(" 'dired-hide-details-mode))

;; Nerd-icons-dired for better icons - load before dirvish
(use-package nerd-icons-dired
  :after (dired nerd-icons)
  :hook (dired-mode . nerd-icons-dired-mode)
  :config
  ;; Gruvbox-themed nerd-icons-dired faces with direct hex codes
  (custom-set-faces
   '(nerd-icons-dired-dir-face ((t (:foreground "#83a598" :weight bold))))
   '(nerd-icons-dired-file-face ((t (:foreground "#ebdbb2"))))
   '(nerd-icons-dired-symlink-face ((t (:foreground "#8ec07c"))))
   '(nerd-icons-dired-executable-face ((t (:foreground "#b8bb26"))))
   '(nerd-icons-dired-compressed-face ((t (:foreground "#fabd2f"))))
   '(nerd-icons-dired-audio-face ((t (:foreground "#d3869b"))))
   '(nerd-icons-dired-video-face ((t (:foreground "#fb4934"))))
   '(nerd-icons-dired-image-face ((t (:foreground "#fe8019"))))))

;; Dirvish - Modern dired interface
(use-package dirvish
  :after (dired dired-x nerd-icons-dired)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/" "Home")
     ("d" "~/Downloads/" "Downloads")
     ("D" "~/Documents/" "Documents")
     ("p" "~/Projects/" "Projects")
     ("/" "/" "Root")))
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))
  (dirvish-attributes
   '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (dirvish-subtree-state-style 'nerd)
  (dirvish-path-separators (list "  " "  " "  "))
  (dirvish-header-line-format
   '(:left (path) :right (free-space)))
  (dirvish-use-header-line t)
  (dirvish-use-mode-line t)
  :config
  ;; Gruvbox-themed dirvish faces with direct hex codes
  (custom-set-faces
   '(dirvish-hl-line ((t (:background "#3c3836"))))
   '(dirvish-emerge-group-title ((t (:foreground "#fabd2f" :weight bold))))
   '(dirvish-emerge-group-separator ((t (:foreground "#928374"))))
   '(dirvish-git-commit-message ((t (:foreground "#bdae93"))))
   '(dirvish-git-commit-author ((t (:foreground "#83a598"))))
   '(dirvish-subtree-guide ((t (:foreground "#928374"))))
   '(dirvish-path-separator ((t (:foreground "#928374"))))
   '(dirvish-free-space ((t (:foreground "#8ec07c"))))
   '(dirvish-yank-line ((t (:background "#504945"))))
   '(dirvish-index-number ((t (:foreground "#fe8019"))))
   '(dirvish-sort-indicator ((t (:foreground "#b8bb26"))))
   '(dirvish-file-size ((t (:foreground "#a89984"))))
   '(dirvish-file-time ((t (:foreground "#a89984")))))

  ;; Dirvish keybindings
  (global-leader-key
    "df"  '(dirvish-fd :which-key "find file")
    "ds"  '(dirvish-side :which-key "side panel")
    "dq"  '(dirvish-quicksort :which-key "quicksort")
    "dy"  '(dirvish-yank :which-key "yank")
    "dY"  '(dirvish-yank-menu :which-key "yank menu")
    "dt"  '(dirvish-subtree-toggle :which-key "toggle subtree")
    "dT"  '(dirvish-subtree-remove :which-key "remove subtree")
    "de"  '(dirvish-emerge-menu :which-key "emerge menu")
    "dm"  '(dirvish-mark-menu :which-key "mark menu")
    "dc"  '(dirvish-copy-menu :which-key "copy menu")
    "dr"  '(dirvish-rename-menu :which-key "rename menu")
    "dD"  '(dirvish-delete-menu :which-key "delete menu")
    "dz"  '(dirvish-zip :which-key "zip")
    "dZ"  '(dirvish-unzip :which-key "unzip")
    "dg"  '(dirvish-goto-menu :which-key "goto menu")
    "dG"  '(dirvish-layout-toggle :which-key "layout toggle")
    "dv"  '(dirvish-vc-menu :which-key "vc menu")
    "dH"  '(dirvish-history-menu :which-key "history menu")
    "dA"  '(dirvish-quick-access :which-key "quick access")
    "dL"  '(dirvish-ls-switches-menu :which-key "ls switches")
    "do"  '(dired-omit-mode :which-key "toggle omit mode")
    "dh"  '(dired-hide-details-mode :which-key "toggle details"))

  ;; Dirvish-specific keybindings
  (general-define-key
   :keymaps 'dirvish-mode-map
   :states 'normal
   "TAB" 'dirvish-subtree-toggle
   "S-TAB" 'dirvish-subtree-remove
   "M-j" 'dirvish-fd-jump
   "M-s" 'dirvish-quicksort
   "M-y" 'dirvish-yank-menu
   "M-e" 'dirvish-emerge-menu
   "M-m" 'dirvish-mark-menu
   "M-c" 'dirvish-copy-menu
   "M-r" 'dirvish-rename-menu
   "M-d" 'dirvish-delete-menu
   "M-g" 'dirvish-goto-menu
   "M-v" 'dirvish-vc-menu
   "M-h" 'dirvish-history-menu
   "M-a" 'dirvish-quick-access
   "M-l" 'dirvish-ls-switches-menu
   "f" 'dirvish-file-info-menu
   "F" 'dirvish-toggle-fullscreen
   "y" 'dirvish-yank
   "Y" 'dirvish-yank-menu
   "p" 'dirvish-paste
   "P" 'dirvish-paste-menu
   "z" 'dirvish-zip
   "Z" 'dirvish-unzip
   "v" 'dirvish-vc-menu
   "?" 'dirvish-dispatch
   "SPC" 'dirvish-show-history
   "RET" 'dirvish-find-file-true)

  ;; Initialize dirvish
  (dirvish-override-dired-mode))

;; Dired-subtree for collapsible directory trees
(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  (dired-subtree-line-prefix "  ")
  :config
  ;; Gruvbox-themed dired-subtree
  (custom-set-faces
   '(dired-subtree-depth-1-face ((t (:background "#282828"))))
   '(dired-subtree-depth-2-face ((t (:background "#3c3836"))))
   '(dired-subtree-depth-3-face ((t (:background "#282828"))))
   '(dired-subtree-depth-4-face ((t (:background "#3c3836"))))
   '(dired-subtree-depth-5-face ((t (:background "#282828"))))
   '(dired-subtree-depth-6-face ((t (:background "#3c3836")))))

  ;; Dired-subtree keybindings
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "TAB" 'dired-subtree-toggle
   "S-TAB" 'dired-subtree-cycle
   "C-TAB" 'dired-subtree-remove))

;; Dired-narrow for filtering
(use-package dired-narrow
  :ensure t
  :after dired
  :config
  ;; Gruvbox-themed dired-narrow
  (custom-set-faces
   '(dired-narrow-blink ((t (:foreground "#fabd2f" :background "#504945"))))
   '(dired-narrow-rejected ((t (:foreground "#928374" :strike-through t))))
   '(dired-narrow-match ((t (:foreground "#b8bb26" :weight bold)))))

  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "/" 'dired-narrow-fuzzy
   "n" 'dired-narrow
   "N" 'dired-narrow-regexp))

;; Dired-ranger for copy/move operations
(use-package dired-ranger
  :ensure t
  :after dired
  :config
  ;; Dired-ranger keybindings
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "Y" 'dired-ranger-copy
   "X" 'dired-ranger-move
   "P" 'dired-ranger-paste))

;; Dired-collapse for collapsing single-child directories
(use-package dired-collapse
  :ensure t
  :after dired
  :hook (dired-mode . dired-collapse-mode)
  :config
  ;; Gruvbox-themed dired-collapse faces with direct hex codes
  (custom-set-faces
   '(dired-collapse-face ((t (:foreground "#83a598" :weight normal))))))

;; Dired-async for asynchronous operations
(use-package emacs-async
  :ensure t
  :after dired
  :config
  (dired-async-mode 1)
  
  ;; Gruvbox-themed dired-async faces with direct hex codes
  (custom-set-faces
   '(dired-async-mode-message ((t (:foreground "#b8bb26" :weight bold))))
   '(dired-async-failures ((t (:foreground "#fb4934" :weight bold))))
   '(dired-async-message ((t (:foreground "#83a598"))))))

;; Dired-filter for advanced filtering (optional - can be removed if causing issues)
(use-package dired-filter
  :ensure t
  :after dired
  :custom
  (dired-filter-stack nil)
  (dired-filter-verbose nil)
  :config
  ;; Gruvbox-themed dired-filter faces with direct hex codes
  (custom-set-faces
   '(dired-filter-stack ((t (:foreground "#fabd2f"))))
   '(dired-filter-name ((t (:foreground "#83a598"))))
   '(dired-filter-extension ((t (:foreground "#b8bb26"))))
   '(dired-filter-size ((t (:foreground "#fe8019"))))
   '(dired-filter-date ((t (:foreground "#d3869b"))))
   '(dired-filter-mode ((t (:foreground "#8ec07c")))))

  ;; Dired-filter keybindings
  (global-leader-key
    "dn"  '(dired-filter-by-name :which-key "filter by name")
    "dN"  '(dired-filter-by-regexp :which-key "filter by regexp")
    "dx"  '(dired-filter-by-extension :which-key "filter by extension")
    "dX"  '(dired-filter-by-dot-files :which-key "filter dot files")
    "dS"  '(dired-filter-by-size :which-key "filter by size")
    "dT"  '(dired-filter-by-date :which-key "filter by date")
    "dM"  '(dired-filter-by-mode :which-key "filter by mode")
    "dF"  '(dired-filter-by-file :which-key "filter by file")
    "dR"  '(dired-filter-by-directory :which-key "filter by directory")
    "dC"  '(dired-filter-pop :which-key "pop filter")
    "dP"  '(dired-filter-pop-all :which-key "pop all filters")
    "dI"  '(dired-filter-transpose :which-key "transpose filters")
    "dO"  '(dired-filter-save-filters :which-key "save filters")
    "dU"  '(dired-filter-load-saved-filters :which-key "load saved filters")))

;; Global file navigation keybindings
(global-leader-key
  "f"   '(:ignore t :which-key "files")
  "fd"  '(dired :which-key "dired")
  "fj"  '(dired-jump :which-key "dired jump")
  "fD"  '(dired-other-window :which-key "dired other window")
  "fJ"  '(dired-jump-other-window :which-key "dired jump other window"))

;; Auto-refresh dired buffers
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Ensure dired-omit-mode is available after dired-x loads
(with-eval-after-load 'dired-x
  (add-hook 'dired-mode-hook 'dired-omit-mode))

``````
