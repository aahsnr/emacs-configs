1. Catppuccin Theme InstallationHere is the use-package configuration to install and set up the Catppuccin theme. You can add this to your Emacs configuration file, replacing or commenting out your existing theme (doom-themes in your config).

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha) ; or 'macchiato, 'frappe, 'latte
  (load-theme 'catppuccin t))

2. Hex Code Color ChangesBelow are the sections from your configuration that contain hardcoded Gruvbox color values. I've provided the original Gruvbox version and the updated Catppuccin Mocha version for each.diredFrom (Gruvbox):(custom-set-faces
 '(dired-directory ((t (:foreground "#83a598" :weight bold))))
 '(dired-header ((t (:foreground "#fabd2f" :weight bold))))
 '(dired-symlink ((t (:foreground "#8ec07c"))))
 '(dired-marked ((t (:foreground "#fb4934" :weight bold))))
 '(dired-flagged ((t (:foreground "#fb4934" :background "#3c3836"))))
 '(dired-warning ((t (:foreground "#fe8019" :weight bold))))
 '(dired-perm-write ((t (:foreground "#b8bb26"))))
 '(dired-special ((t (:foreground "#d3869b"))))
 '(dired-ignored ((t (:foreground "#928374"))))))

To (Catppuccin Mocha):(custom-set-faces
 '(dired-directory ((t (:foreground "#89B4FA" :weight bold))))
 '(dired-header ((t (:foreground "#F9E2AF" :weight bold))))
 '(dired-symlink ((t (:foreground "#94E2D5"))))
 '(dired-marked ((t (:foreground "#F38BA8" :weight bold))))
 '(dired-flagged ((t (:foreground "#F38BA8" :background "#181825"))))
 '(dired-warning ((t (:foreground "#FAB387" :weight bold))))
 '(dired-perm-write ((t (:foreground "#A6E3A1"))))
 '(dired-special ((t (:foreground "#CBA6F7"))))
 '(dired-ignored ((t (:foreground "#7F849C"))))))

dirvishFrom (Gruvbox):(custom-set-faces
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

To (Catppuccin Mocha):(custom-set-faces
 '(dirvish-hl-line ((t (:background "#181825"))))
 '(dirvish-emerge-group-title ((t (:foreground "#F9E2AF" :weight bold))))
 '(dirvish-emerge-group-separator ((t (:foreground "#7F849C"))))
 '(dirvish-git-commit-message ((t (:foreground "#BAC2DE"))))
 '(dirvish-git-commit-author ((t (:foreground "#89B4FA"))))
 '(dirvish-subtree-guide ((t (:foreground "#7F849C"))))
 '(dirvish-path-separator ((t (:foreground "#7F849C"))))
 '(dirvish-free-space ((t (:foreground "#94E2D5"))))
 '(dirvish-yank-line ((t (:background "#313244"))))
 '(dirvish-index-number ((t (:foreground "#FAB387"))))
 '(dirvish-sort-indicator ((t (:foreground "#A6E3A1"))))
 '(dirvish-file-size ((t (:foreground "#A6ADC8"))))
 '(dirvish-file-time ((t (:foreground "#A6ADC8")))))

dired-subtreeFrom (Gruvbox):(custom-set-faces
 '(dired-subtree-depth-1-face ((t (:background "#282828"))))
 '(dired-subtree-depth-2-face ((t (:background "#3c3836"))))
 '(dired-subtree-depth-3-face ((t (:background "#282828"))))
 '(dired-subtree-depth-4-face ((t (:background "#3c3836"))))
 '(dired-subtree-depth-5-face ((t (:background "#282828"))))
 '(dired-subtree-depth-6-face ((t (:background "#3c3836"))))))

To (Catppuccin Mocha):(custom-set-faces
 '(dired-subtree-depth-1-face ((t (:background "#1E1E2E"))))
 '(dired-subtree-depth-2-face ((t (:background "#181825"))))
 '(dired-subtree-depth-3-face ((t (:background "#1E1E2E"))))
 '(dired-subtree-depth-4-face ((t (:background "#181825"))))
 '(dired-subtree-depth-5-face ((t (:background "#1E1E2E"))))
 '(dired-subtree-depth-6-face ((t (:background "#181825"))))))

evil (Cursor Colors)From (Gruvbox):(setq evil-normal-state-cursor '(box "#fe8019")
      evil-insert-state-cursor '(bar "#fb4934")
      evil-visual-state-cursor '(hollow "#fe8019")
      evil-replace-state-cursor '(hbar "#fb4934")
      evil-operator-state-cursor '(evil-half-cursor "#fb4934")
      evil-motion-state-cursor '(box "#b8bb26")
      evil-emacs-state-cursor '(hbar "#d3869b"))

To (Catppuccin Mocha):(setq evil-normal-state-cursor '(box "#FAB387")
      evil-insert-state-cursor '(bar "#F38BA8")
      evil-visual-state-cursor '(hollow "#FAB387")
      evil-replace-state-cursor '(hbar "#F38BA8")
      evil-operator-state-cursor '(evil-half-cursor "#F38BA8")
      evil-motion-state-cursor '(box "#A6E3A1")
      evil-emacs-state-cursor '(hbar "#CBA6F7"))

verticoFrom (Gruvbox):(custom-set-faces
 '(vertico-current ((t (:background "#504945" :foreground "#ebdbb2" :weight bold))))
 '(vertico-group-title ((t (:foreground "#d3869b" :weight bold))))
 '(vertico-group-separator ((t (:foreground "#7c6f64"))))
 '(vertico-multiline ((t (:foreground "#83a598"))))))

To (Catppuccin Mocha):(custom-set-faces
 '(vertico-current ((t (:background "#313244" :foreground "#CDD6F4" :weight bold))))
 '(vertico-group-title ((t (:foreground "#CBA6F7" :weight bold))))
 '(vertico-group-separator ((t (:foreground "#6C7086"))))
 '(vertico-multiline ((t (:foreground "#89B4FA"))))))

corfuFrom (Gruvbox):(custom-set-faces
 '(corfu-default ((t (:background "#282828" :foreground "#ebdbb2"))))
 '(corfu-current ((t (:background "#504945" :foreground "#ebdbb2" :weight bold))))
 '(corfu-bar ((t (:background "#b16286"))))
 '(corfu-border ((t (:background "#7c6f64"))))
 '(corfu-annotations ((t (:foreground "#a89984" :italic t))))
 '(corfu-deprecated ((t (:foreground "#7c6f64" :strike-through t)))))

To (Catppuccin Mocha):(custom-set-faces
 '(corfu-default ((t (:background "#1E1E2E" :foreground "#CDD6F4"))))
 '(corfu-current ((t (:background "#313244" :foreground "#CDD6F4" :weight bold))))
 '(corfu-bar ((t (:background "#CBA6F7"))))
 '(corfu-border ((t (:background "#6C7086"))))
 '(corfu-annotations ((t (:foreground "#A6ADC8" :italic t))))
 '(corfu-deprecated ((t (:foreground "#6C7086" :strike-through t)))))

marginalia**From (Gruvbox):**to(custom-set-faces
 '(marginalia-file-name ((t (:foreground "#ebdbb2"))))
 '(marginalia-file-owner ((t (:foreground "#d3869b"))))
 ... and so on

To (Catppuccin Mocha):(custom-set-faces
 '(marginalia-archive ((t (:foreground "#A6E3A1"))))
 '(marginalia-char ((t (:foreground "#FAB387"))))
 '(marginalia-date ((t (:foreground "#89B4FA"))))
 '(marginalia-documentation ((t (:foreground "#A6ADC8" :italic t))))
 '(marginalia-file-name ((t (:foreground "#CDD6F4"))))
 '(marginalia-file-owner ((t (:foreground "#CBA6F7"))))
 '(marginalia-file-priv-dir ((t (:foreground "#89B4FA"))))
 '(marginalia-file-priv-exec ((t (:foreground "#A6E3A1"))))
 '(marginalia-file-priv-link ((t (:foreground "#94E2D5"))))
 '(marginalia-file-priv-read ((t (:foreground "#FAB387"))))
 '(marginalia-file-priv-write ((t (:foreground "#F38BA8"))))
 '(marginalia-function ((t (:foreground "#89B4FA"))))
 '(marginalia-key ((t (:foreground "#FAB387"))))
 '(marginalia-lighter ((t (:foreground "#6C7086"))))
 '(marginalia-list ((t (:foreground "#94E2D5"))))
 '(marginalia-mode ((t (:foreground "#CBA6F7"))))
 '(marginalia-modified ((t (:foreground "#F9E2AF"))))
 '(marginalia-null ((t (:foreground "#6C7086"))))
 '(marginalia-number ((t (:foreground "#FAB387"))))
 '(marginalia-size ((t (:foreground "#A6E3A1"))))
 '(marginalia-string ((t (:foreground "#A6E3A1"))))
 '(marginalia-symbol ((t (:foreground "#CBA6F7"))))
 '(marginalia-true ((t (:foreground "#A6E3A1"))))
 '(marginalia-type ((t (:foreground "#89B4FA"))))
 '(marginalia-value ((t (:foreground "#CDD6F4"))))
 '(marginalia-variable ((t (:foreground "#94E2D5"))))
 '(marginalia-version ((t (:foreground "#A6E3A1"))))))

orderlessFrom (Gruvbox):(custom-set-faces
 '(orderless-match-face-0 ((t (:foreground "#d3869b" :weight bold))))
 '(orderless-match-face-1 ((t (:foreground "#83a598" :weight bold))))
 '(orderless-match-face-2 ((t (:foreground "#b8bb26" :weight bold))))
 '(orderless-match-face-3 ((t (:foreground "#fabd2f" :weight bold))))))

To (Catppuccin Mocha):(custom-set-faces
 '(orderless-match-face-0 ((t (:foreground "#CBA6F7" :weight bold))))
 '(orderless-match-face-1 ((t (:foreground "#89B4FA" :weight bold))))
 '(orderless-match-face-2 ((t (:foreground "#A6E3A1" :weight bold))))
 '(orderless-match-face-3 ((t (:foreground "#F9E2AF" :weight bold))))))

3. Solaire Mode IntegrationThe Catppuccin theme has excellent built-in support for solaire-mode. When you load the Catppuccin theme, it automatically detects if solaire-mode is active and applies the appropriate alternative background color for pop-ups and sidebars.Your existing solaire-mode configuration does not need to be changed. Just ensure (solaire-global-mode +1) is called before you load the Catppuccin theme to ensure the integration works correctly on startup.The structure should be:(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1)
  ;; ... your other solaire-mode configs
  )

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin t))

This setup will give you a consistent and beautiful Catppuccin Mocha look across your entire Emacs environment, including the subtle background variations provided by solaire-mode.
