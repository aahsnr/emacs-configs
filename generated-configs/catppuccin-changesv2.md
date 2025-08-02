
;;;; Catppuccin Theme Installation
;;
;; This section ensures the catppuccin-theme is installed and configured.
;; It sets the theme to the 'mocha' flavor.

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha) ; or 'latte, 'frappe, 'macchiato
  (load-theme 'catppuccin t))

;;;; ------------------------------------------------------------------
;;;; Hex Code Changes from Gruvbox to Catppuccin Mocha
;;;;
;;;; The following sections replace the hardcoded Gruvbox hex codes
;;;; in your configuration with their Catppuccin Mocha equivalents.
;;;; ------------------------------------------------------------------

;;; Catppuccin Palette (Mocha) for Reference:
;;; Base: #1e1e2e
;;; Mantle: #181825
;;; Crust: #11111b
;;; Text: #cdd6f4
;;; Subtext0: #a6adc8
;;; Overlay2: #9399b2
;;; Surface0: #313244
;;; Surface1: #45475a
;;; Red: #f38ba8
;;; Peach: #fab387
;;; Yellow: #f9e2af
;;; Green: #a6e3a1
;;; Teal: #94e2d5
;;; Blue: #89b4fa
;;; Mauve: #cba6f7
;;; Pink: #f5c2e7
;;; Lavender: #b4befe

;;; 1. Dired Faces
(use-package dired
  :ensure nil
  :config
  ;; Catppuccin-themed dired faces with direct hex codes
  (custom-set-faces
   '(dired-directory ((t (:foreground "#89b4fa" :weight bold))))  ; Blue
   '(dired-header ((t (:foreground "#f9e2af" :weight bold))))     ; Yellow
   '(dired-symlink ((t (:foreground "#94e2d5"))))                 ; Teal
   '(dired-marked ((t (:foreground "#f38ba8" :weight bold))))      ; Red
   '(dired-flagged ((t (:foreground "#f38ba8" :background "#313244")))) ; Red on Surface0
   '(dired-warning ((t (:foreground "#fab387" :weight bold))))     ; Peach
   '(dired-perm-write ((t (:foreground "#a6e3a1"))))               ; Green
   '(dired-special ((t (:foreground "#f5c2e7"))))                 ; Pink
   '(dired-ignored ((t (:foreground "#6c7086"))))))               ; Overlay0

;;; 2. Dirvish Faces
(use-package dirvish
  :ensure nil
  :config
  ;; Catppuccin-themed dirvish faces with direct hex codes
  (custom-set-faces
   '(dirvish-hl-line ((t (:background "#313244"))))                     ; Surface0
   '(dirvish-emerge-group-title ((t (:foreground "#f9e2af" :weight bold)))) ; Yellow
   '(dirvish-emerge-group-separator ((t (:foreground "#6c7086"))))       ; Overlay0
   '(dirvish-git-commit-message ((t (:foreground "#cdd6f4"))))           ; Text
   '(dirvish-git-commit-author ((t (:foreground "#89b4fa"))))            ; Blue
   '(dirvish-subtree-guide ((t (:foreground "#6c7086"))))                ; Overlay0
   '(dirvish-path-separator ((t (:foreground "#6c7086"))))               ; Overlay0
   '(dirvish-free-space ((t (:foreground "#a6e3a1"))))                   ; Green
   '(dirvish-yank-line ((t (:background "#45475a"))))                    ; Surface1
   '(dirvish-index-number ((t (:foreground "#fab387"))))                 ; Peach
   '(dirvish-sort-indicator ((t (:foreground "#a6e3a1"))))               ; Green
   '(dirvish-file-size ((t (:foreground "#a6adc8"))))                    ; Subtext0
   '(dirvish-file-time ((t (:foreground "#a6adc8"))))))                  ; Subtext0

;;; 3. Dired-Subtree Faces
(use-package dired-subtree
  :ensure nil
  :config
  ;; Catppuccin-themed dired-subtree
  (custom-set-faces
   '(dired-subtree-depth-1-face ((t (:background "#1e1e2e")))) ; Base
   '(dired-subtree-depth-2-face ((t (:background "#313244")))) ; Surface0
   '(dired-subtree-depth-3-face ((t (:background "#1e1e2e")))) ; Base
   '(dired-subtree-depth-4-face ((t (:background "#313244")))) ; Surface0
   '(dired-subtree-depth-5-face ((t (:background "#1e1e2e")))) ; Base
   '(dired-subtree-depth-6-face ((t (:background "#313244")))))) ; Surface0

;;; 4. Dired-Narrow Faces
(use-package dired-narrow
  :ensure nil
  :config
  ;; Catppuccin-themed dired-narrow
  (custom-set-faces
   '(dired-narrow-blink ((t (:foreground "#f9e2af" :background "#45475a")))) ; Yellow on Surface1
   '(dired-narrow-rejected ((t (:foreground "#6c7086" :strike-through t))))   ; Overlay0
   '(dired-narrow-match ((t (:foreground "#a6e3a1" :weight bold))))))         ; Green

;;; 5. Dired-Collapse Faces
(use-package dired-collapse
  :ensure nil
  :config
  ;; Catppuccin-themed dired-collapse faces with direct hex codes
  (custom-set-faces
   '(dired-collapse-face ((t (:foreground "#89b4fa" :weight normal)))))) ; Blue

;;; 6. Evil Cursor Faces
(use-package evil
  :ensure nil
  :config
  (setq evil-normal-state-cursor '(box "#fab387")      ; Peach
        evil-insert-state-cursor '(bar "#f38ba8")      ; Red
        evil-visual-state-cursor '(hollow "#fab387")   ; Peach
        evil-replace-state-cursor '(hbar "#f38ba8")    ; Red
        evil-operator-state-cursor '(evil-half-cursor "#f38ba8") ; Red
        evil-motion-state-cursor '(box "#a6e3a1")      ; Green
        evil-emacs-state-cursor '(hbar "#cba6f7")))    ; Mauve

;;; 7. Vertico Faces
(use-package vertico
  :ensure nil
  :config
  (custom-set-faces
   '(vertico-current ((t (:background "#45475a" :foreground "#cdd6f4" :weight bold)))) ; Text on Surface1
   '(vertico-group-title ((t (:foreground "#cba6f7" :weight bold))))  ; Mauve
   '(vertico-group-separator ((t (:foreground "#6c7086"))))           ; Overlay0
   '(vertico-multiline ((t (:foreground "#89b4fa"))))))               ; Blue

;;; 8. Corfu Faces
(use-package corfu
  :ensure nil
  :config
  (custom-set-faces
   '(corfu-default ((t (:background "#1e1e2e" :foreground "#cdd6f4"))))     ; Text on Base
   '(corfu-current ((t (:background "#45475a" :foreground "#cdd6f4" :weight bold)))) ; Text on Surface1
   '(corfu-bar ((t (:background "#f5c2e7"))))                             ; Pink
   '(corfu-border ((t (:background "#6c7086"))))                           ; Overlay0
   '(corfu-annotations ((t (:foreground "#a6adc8" :italic t))))           ; Subtext0
   '(corfu-deprecated ((t (:foreground "#6c7086" :strike-through t))))))   ; Overlay0

;;; 9. Marginalia Faces
(use-package marginalia
  :ensure nil
  :config
  (custom-set-faces
   '(marginalia-archive ((t (:foreground "#a6e3a1"))))      ; Green
   '(marginalia-char ((t (:foreground "#fab387"))))         ; Peach
   '(marginalia-date ((t (:foreground "#89b4fa"))))         ; Blue
   '(marginalia-documentation ((t (:foreground "#a6adc8" :italic t)))) ; Subtext0
   '(marginalia-file-name ((t (:foreground "#cdd6f4"))))     ; Text
   '(marginalia-file-owner ((t (:foreground "#cba6f7"))))    ; Mauve
   '(marginalia-file-priv-dir ((t (:foreground "#89b4fa"))))  ; Blue
   '(marginalia-file-priv-exec ((t (:foreground "#a6e3a1")))) ; Green
   '(marginalia-file-priv-link ((t (:foreground "#94e2d5")))) ; Teal
   '(marginalia-file-priv-read ((t (:foreground "#fab387")))) ; Peach
   '(marginalia-file-priv-write ((t (:foreground "#f38ba8")))); Red
   '(marginalia-function ((t (:foreground "#89b4fa"))))      ; Blue
   '(marginalia-key ((t (:foreground "#fab387"))))          ; Peach
   '(marginalia-lighter ((t (:foreground "#6c7086"))))       ; Overlay0
   '(marginalia-list ((t (:foreground "#94e2d5"))))         ; Teal
   '(marginalia-mode ((t (:foreground "#cba6f7"))))         ; Mauve
   '(marginalia-modified ((t (:foreground "#f9e2af"))))     ; Yellow
   '(marginalia-null ((t (:foreground "#6c7086"))))          ; Overlay0
   '(marginalia-number ((t (:foreground "#fab387"))))       ; Peach
   '(marginalia-size ((t (:foreground "#a6e3a1"))))         ; Green
   '(marginalia-string ((t (:foreground "#a6e3a1"))))       ; Green
   '(marginalia-symbol ((t (:foreground "#cba6f7"))))       ; Mauve
   '(marginalia-true ((t (:foreground "#a6e3a1"))))         ; Green
   '(marginalia-type ((t (:foreground "#89b4fa"))))         ; Blue
   '(marginalia-value ((t (:foreground "#cdd6f4"))))        ; Text
   '(marginalia-variable ((t (:foreground "#94e2d5"))))     ; Teal
   '(marginalia-version ((t (:foreground "#a6e3a1"))))))   ; Green

;;; 10. Orderless Faces
(use-package orderless
  :ensure nil
  :config
  (custom-set-faces
   '(orderless-match-face-0 ((t (:foreground "#cba6f7" :weight bold)))) ; Mauve
   '(orderless-match-face-1 ((t (:foreground "#89b4fa" :weight bold)))) ; Blue
   '(orderless-match-face-2 ((t (:foreground "#a6e3a1" :weight bold)))) ; Green
   '(orderless-match-face-3 ((t (:foreground "#f9e2af" :weight bold)))))) ; Yellow

;;; 11. Consult Faces
(use-package consult
  :ensure nil
  :config
  (custom-set-faces
   '(consult-bookmark ((t (:foreground "#cba6f7"))))              ; Mauve
   '(consult-buffer ((t (:foreground "#cdd6f4"))))                ; Text
   '(consult-file ((t (:foreground "#94e2d5"))))                  ; Teal
   '(consult-imenu-prefix ((t (:foreground "#6c7086"))))           ; Overlay0
   '(consult-key ((t (:foreground "#fab387"))))                   ; Peach
   '(consult-line-number ((t (:foreground "#6c7086"))))            ; Overlay0
   '(consult-line-number-prefix ((t (:foreground "#45475a"))))     ; Surface1
   '(consult-line-number-wrapped ((t (:foreground "#f38ba8"))))    ; Red
   '(consult-narrow-indicator ((t (:foreground "#f9e2af"))))       ; Yellow
   '(consult-preview-cursor ((t (:background "#fab387"))))         ; Peach
   '(consult-preview-error ((t (:foreground "#f38ba8"))))          ; Red
   '(consult-preview-insertion ((t (:background "#a6e3a1" :foreground "#1e1e2e")))) ; Green/Base
   '(consult-preview-line ((t (:background "#45475a"))))            ; Surface1
   '(consult-preview-match ((t (:background "#cba6f7" :foreground "#1e1e2e")))))) ; Mauve/Base

;;; 12. Nerd-Icons-Corfu
(use-package nerd-icons-corfu
  :ensure nil
  :config
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face (:foreground "#94e2d5"))           ; Teal
          (boolean :style "cod" :icon "symbol_boolean" :face (:foreground "#a6e3a1"))       ; Green
          (class :style "cod" :icon "symbol_class" :face (:foreground "#cba6f7"))           ; Mauve
          (color :style "cod" :icon "symbol_color" :face (:foreground "#fab387"))           ; Peach
          (command :style "cod" :icon "terminal" :face (:foreground "#89b4fa"))             ; Blue
          (constant :style "cod" :icon "symbol_constant" :face (:foreground "#f9e2af"))     ; Yellow
          (constructor :style "cod" :icon "triangle_right" :face (:foreground "#94e2d5"))   ; Teal
          (enummember :style "cod" :icon "symbol_enum_member" :face (:foreground "#a6e3a1")); Green
          (enum-member :style "cod" :icon "symbol_enum_member" :face (:foreground "#a6e3a1")); Green
          (enum :style "cod" :icon "symbol_enum" :face (:foreground "#cba6f7"))             ; Mauve
          (event :style "cod" :icon "symbol_event" :face (:foreground "#f38ba8"))           ; Red
          (field :style "cod" :icon "symbol_field" :face (:foreground "#94e2d5"))           ; Teal
          (file :style "cod" :icon "symbol_file" :face (:foreground "#cdd6f4"))             ; Text
          (folder :style "cod" :icon "folder" :face (:foreground "#89b4fa"))                ; Blue
          (interface :style "cod" :icon "symbol_interface" :face (:foreground "#cba6f7"))   ; Mauve
          (keyword :style "cod" :icon "symbol_keyword" :face (:foreground "#fab387"))       ; Peach
          (macro :style "cod" :icon "symbol_misc" :face (:foreground "#f9e2af"))            ; Yellow
          (magic :style "cod" :icon "wand" :face (:foreground "#cba6f7"))                   ; Mauve
          (method :style "cod" :icon "symbol_method" :face (:foreground "#89b4fa"))         ; Blue
          (function :style "cod" :icon "symbol_method" :face (:foreground "#89b4fa"))       ; Blue
          (module :style "cod" :icon "file_submodule" :face (:foreground "#a6e3a1"))        ; Green
          (numeric :style "cod" :icon "symbol_numeric" :face (:foreground "#fab387"))       ; Peach
          (operator :style "cod" :icon "symbol_operator" :face (:foreground "#f38ba8"))     ; Red
          (param :style "cod" :icon "symbol_parameter" :face (:foreground "#94e2d5"))       ; Teal
          (property :style "cod" :icon "symbol_property" :face (:foreground "#a6e3a1"))     ; Green
          (reference :style "cod" :icon "references" :face (:foreground "#94e2d5"))         ; Teal
          (snippet :style "cod" :icon "symbol_snippet" :face (:foreground "#f9e2af"))       ; Yellow
          (string :style "cod" :icon "symbol_string" :face (:foreground "#a6e3a1"))         ; Green
          (struct :style "cod" :icon "symbol_structure" :face (:foreground "#cba6f7"))      ; Mauve
          (text :style "cod" :icon "symbol_key" :face (:foreground "#cdd6f4"))              ; Text
          (typeparameter :style "cod" :icon "list_unordered" :face (:foreground "#94e2d5")) ; Teal
          (type-parameter :style "cod" :icon "list_unordered" :face (:foreground "#94e2d5")); Teal
          (unit :style "cod" :icon "symbol_ruler" :face (:foreground "#a6e3a1"))            ; Green
          (value :style "cod" :icon "symbol_field" :face (:foreground "#cdd6f4"))           ; Text
          (variable :style "cod" :icon "symbol_variable" :face (:foreground "#94e2d5"))     ; Teal
          (t :style "cod" :icon "code" :face (:foreground "#cdd6f4")))))                     ; Text


;;;; Solaire Mode Integration for Catppuccin
;;
;; This ensures that solaire-mode, which provides visual distinction for
;; popups and sidebars, uses the correct background colors from the
;; Catppuccin theme palette. It swaps between the 'base' and 'mantle'
;; colors.

(use-package solaire-mode
  :ensure t
  :config
  ;; Enable solaire-mode globally
  (solaire-global-mode +1)

  ;; Hook to correctly set the background for Catppuccin
  (defun update-solaire-catppuccin-bg ()
    "Set the solaire background from the Catppuccin theme."
    (when (and (boundp 'catppuccin-palette) catppuccin-palette)
      (let ((bg-main (plist-get catppuccin-palette :base))
            (bg-alt (plist-get catppuccin-palette :mantle)))
        (set-face-attribute 'solaire-mode-line-face nil
                            :background bg-alt :box `(:line-width 1 :color ,bg-alt))
        (set-face-attribute 'solaire-mode-line-inactive-face nil
                            :background bg-alt :box `(:line-width 1 :color ,bg-alt))
        (setq solaire-mode-real-buffer-bg bg-main
              solaire-mode-remap-modeline 'simple))))

  ;; Add a hook to apply our custom function after Catppuccin loads
  (add-hook 'catppuccin-after-load-theme-hook #'update-solaire-catppuccin-bg)
  ;; Also call it after solaire-mode is enabled
  (add-hook 'solaire-mode-hook #'update-solaire-catppuccin-bg)

  ;; Apply to specific buffers as in your original config
  (with-eval-after-load 'corfu
    (advice-add 'corfu--make-buffer :after
                (lambda (candidates &rest _)
                  (when-let ((buffer (get-buffer " *corfu*")))
                    (with-current-buffer buffer (solaire-mode +1))))))

  (with-eval-after-load 'vertico
    (advice-add 'vertico--display-candidates :after
                (lambda (&rest _)
                  (when (minibufferp)
                    (with-selected-window (minibuffer-window) (solaire-mode +1))))))

  (with-eval-after-load 'ediff
    (advice-add 'ediff-setup-control-buffer :after
                (lambda (&rest _) (solaire-mode +1))))

  (with-eval-after-load 'org
    (add-hook 'org-src-mode-hook #'solaire-mode))

  (with-eval-after-load 'which-key
    (advice-add 'which-key--show-buffer-side-window :after
                (lambda (&rest _)
                  (when-let ((buffer (get-buffer which-key--buffer)))
                    (with-current-buffer buffer (solaire-mode +1))))))

  (dolist (mode '(help-mode-hook info-mode-hook))
    (add-hook mode #'solaire-mode)))
