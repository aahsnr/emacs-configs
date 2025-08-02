;;; -*- lexical-binding: t; -*-
;;; Comprehensive Ligatures and Unicode Configuration for Emacs 30
;;; Configured for org-mode, text-mode, and latex-mode only

;;; Unicode and Font Configuration
(use-package emacs
  :ensure nil
  :config
  ;; Enable Unicode support
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  
  ;; Ensure proper Unicode handling in daemon mode
  (when (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (set-language-environment "UTF-8")
                (set-default-coding-systems 'utf-8))))
  
  ;; Font fallback for Unicode characters
  (set-fontset-font t 'unicode-bmp "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'unicode-sip "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'mathematical "Noto Color Emoji" nil 'append))

;;; Prettify Symbols Mode Configuration
(use-package prog-mode
  :ensure nil
  :config
  ;; Base prettify symbols for programming modes
  (defvar base-prettify-symbols-alist
    '(;; Logical operators
      ("&&" . ?∧)
      ("||" . ?∨)
      ("not" . ?¬)
      
      ;; Relational operators
      ("==" . ?≡)
      ("!=" . ?≠)
      ("<=" . ?≤)
      (">=" . ?≥)
      
      ;; Arrows
      ("->" . ?→)
      ("=>" . ?⇒)
      ("<-" . ?←)
      ("<->" . ?↔)
      ("<=>" . ?⇔)
      
      ;; Mathematical
      ("sum" . ?∑)
      ("product" . ?∏)
      ("sqrt" . ?√)
      ("infinity" . ?∞)
      ("alpha" . ?α)
      ("beta" . ?β)
      ("gamma" . ?γ)
      ("delta" . ?δ)
      ("lambda" . ?λ)
      ("pi" . ?π)
      ("sigma" . ?σ)
      ("theta" . ?θ)
      ("omega" . ?ω)
      
      ;; Others
      ("..." . ?…)
      ("null" . ?∅)
      ("true" . ?⊤)
      ("false" . ?⊥))
    "Base prettify symbols alist for all modes."))

;;; Org-mode specific configuration
(use-package org
  :ensure nil
  :hook (org-mode . (lambda ()
                      (setq-local prettify-symbols-alist
                                  (append base-prettify-symbols-alist
                                          org-prettify-symbols-alist))
                      (prettify-symbols-mode 1)))
  :config
  ;; Org-specific symbols
  (defvar org-prettify-symbols-alist
    '(;; Org-specific symbols
      ("TODO" . ?⚡)
      ("DONE" . ?✓)
      ("CANCELLED" . ?✗)
      ("SCHEDULED" . ?⏰)
      ("DEADLINE" . ?⚠)
      ("CLOSED" . ?✓)
      ("PROPERTIES" . ?⚙)
      ("BEGIN_SRC" . ?⌨)
      ("END_SRC" . ?⌨)
      ("BEGIN_QUOTE" . ?❝)
      ("END_QUOTE" . ?❞)
      ("BEGIN_EXAMPLE" . ?⌨)
      ("END_EXAMPLE" . ?⌨)
      ("#+TITLE:" . ?📄)
      ("#+AUTHOR:" . ?✍)
      ("#+DATE:" . ?📅)
      ("#+EMAIL:" . ?✉)
      ("#+TAGS:" . ?🏷)
      ("#+FILETAGS:" . ?🏷)
      ("#+CATEGORY:" . ?📂)
      ("#+STARTUP:" . ?🚀)
      ("#+OPTIONS:" . ?⚙)
      ("#+LATEX_HEADER:" . ?📄)
      ("#+HTML_HEAD:" . ?🌐)
      ("[ ]" . ?☐)
      ("[X]" . ?☑)
      ("[-]" . ?☐)
      ("#+BEGIN_" . ?▼)
      ("#+END_" . ?▲))
    "Org-mode specific prettify symbols."))

;;; Text-mode specific configuration
(use-package text-mode
  :ensure nil
  :hook (text-mode . (lambda ()
                       (setq-local prettify-symbols-alist
                                   (append base-prettify-symbols-alist
                                           text-prettify-symbols-alist))
                       (prettify-symbols-mode 1)))
  :config
  ;; Text-mode specific prettification
  (defvar text-prettify-symbols-alist
    '(;; Common text symbols
      ("(c)" . ?©)
      ("(r)" . ?®)
      ("(tm)" . ?™)
      ("+-" . ?±)
      ("1/2" . ?½)
      ("1/3" . ?⅓)
      ("1/4" . ?¼)
      ("2/3" . ?⅔)
      ("3/4" . ?¾)
      ("1/8" . ?⅛)
      ("3/8" . ?⅜)
      ("5/8" . ?⅝)
      ("7/8" . ?⅞)
      ("---" . ?—)
      ("--" . ?–)
      ("..." . ?…)
      ("<<" . ?«)
      (">>" . ?»)
      ("''" . ?")
      ("``" . ?")
      ("<->" . ?↔)
      ("->" . ?→)
      ("<-" . ?←)
      ("=>" . ?⇒)
      ("<=>" . ?⇔)
      ("degree" . ?°)
      ("celsius" . ?℃)
      ("fahrenheit" . ?℉))
    "Text-mode specific prettify symbols."))

;;; LaTeX-mode specific configuration
(use-package tex-mode
  :ensure nil
  :hook (latex-mode . (lambda ()
                        (setq-local prettify-symbols-alist
                                    (append base-prettify-symbols-alist
                                            latex-prettify-symbols-alist))
                        (prettify-symbols-mode 1)))
  :config
  ;; LaTeX-mode specific prettification
  (defvar latex-prettify-symbols-alist
    '(;; Greek letters (lowercase)
      ("\\alpha" . ?α)
      ("\\beta" . ?β)
      ("\\gamma" . ?γ)
      ("\\delta" . ?δ)
      ("\\epsilon" . ?ε)
      ("\\varepsilon" . ?ε)
      ("\\zeta" . ?ζ)
      ("\\eta" . ?η)
      ("\\theta" . ?θ)
      ("\\vartheta" . ?ϑ)
      ("\\iota" . ?ι)
      ("\\kappa" . ?κ)
      ("\\lambda" . ?λ)
      ("\\mu" . ?μ)
      ("\\nu" . ?ν)
      ("\\xi" . ?ξ)
      ("\\pi" . ?π)
      ("\\varpi" . ?ϖ)
      ("\\rho" . ?ρ)
      ("\\varrho" . ?ϱ)
      ("\\sigma" . ?σ)
      ("\\varsigma" . ?ς)
      ("\\tau" . ?τ)
      ("\\upsilon" . ?υ)
      ("\\phi" . ?φ)
      ("\\varphi" . ?ϕ)
      ("\\chi" . ?χ)
      ("\\psi" . ?ψ)
      ("\\omega" . ?ω)
      
      ;; Greek letters (uppercase)
      ("\\Gamma" . ?Γ)
      ("\\Delta" . ?Δ)
      ("\\Theta" . ?Θ)
      ("\\Lambda" . ?Λ)
      ("\\Xi" . ?Ξ)
      ("\\Pi" . ?Π)
      ("\\Sigma" . ?Σ)
      ("\\Upsilon" . ?Υ)
      ("\\Phi" . ?Φ)
      ("\\Psi" . ?Ψ)
      ("\\Omega" . ?Ω)
      
      ;; Mathematical operators
      ("\\sum" . ?∑)
      ("\\prod" . ?∏)
      ("\\coprod" . ?∐)
      ("\\int" . ?∫)
      ("\\iint" . ?∬)
      ("\\iiint" . ?∭)
      ("\\oint" . ?∮)
      ("\\infty" . ?∞)
      ("\\partial" . ?∂)
      ("\\nabla" . ?∇)
      ("\\pm" . ?±)
      ("\\mp" . ?∓)
      ("\\times" . ?×)
      ("\\div" . ?÷)
      ("\\cdot" . ?·)
      ("\\bullet" . ?•)
      ("\\circ" . ?∘)
      ("\\oplus" . ?⊕)
      ("\\ominus" . ?⊖)
      ("\\otimes" . ?⊗)
      ("\\oslash" . ?⊘)
      ("\\odot" . ?⊙)
      ("\\bigcirc" . ?◯)
      ("\\dagger" . ?†)
      ("\\ddagger" . ?‡)
      ("\\star" . ?⋆)
      ("\\ast" . ?∗)
      
      ;; Relations
      ("\\leq" . ?≤)
      ("\\geq" . ?≥)
      ("\\equiv" . ?≡)
      ("\\models" . ?⊨)
      ("\\prec" . ?≺)
      ("\\succ" . ?≻)
      ("\\sim" . ?∼)
      ("\\perp" . ?⊥)
      ("\\preceq" . ?≼)
      ("\\succeq" . ?≽)
      ("\\simeq" . ?≃)
      ("\\mid" . ?∣)
      ("\\ll" . ?≪)
      ("\\gg" . ?≫)
      ("\\asymp" . ?≍)
      ("\\parallel" . ?∥)
      ("\\subset" . ?⊂)
      ("\\supset" . ?⊃)
      ("\\approx" . ?≈)
      ("\\subseteq" . ?⊆)
      ("\\supseteq" . ?⊇)
      ("\\cong" . ?≅)
      ("\\neq" . ?≠)
      ("\\in" . ?∈)
      ("\\ni" . ?∋)
      ("\\propto" . ?∝)
      ("\\vdash" . ?⊢)
      ("\\dashv" . ?⊣)
      ("\\notin" . ?∉)
      
      ;; Arrows
      ("\\leftarrow" . ?←)
      ("\\gets" . ?←)
      ("\\rightarrow" . ?→)
      ("\\to" . ?→)
      ("\\leftrightarrow" . ?↔)
      ("\\Leftarrow" . ?⇐)
      ("\\Rightarrow" . ?⇒)
      ("\\Leftrightarrow" . ?⇔)
      ("\\mapsto" . ?↦)
      ("\\hookleftarrow" . ?↩)
      ("\\hookrightarrow" . ?↪)
      ("\\uparrow" . ?↑)
      ("\\downarrow" . ?↓)
      ("\\updownarrow" . ?↕)
      ("\\Uparrow" . ?⇑)
      ("\\Downarrow" . ?⇓)
      ("\\Updownarrow" . ?⇕)
      ("\\nearrow" . ?↗)
      ("\\searrow" . ?↘)
      ("\\swarrow" . ?↙)
      ("\\nwarrow" . ?↖)
      
      ;; Logic symbols
      ("\\land" . ?∧)
      ("\\lor" . ?∨)
      ("\\lnot" . ?¬)
      ("\\neg" . ?¬)
      ("\\top" . ?⊤)
      ("\\bot" . ?⊥)
      ("\\exists" . ?∃)
      ("\\forall" . ?∀)
      ("\\nexists" . ?∄)
      
      ;; Set theory
      ("\\emptyset" . ?∅)
      ("\\varnothing" . ?∅)
      ("\\cap" . ?∩)
      ("\\cup" . ?∪)
      ("\\bigcap" . ?⋂)
      ("\\bigcup" . ?⋃)
      ("\\setminus" . ?∖)
      
      ;; Miscellaneous
      ("\\angle" . ?∠)
      ("\\wp" . ?℘)
      ("\\Re" . ?ℜ)
      ("\\Im" . ?ℑ)
      ("\\aleph" . ?ℵ)
      ("\\hbar" . ?ℏ)
      ("\\ell" . ?ℓ)
      ("\\partial" . ?∂)
      ("\\clubsuit" . ?♣)
      ("\\diamondsuit" . ?♢)
      ("\\heartsuit" . ?♡)
      ("\\spadesuit" . ?♠)
      ("\\sharp" . ?♯)
      ("\\flat" . ?♭)
      ("\\natural" . ?♮)
      ("\\surd" . ?√)
      ("\\triangle" . ?△)
      ("\\checkmark" . ?✓))
    "LaTeX-mode specific prettify symbols."))

;;; Auto-composition mode for true ligatures (when supported)
(use-package composite
  :ensure nil
  :config
  ;; Enable auto-composition mode globally
  (global-auto-composition-mode 1))

;;; Daemon mode support
(use-package server
  :ensure nil
  :config
  ;; Ensure ligatures and Unicode work properly in daemon mode
  (when (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  ;; Re-enable auto-composition mode
                  (global-auto-composition-mode 1)
                  ;; Re-setup font configurations
                  (set-fontset-font t 'unicode-bmp "Noto Color Emoji" nil 'append)
                  (set-fontset-font t 'unicode-sip "Noto Color Emoji" nil 'append)
                  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
                  (set-fontset-font t 'mathematical "Noto Color Emoji" nil 'append)
                  ;; Refresh all buffers with the modes we care about
                  (dolist (buffer (buffer-list))
                    (with-current-buffer buffer
                      (when (derived-mode-p 'org-mode 'text-mode 'latex-mode)
                        (font-lock-refresh-defaults)
                        (when (bound-and-true-p prettify-symbols-mode)
                          (prettify-symbols-mode -1)
                          (prettify-symbols-mode 1))))))))))

;;; Performance optimizations
(use-package emacs
  :ensure nil
  :config
  ;; Optimize composition performance
  (setq-default bidi-display-reordering 'left-to-right)
  (setq bidi-paragraph-direction 'left-to-right)
  
  ;; Optimize font rendering
  (setq inhibit-compacting-font-caches t)
  (setq font-lock-maximum-decoration t)
  
  ;; Ensure smooth scrolling with ligatures
  (setq scroll-conservatively 10000)
  (setq scroll-preserve-screen-position t)
  (setq scroll-margin 0)
  (setq-default cursor-type 'box)
  (setq x-stretch-cursor t))

;;; Helper functions
(defun toggle-prettify-symbols ()
  "Toggle prettify symbols mode in current buffer."
  (interactive)
  (if (bound-and-true-p prettify-symbols-mode)
      (progn
        (prettify-symbols-mode -1)
        (message "Prettify symbols disabled"))
    (progn
      (prettify-symbols-mode 1)
      (message "Prettify symbols enabled"))))

(defun reload-prettify-symbols ()
  "Reload prettify symbols configuration for current buffer."
  (interactive)
  (when (derived-mode-p 'org-mode 'text-mode 'latex-mode)
    (font-lock-refresh-defaults)
    (when (bound-and-true-p prettify-symbols-mode)
      (prettify-symbols-mode -1)
      (prettify-symbols-mode 1))
    (message "Prettify symbols configuration reloaded")))

;; Global keybindings
(global-set-key (kbd "C-c l t") 'toggle-prettify-symbols)
(global-set-key (kbd "C-c l r") 'reload-prettify-symbols)

(provide 'ligatures-unicode-config)
