;;; definitive-latex-and-pdf-config.el --- The Ultimate LaTeX, Org, and PDF Setup for Emacs 30

;;; Commentary:
;;
;; This configuration provides a complete, IDE-like environment for writing
;; LaTeX and Org documents. It is powered by the `texlab` Language Server
;; for intelligent features and Tectonic for fast, modern compilation.
;;
;; This setup is designed to be robust, streamlined, and highly integrated.

;;; Prerequisites:
;;
;; Please ensure the following external tools are installed on your system:
;;
;; 1. **Tectonic (Compiler):** `https://tectonic-typesetting.github.io/`
;;    The recommended compiler for this setup.
;;
;; 2. **texlab (Language Server):** `https://github.com/latex-lsp/texlab`
;;    Install via your system's package manager (e.g., `sudo apt install texlab`)
;;    or download the binary from the releases page.
;;
;; 3. **chktex (Linter):** `https://www.nongnu.org/chktex/`
;;    Used by `texlab` for on-the-fly diagnostics.
;;    (e.g., `sudo apt install chktex`).
;;
;; 4. **A functional Org and Org-roam v2 setup** and a bibliography file.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Core LaTeX Environment (eglot + texlab + AUCTeX)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'eglot
  ;; Teach eglot how to start texlab
  (add-to-list 'eglot-server-programs
               '((latex-mode tex-mode plain-tex-mode) . ("texlab"))))

;; AUCTeX is still essential for its powerful compilation and viewing commands.
;; It works alongside eglot, which will handle diagnostics, completion, etc.
(use-package tex
  :ensure auctex
  :defer t
  :config
  ;; Set TeX-engine to Tectonic for fast, modern compilation
  (setq-default TeX-engine 'tectonic)

  ;; Enable multi-file project support
  (setq-default TeX-master nil) ; Ask for master file on a per-project basis
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  ;; Hook to enable AUCTeX features and start the LSP
  (add-hook 'TeX-mode-hook
            (lambda ()
              ;; Start the Language Server
              (eglot-ensure)
              ;; Enable AUCTeX's font-locking and document parser
              (font-latex-setup)
              (TeX-parser-change-style-and-environment)
              ;; Enable RefTeX for cross-referencing (complements Citar)
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              ;; Enable folding of environments
              (TeX-fold-mode 1)
              ;; Enable auto-fill for line wrapping
              (auto-fill-mode 1)))

  ;; Configure Tectonic as a compilation option in AUCTeX
  (with-eval-after-load 'tex
    (add-to-list 'TeX-command-list
                 '("Tectonic" "tectonic -X compile %s"
                   TeX-run-command nil t :help "Compile with Tectonic"))
    (setq TeX-default-mode 'latex-mode)
    (setq TeX-source-correlate-mode t)
    (setq TeX-source-correlate-method 'synctex)
    (setq TeX-source-correlate-start-server t))

  ;; Set viewer to PDF-Tools
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" "TeX-pdf-tools-sync-view"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Citation Management (Citar, Embark, Parsebib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package citar
  :ensure t
  :after org
  :custom
  (citar-bibliography (list (expand-file-name "~/org/roam/refs.bib")))
  (citar-library-read-file-functions '((bibtex . citar-bibtex-read-file-parsebib)))
  (citar-notes-paths (list (expand-file-name "~/org/roam/")))
  (citar-notes-source 'org-roam)
  :config
  ;; Integration with `completion-at-point`
  (defun +latex-completion-at-point ()
    "Enable citar completion for LaTeX and Org."
    (setq-local completion-at-point-functions
                (cons #'citar-capf-citation
                      (cons #'cape-tex
                            completion-at-point-functions))))
  (add-hook 'LaTeX-mode-hook #'+latex-completion-at-point)
  (add-hook 'org-mode-hook #'+latex-completion-at-point))

(use-package parsebib
  :ensure t)

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  (add-to-list 'embark-indicators #'embark-mixed-indicator)
  (add-to-list 'embark-keymap-alist '(citar-ref . embark-citar-map)))

(use-package citar-embark
  :ensure t
  :after (citar embark)
  :config
  (citar-embark-mode)
  ;; Add custom Embark actions for citations
  (define-key embark-citar-map (kbd "c") #'citar-copy-reference)
  (define-key embark-citar-map (kbd "b") #'citar-copy-bibtex)
  (define-key embark-citar-map (kbd "o") #'citar-open)
  (define-key embark-citar-map (kbd "n") #'citar-open-notes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Org Mode and Org-roam for LaTeX Authoring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :ensure nil
  :config
  (setq org-latex-default-class "article")
  (setq org-latex-default-class-options '("11pt" "a4paper"))
  (setq org-latex-compiler "tectonic")
  (setq org-latex-pdf-process '("tectonic -X compile %f -o %o"))
  (setq org-latex-custom-preamble-header "\\usepackage{amsmath}\n\\usepackage{amssymb}")
  (add-hook 'org-mode-hook (lambda () (org-latex-preview-mode 1))))

(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("◉" "○" "●" "▷" "▶" "◆" "◇"))
  (org-superstar-prettify-item-bullets t))

(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :config
  (setq citar-org-roam-note-title-template "${author} (${year}) - ${title}")
  (setq citar-org-roam-capture-template-key "r"))

(with-eval-after-load 'org-roam
  (require 'org-roam-dailies)
  (org-roam-dailies-mode 1)
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%H:%M> %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. PDF Viewer (PDF-Tools)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (setq-default TeX-view-program-selection '((output-pdf "PDF Tools")))
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode 1)))

  ;; Catppuccin Mocha Theme Integration for PDF-Tools
  (defun setup-catppuccin-pdf-tools-faces ()
    (custom-set-faces
     '(pdf-view-background-color ((t (:background "#1e1e2e"))))
     '(pdf-view-selection-face ((t (:background "#585b70"))))
     '(pdf-view-search-hl-face ((t (:background "#f9e2af" :foreground "#1e1e2e"))))
     '(pdf-view-link-face ((t (:foreground "#89b4fa" :underline t))))
     '(pdf-view-annotation-face ((t (:background "#f38ba8"))))
     '(pdf-view-annotation-fringe-face ((t (:foreground "#f38ba8"))))
     '(pdf-view-active-line-face ((t (:background "#313244"))))))
  (setup-catppuccin-pdf-tools-faces)
  (add-hook 'after-load-theme-hook #'setup-catppuccin-pdf-tools-faces)

  ;; Annotation keybindings
  (define-key pdf-view-mode-map (kbd "C-c C-a a") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "C-c C-a d") 'pdf-annot-delete)
  (define-key pdf-view-mode-map (kbd "C-c C-a h") 'pdf-annot-add-highlight-markup-annotation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Writing Environment (Snippets, Typography, Helpers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-latex-prettify-symbols-alist
  '(("\\alpha" . ?α) ("\\beta" . ?β) ("\\gamma" . ?γ) ("\\delta" . ?δ)
    ("\\epsilon" . ?ε) ("\\zeta" . ?ζ) ("\\eta" . ?η) ("\\theta" . ?θ)
    ("\\iota" . ?ι) ("\\kappa" . ?κ) ("\\lambda" . ?λ) ("\\mu" . ?μ)
    ("\\nu" . ?ν) ("\\xi" . ?ξ) ("\\pi" . ?π) ("\\rho" . ?ρ)
    ("\\sigma" . ?σ) ("\\tau" . ?τ) ("\\upsilon" . ?υ) ("\\phi" . ?φ)
    ("\\chi" . ?χ) ("\\psi" . ?ψ) ("\\omega" . ?ω)
    ("\\Gamma" . ?Γ) ("\\Delta" . ?Δ) ("\\Theta" . ?Θ) ("\\Lambda" . ?Λ)
    ("\\Xi" . ?Ξ) ("\\Pi" . ?Π) ("\\Sigma" . ?Σ) ("\\Upsilon" . ?Υ)
    ("\\Phi" . ?Φ) ("\\Psi" . ?Ψ) ("\\Omega" . ?Ω)
    ("\\int" . ?∫) ("\\sum" . ?∑) ("\\prod" . ?∏) ("\\sqrt" . ?√)
    ("\\infty" . ?∞) ("\\partial" . ?∂) ("\\nabla" . ?∇) ("\\pm" . ?±)
    ("\\times" . ?×) ("\\div" . ?÷) ("\\neq" . ?≠) ("\\leq" . ?≤)
    ("\\geq" . ?≥) ("\\equiv" . ?≡) ("\\approx" . ?≈) ("\\in" . ?∈)
    ("\\notin" . ?∉) ("\\subset" . ?⊂) ("\\supset" . ?⊃)
    ("\\to" . ?→) ("\\rightarrow" . ?→) ("\\gets" . ?←) ("\\leftarrow" . ?←)
    ("\\Rightarrow" . ?⇒) ("\\Leftarrow" . ?⇐) ("\\leftrightarrow" . ?↔)
    ("\\forall" . ?∀) ("\\exists" . ?∃) ("\\land" . ?∧) ("\\lor" . ?∨)
    ("\\lnot" . ?¬))
  "Alist of symbols to prettify in LaTeX and Org modes.")

(add-hook 'latex-mode-hook
          (lambda ()
            (setq-local prettify-symbols-alist my-latex-prettify-symbols-alist)
            (prettify-symbols-mode 1)))
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local prettify-symbols-alist my-latex-prettify-symbols-alist)
            (prettify-symbols-mode 1)))

(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(with-eval-after-load 'cape
  ;; Integrate yasnippet into the completion framework
  (add-to-list 'completion-at-point-functions #'cape-yasnippet))

(use-package laas
  :ensure t
  :hook (latex-mode . laas-mode)
  :custom
  (laas-enable-auto-preview t)
  (laas-preview-engine 'tectonic))

(use-package cdlatex
  :ensure t
  :hook ((latex-mode . cdlatex-mode)
         (org-mode . (lambda () (cdlatex-mode 1))))
  :config
  (setq cdlatex-str-alist
        '(("eq" . "equation")
          ("en" . "enumerate")
          ("it" . "itemize")
          ("fig" . "figure")
          ("tab" . "tabular")))
  (setq cdlatex-paired-parens-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Custom Functions and Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-latex/clean-aux-files ()
  "Clean up LaTeX auxiliary files in the current directory."
  (interactive)
  (when (derived-mode-p 'latex-mode 'org-mode)
    (let* ((dir (file-name-directory (or (buffer-file-name) default-directory)))
           (files (directory-files dir t "\\.\\(aux\\|bbl\\|blg|fdb_latexmk\\|log\\|out\\|toc\\|synctex\\.gz\\)$")))
      (if files
          (when (y-or-n-p (format "Delete %d auxiliary files? " (length files)))
            (mapc #'delete-file files)
            (message "Auxiliary files cleaned."))
        (message "No auxiliary files to clean.")))))

;; Assuming `general.el` is already configured from your base config.
(with-eval-after-load 'tex
  (general-define-key
   :keymaps 'latex-mode-map
   :states '(normal insert visual)
   :prefix "SPC"
   "m" `(:keymap ,(make-sparse-keymap) :which-key "LaTeX")
   "m c" '(:ignore t :which-key "Compile")
   "m c c" '(TeX-command-master :which-key "Compile Master")
   "m c v" '(TeX-view :which-key "View Output")
   "m c k" '(TeX-kill-job :which-key "Kill Job")
   "m c C" '(my-latex/clean-aux-files :which-key "Clean Aux Files")
   "m a" '(:ignore t :which-key "LSP Actions")
   "m a a" '(eglot-code-actions :which-key "Code Actions")
   "m r" '(:ignore t :which-key "LSP Rename/Refs")
   "m r r" '(eglot-rename :which-key "Rename Symbol")
   "m b" '(:ignore t :which-key "Bibliography")
   "m b i" '(citar-insert-citation :which-key "Insert Citation")))

(with-eval-after-load 'org
  (general-define-key
   :keymaps 'org-mode-map
   :prefix "SPC"
   "m e" '(:ignore t :wk "Export")
   "m e l o" '(org-latex-export-to-pdf :which-key "Export LaTeX to PDF")))

(general-define-key
 :keymaps 'global-map
 :prefix "C-c"
 "l" '(:ignore t :wk "LaTeX Global")
 "l b" '(:ignore t :wk "Bibliography")
 "l b i" '(citar-insert-citation :which-key "Insert Citation")
 "l b o" '(citar-open :which-key "Open Reference")
 "l b n" '(citar-open-notes :which-key "Open Notes"))


(provide 'definitive-latex-and-pdf-config)
;;; definitive-latex-and-pdf-config.el ends here
