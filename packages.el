;;; -*- lexical-binding: t; -*-

;; Theme & UI
(package! all-the-icons)
(package! catppuccin-theme)
(package! mixed-pitch)
(package! visual-fill-column)
(package! info-colors)
(package! colorful-mode)
(package! rainbow-identifiers)
(package! rainbow-delimiters)

;; Evil
(package! evil-goggles)
(package! evil-escape)
(package! evil-cleverparens
  :recipe (:host github :repo "tomdl89/evil-cleverparens" :branch "fix/delete-escaped-parens"))
(package! paredit)

;; Markdown
(package! vmd-mode)

;; Org & Roam
(package! org-roam-ui)
(package! doct)
(package! evil-org)
(package! org-super-agenda)
(package! org-fragtog)

;; Spell Checking
(package! jinx)

;; Completion & Snippets
(package! consult-yasnippet)

;; Citations & LaTeX
(package! org-fragtog)
(package! citar-org-roam)
(package! org-roam-bibtex)
(package! consult-bibtex
  :recipe (:host github :repo "mohkale/consult-bibtex"))
(package! mixed-pitch)
(package! laas)
(package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon"))
(package! engrave-faces :recipe (:host github :repo "tecosaur/engrave-faces"))

;; Development & System
(package! embark-vc)
(package! multi-vterm)
(package! feature-mode)
(package! systemd)

;; Dependencies for LSP/Org-Jupyter
(package! zmq)
(package! websocket)

;; Ignored Packages
(package! hydra :ignore t)
(package! helm-bibtex :ignore t)
