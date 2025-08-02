``````el
;; Avy Configuration for Emacs 30 with Spacemacs-like keybindings
;; Add this to your init.el or dedicated config file

;; Setup general.el for keybinding management
(use-package general
  :ensure t
  :config
  ;; Define SPC as leader key
  (general-create-definer my/leader-def
    :keymaps 'override
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC"))

;; Install avy package
(use-package avy
  :ensure t
  :config
  ;; Basic avy settings
  (setq avy-background t)           ; Dim background during selection
  (setq avy-style 'at-full)         ; Show hints at full word
  (setq avy-timeout-seconds 0.5)    ; Timeout for single char input
  (setq avy-all-windows t)          ; Search in all windows by default
  
  ;; Custom avy keys for better ergonomics
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p))
  
  ;; Custom avy actions
  (defun avy-action-kill-whole-line (pt)
    "Kill whole line at PT."
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    "Copy whole line at PT."
    (save-excursion
      (goto-char pt)
      (let ((bounds (bounds-of-thing-at-point 'line)))
        (when bounds
          (copy-region-as-kill (car bounds) (cdr bounds)))))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    "Yank whole line at PT."
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  ;; Bind custom actions to keys during avy selection
  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank-whole-line))

;; zzz-to-char - enhanced zap-to-char functionality
(use-package zzz-to-char
  :ensure t)

;; ace-window - window navigation using avy-style hints
(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ; Same keys as avy for consistency
  (setq aw-scope 'frame)                       ; Consider all windows in frame
  (setq aw-background t))                      ; Dim background

;; ace-link - follow links using avy-style hints
(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default))

;; Spacemacs-like keybindings using general.el
(my/leader-def
  ;; Jump commands (SPC j)
  "j"   '(:ignore t :which-key "jump")
  "jj"  'avy-goto-char-timer
  "jw"  'avy-goto-word-1
  "jW"  'avy-goto-word-0
  "jl"  'avy-goto-line
  "jc"  'avy-goto-char
  "jC"  'avy-goto-char-2
  "js"  'avy-goto-symbol-1
  "jf"  'avy-goto-char-in-line
  "jb"  'avy-pop-mark
  
  ;; Window commands (SPC w)
  "w"   '(:ignore t :which-key "window")
  "wo"  'ace-window
  "wd"  'ace-delete-window
  "ws"  'ace-swap-window
  
  ;; Navigation commands (SPC n)
  "n"   '(:ignore t :which-key "navigation")
  "nl"  'ace-link
  
  ;; Kill/Zap commands (SPC k)
  "k"   '(:ignore t :which-key "kill")
  "kc"  'zzz-to-char
  "kC"  'zzz-up-to-char)

;; Essential global keybindings
(general-define-key
 "M-z"     'zzz-to-char
 "M-Z"     'zzz-up-to-char
 "C-x o"   'ace-window)

;; Isearch integration
(general-define-key
 :keymaps 'isearch-mode-map
 "C-'"     'avy-isearch)

;; Evil-mode compatibility (optional - only loads if evil is present)
(with-eval-after-load 'evil
  ;; Unbind SPC from evil so leader can work
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   "SPC" nil)
  
  ;; Evil-specific avy bindings
  (general-define-key
   :states '(normal visual)
   "s"   'avy-goto-char-2
   "S"   'avy-goto-char-timer))

``````
