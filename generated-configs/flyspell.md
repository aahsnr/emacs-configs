``````el
;; Flyspell configuration with Hunspell backend and popup-el integration
(use-package popup :defer t)

(use-package flyspell
  :ensure nil
  :defer t
  :init
  ;; Hunspell configuration
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US")
  
  (when (executable-find "hunspell")
    (setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
            ("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8))))
  
  ;; Verification functions
  (defun my/flyspell-verify ()
    "Verify if flyspell should check current position."
    (cond
     ((derived-mode-p 'org-mode)
      (not (memq (org-element-type (org-element-at-point))
                 '(headline src-block inline-src-block example-block
                   fixed-width export-block latex-environment
                   latex-fragment link property-drawer keyword))))
     ((derived-mode-p 'prog-mode)
      (let ((face (get-text-property (point) 'face)))
        (or (memq face '(font-lock-comment-face font-lock-comment-delimiter-face
                         font-lock-string-face font-lock-doc-face))
            (and (listp face)
                 (seq-some (lambda (f) (memq f '(font-lock-comment-face
                                                font-lock-comment-delimiter-face
                                                font-lock-string-face
                                                font-lock-doc-face))) face)))))
     (t t)))
  
  ;; Popup correction function
  (defun my/flyspell-popup-correct (event poss word)
    "Show flyspell correction menu using popup-el."
    (require 'popup)
    (let* ((corrects (if flyspell-sort-corrections
                         (sort (nth 2 poss) 'string<)
                       (nth 2 poss)))
           (menu-items (append 
                       (mapcar (lambda (c) (cons c c)) corrects)
                       '(("Save word" . save)
                         ("Accept (session)" . session)
                         ("Accept (buffer)" . buffer))))
           (choice (popup-menu* (mapcar #'car menu-items))))
      (when choice
        (let ((action (cdr (assoc choice menu-items))))
          (if (stringp action)
              (flyspell-do-correct action poss word (point) action)
            (flyspell-do-correct action nil word (point) poss))))))
  
  :config
  ;; Performance and behavior settings
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil
        flyspell-consider-dash-as-word-delimiter-flag t
        flyspell-delay 1
        flyspell-lazy-idle-seconds 1
        flyspell-emacs-popup-function 'my/flyspell-popup-correct
        flyspell-generic-check-word-predicate 'my/flyspell-verify
        flyspell-use-meta-tab nil)
  
  ;; Disable in minibuffer
  (add-hook 'minibuffer-setup-hook (lambda () (flyspell-mode -1)))
  
  :hook
  ((text-mode org-mode markdown-mode rst-mode latex-mode tex-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

;; Optional: Auto-popup correction mode
(defvar my/auto-correct-delay 1.6 "Auto-correction delay in seconds.")
(defvar my/auto-correct-timer nil "Auto-correction timer.")

(defun my/auto-correct-word ()
  "Auto-correct previous misspelled word."
  (when (and flyspell-mode (not (minibufferp)))
    (save-excursion
      (let ((overlay (flyspell-get-next-error t)))
        (when overlay
          (goto-char (overlay-start overlay))
          (flyspell-correct-word-before-point))))))

(defun my/start-auto-correct-timer ()
  "Start auto-correction timer."
  (when my/auto-correct-timer
    (cancel-timer my/auto-correct-timer))
  (setq my/auto-correct-timer
        (run-with-idle-timer my/auto-correct-delay nil #'my/auto-correct-word)))

(define-minor-mode my/flyspell-auto-correct-mode
  "Auto-correct flyspell errors after idle delay."
  :lighter " AutoCorrect"
  (if my/flyspell-auto-correct-mode
      (add-hook 'post-command-hook #'my/start-auto-correct-timer nil t)
    (remove-hook 'post-command-hook #'my/start-auto-correct-timer t)
    (when my/auto-correct-timer
      (cancel-timer my/auto-correct-timer)
      (setq my/auto-correct-timer nil))))

``````
