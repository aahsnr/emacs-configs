
;;; org-mode.el --- Standard Emacs Lisp configuration for Org mode

;; This file provides a configuration for Org mode without using the `use-package`
;; macro. The functionality is equivalent to the provided `use-package` version.

;; ----------------------------------------------------------------------------
;; Custom function for Babel source block completion
;; ----------------------------------------------------------------------------
;; We define this function at the top level so it is available to be added
;; to the hook later.
(defun my/org-babel-complete-at-point ()
  "Complete at point in org-babel src blocks.
This function checks if the point is within a source block and,
if so, attempts to provide completion based on the block's language."
  (when (org-in-src-block-p)
    (let ((lang (car (org-babel-get-src-block-info t))))
      (cond
       ((string-equal lang "emacs-lisp") (elisp-completion-at-point))
       ((string-equal lang "python") (python-completion-at-point))
       (t (cape-dabbrev))))))

;; ----------------------------------------------------------------------------
;; Configuration applied after dependencies (cape, corfu) and org are loaded
;; ----------------------------------------------------------------------------
;; This nested structure is the standard equivalent of `:after`. The outer
;; `with-eval-after-load` forms ensure that the code inside them only runs
;; after 'cape' and 'corfu' have been loaded. This prevents errors from
;; calling functions (like `cape-dabbrev`) before they are defined.
(with-eval-after-load 'cape
  (with-eval-after-load 'corfu
    ;; The `with-eval-after-load 'org` construct ensures that the enclosed code is
    ;; executed only after the 'org' feature has been loaded.
    (with-eval-after-load 'org
      ;; -- Set custom faces for a Gruvbox-like theme --
      ;; This is equivalent to placing `custom-set-faces` inside the `:config`
      ;; block of `use-package`.
      (custom-set-faces
       '(org-block-begin-line ((t (:foreground "#7c6f64" :background "#32302f"))))
       '(org-block-end-line ((t (:foreground "#7c6f64" :background "#32302f"))))
       '(org-code ((t (:foreground "#fe8019" :background "#3c3836"))))
       '(org-verbatim ((t (:foreground "#b8bb26" :background "#3c3836")))))

      ;; -- Add hooks to `org-mode` --
      ;; This replaces the `:hook` keyword from the `use-package` declaration.
      ;; It adds a lambda function that runs every time org-mode is enabled.
      (add-hook 'org-mode-hook
                (lambda ()
                  ;; Set up completion-at-point-functions (CAPFs) for Org mode.
                  ;; This list determines which functions are used to find completions.
                  (setq-local completion-at-point-functions
                              (list #'cape-elisp-symbol
                                    #'org-pcomplete-initial
                                    #'cape-dabbrev
                                    #'cape-file
                                    #'cape-keyword))
                  ;; Add our custom babel completion function to the list of CAPFs.
                  ;; The 't' at the end makes this hook buffer-local.
                  (add-hook 'completion-at-point-functions #'my/org-babel-complete-at-point nil t))))))

;;; org-mode.el ends here
