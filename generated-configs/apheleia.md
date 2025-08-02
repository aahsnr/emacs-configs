``````el
;; *** Core Configuration
(use-package apheleia
  :hook ((prog-mode . apheleia-mode)
         (text-mode . apheleia-mode)
         (conf-mode . apheleia-mode))
  :custom
  ;; Performance and behavior
  (apheleia-remote-algorithm 'cancel)
  (apheleia-log-only-errors t)
  (apheleia-hide-log-buffers t)
  (apheleia-max-alignment-size 1000)
  (apheleia-formatters-respect-indent-level t)
  (apheleia-mode-lighter " Φ")
  
  :config
  ;; Enhanced formatter configurations
  (setq apheleia-formatters
        (append apheleia-formatters
                `((clang-format . ("clang-format"
                                   "--style={BasedOnStyle: LLVM, IndentWidth: 4, ColumnLimit: 100, AllowShortFunctionsOnASingleLine: Empty, BreakBeforeBraces: Attach, IndentCaseLabels: true, AlignTrailingComments: true, SpaceBeforeParens: ControlStatements}"
                                   "--assume-filename" ,(lambda () (or (buffer-file-name) "stdin"))))
                  
                  (black . ("black" "--quiet" "--line-length" "88" "--stdin-filename"
                           ,(lambda () (or (buffer-file-name) "stdin")) "-"))
                  
                  (isort . ("isort" "--quiet" "--stdout" "--profile" "black"
                           "--filename" ,(lambda () (or (buffer-file-name) "stdin")) "-"))
                  
                  (shfmt . ("shfmt" "-i" "2" "-ci"))
                  
                  ;; Add ruff formatter for Python if available
                  ,@(when (executable-find "ruff")
                      `((ruff . ("ruff" "format" "--stdin-filename" 
                                ,(lambda () (or (buffer-file-name) "stdin")) "-"))))
                  
                  ;; Prettier for various web formats
                  ,@(when (executable-find "prettier")
                      `((prettier-yaml . ("prettier" "--parser" "yaml" "--stdin-filepath"
                                         ,(lambda () (or (buffer-file-name) "stdin"))))
                        (prettier-json . ("prettier" "--parser" "json" "--stdin-filepath"
                                         ,(lambda () (or (buffer-file-name) "stdin"))))
                        (prettier-css . ("prettier" "--parser" "css" "--stdin-filepath"
                                        ,(lambda () (or (buffer-file-name) "stdin"))))
                        (prettier-html . ("prettier" "--parser" "html" "--stdin-filepath"
                                         ,(lambda () (or (buffer-file-name) "stdin"))))))
                  
                  ;; SQL formatter if available
                  ,@(when (executable-find "sqlformat")
                      `((sqlformat . ("sqlformat" "--reindent" "--keywords" "upper" 
                                     "--identifiers" "lower" "-"))))
                  
                  ;; LaTeX formatter if available
                  ,@(when (executable-find "latexindent")
                      `((latexindent . ("latexindent" "--local" "--silent")))))))
  
  ;; Mode associations with intelligent formatter selection
  (setq apheleia-mode-alist
        (append apheleia-mode-alist
                `((c-ts-mode . clang-format)
                  (c++-ts-mode . clang-format)
                  (c-mode . clang-format)
                  (c++-mode . clang-format)
                  
                  ;; Python formatting - use ruff if available, otherwise black + isort
                  ,@(if (executable-find "ruff")
                        `((python-ts-mode . ruff)
                          (python-mode . ruff))
                      `((python-ts-mode . (isort black))
                        (python-mode . (isort black))))
                  
                  (bash-ts-mode . shfmt)
                  (sh-mode . shfmt)
                  
                  ;; Web formats with prettier
                  ,@(when (executable-find "prettier")
                      `((yaml-mode . prettier-yaml)
                        (yaml-ts-mode . prettier-yaml)
                        (json-mode . prettier-json)
                        (json-ts-mode . prettier-json)
                        (css-mode . prettier-css)
                        (css-ts-mode . prettier-css)
                        (html-mode . prettier-html)
                        (web-mode . prettier-html)))
                  
                  ;; SQL formatting
                  ,@(when (executable-find "sqlformat")
                      `((sql-mode . sqlformat)))
                  
                  ;; LaTeX formatting
                  ,@(when (executable-find "latexindent")
                      `((latex-mode . latexindent)
                        (LaTeX-mode . latexindent))))))
  
  ;; Enhanced save hook with better error handling
  (defun apheleia-format-on-save-safe ()
    "Safe format on save that handles errors gracefully."
    (when (and apheleia-mode
               (buffer-modified-p)
               (not buffer-read-only)
               (not (bound-and-true-p apheleia-temporarily-disabled)))
      (condition-case err
          (apheleia-format-buffer)
        (error
         (message "Apheleia formatting failed: %s" (error-message-string err))
         ;; Don't prevent saving due to formatting errors
         nil))))
  
  ;; Replace default save hook with safer version
  (remove-hook 'before-save-hook #'apheleia-format-buffer)
  (add-hook 'before-save-hook #'apheleia-format-on-save-safe)
  
  :bind (("C-c a f" . apheleia-format-buffer)
         ("C-c a t" . apheleia-mode)
         ("C-c a r" . apheleia-restart)))

;; *** Apheleia Extensions

(with-eval-after-load 'apheleia
  ;; Custom formatters for additional languages
  
  ;; Emacs Lisp formatting using built-in functions
  (defun apheleia-elisp-format-buffer (input)
    "Format Emacs Lisp buffer by reindenting."
    (with-temp-buffer
      (insert input)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (condition-case nil
          (while (not (eobp))
            (condition-case nil
                (progn
                  (indent-sexp)
                  (forward-sexp 1))
              (scan-error 
               (forward-line 1))))
        (error nil))
      (buffer-string)))
  
  ;; Add Emacs Lisp formatter
  (add-to-list 'apheleia-formatters '(elisp-indent . apheleia-elisp-format-buffer))
  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . elisp-indent))
  (add-to-list 'apheleia-mode-alist '(lisp-interaction-mode . elisp-indent)))

;; *** Project-Specific Configuration

(with-eval-after-load 'apheleia
  (defvar apheleia-project-formatters-alist
    '(;; Python projects with ruff configuration
      (("pyproject.toml" "ruff.toml" ".ruff.toml")
       (python-ts-mode . ruff)
       (python-mode . ruff))
      
      ;; Python projects with black configuration
      (("pyproject.toml" "setup.cfg" ".black")
       (python-ts-mode . (isort black))
       (python-mode . (isort black)))
      
      ;; C/C++ projects with clang-format config
      ((".clang-format" "_clang-format")
       (c-ts-mode . clang-format)
       (c++-ts-mode . clang-format)
       (c-mode . clang-format)
       (c++-mode . clang-format))
      
      ;; Web projects with prettier config
      ((".prettierrc" ".prettierrc.json" ".prettierrc.js" "prettier.config.js")
       (json-mode . prettier-json)
       (yaml-mode . prettier-yaml)
       (css-mode . prettier-css)
       (html-mode . prettier-html)))
    "Alist of project root patterns to formatter configurations.")
  
  (defun apheleia-find-project-root ()
    "Find the project root directory."
    (or (when (fboundp 'project-root)
          (when-let ((project (project-current)))
            (project-root project)))
        (locate-dominating-file default-directory ".git")
        (locate-dominating-file default-directory ".projectile")
        (locate-dominating-file default-directory "pyproject.toml")
        (locate-dominating-file default-directory "package.json")
        default-directory))
  
  (defun apheleia-formatter-available-p (formatter)
    "Check if FORMATTER is available on the system."
    (cond
     ((symbolp formatter)
      (let ((formatter-def (alist-get formatter apheleia-formatters)))
        (cond
         ((functionp formatter-def) t) ; Function formatters are always available
         ((listp formatter-def)
          (let ((command (car formatter-def)))
            (if (stringp command)
                (executable-find command)
              t)))
         (t nil))))
     ((listp formatter)
      (cl-every #'apheleia-formatter-available-p formatter))
     (t t)))
  
  (defun apheleia-detect-project-formatters ()
    "Detect project-specific formatters based on project files."
    (when-let* ((project-root (apheleia-find-project-root))
                (config (cl-find-if
                         (lambda (entry)
                           (let ((patterns (car entry)))
                             (cl-some (lambda (pattern)
                                        (file-exists-p (expand-file-name pattern project-root)))
                                      patterns)))
                         apheleia-project-formatters-alist)))
      (let ((formatters (cdr config)))
        (when-let ((formatter (alist-get major-mode formatters)))
          ;; Check if formatter is available before setting it
          (when (apheleia-formatter-available-p formatter)
            (setq-local apheleia-mode-alist
                        (cons (cons major-mode formatter)
                              (cl-remove major-mode apheleia-mode-alist :key #'car))))))))
  
  (add-hook 'apheleia-mode-hook #'apheleia-detect-project-formatters))

;; *** Enhanced Integration and Utilities

(with-eval-after-load 'apheleia
  ;; Function to check formatter availability
  (defun apheleia-check-formatters ()
    "Check which formatters are available on the system."
    (interactive)
    (let ((available-formatters '())
          (missing-formatters '()))
      (dolist (formatter apheleia-formatters)
        (let ((name (car formatter))
              (definition (cdr formatter)))
          (if (apheleia-formatter-available-p name)
              (push name available-formatters)
            (push name missing-formatters))))
      (with-help-window "*Apheleia Formatters*"
        (princ "Available formatters:\n")
        (dolist (formatter (sort available-formatters #'string<))
          (princ (format "  ✓ %s\n" formatter)))
        (when missing-formatters
          (princ "\nMissing formatters:\n")
          (dolist (formatter (sort missing-formatters #'string<))
            (princ (format "  ✗ %s\n" formatter)))))))
  
  ;; Function to show current formatter for buffer
  (defun apheleia-show-current-formatter ()
    "Show the current formatter(s) for this buffer."
    (interactive)
    (if (bound-and-true-p apheleia-mode)
        (condition-case nil
            (let ((formatters (apheleia--get-formatters)))
              (if formatters
                  (message "Current formatter(s): %s"
                           (mapconcat (lambda (f) 
                                        (if (symbolp f) (symbol-name f) (format "%s" f)))
                                      formatters ", "))
                (message "No formatter configured for %s" major-mode)))
          (error (message "Error getting formatter information")))
      (message "Apheleia mode not enabled")))
  
  ;; Function to temporarily disable formatting
  (defvar-local apheleia-temporarily-disabled nil
    "Whether apheleia is temporarily disabled for this buffer.")
  
  (defun apheleia-toggle-temporary-disable ()
    "Temporarily disable/enable apheleia for current buffer."
    (interactive)
    (setq apheleia-temporarily-disabled (not apheleia-temporarily-disabled))
    (message "Apheleia %s for this buffer"
             (if apheleia-temporarily-disabled "disabled" "enabled")))
  
  ;; Success message display
  (defun apheleia-show-success-message ()
    "Show success message after formatting."
    (condition-case nil
        (let ((formatters (apheleia--get-formatters)))
          (when formatters
            (let ((formatter-names (mapcar (lambda (f) 
                                             (if (symbolp f) (symbol-name f) (format "%s" f)))
                                           formatters)))
              (message "✓ Formatted with %s" 
                       (string-join formatter-names ", ")))))
      (error nil)))
  
  ;; Hook for successful formatting
  (add-hook 'apheleia-post-format-hook #'apheleia-show-success-message)
  
  ;; Enhanced Eglot integration
  (defun apheleia-eglot-integration ()
    "Ensure apheleia coordinates with eglot."
    (when (and apheleia-mode
               (bound-and-true-p eglot--managed-mode))
      ;; Add hook to trigger diagnostics refresh after formatting
      (add-hook 'apheleia-post-format-hook
                (lambda () 
                  (when (and (buffer-live-p (current-buffer))
                             (bound-and-true-p eglot--managed-mode))
                    (run-with-timer 0.5 nil 
                                    (lambda ()
                                      (when (buffer-live-p (current-buffer))
                                        (eglot-code-actions nil nil 'refresh-only))))))
                nil t)))
  
  (add-hook 'eglot-managed-mode-hook #'apheleia-eglot-integration)
  
  ;; Additional keybindings
  (when (bound-and-true-p apheleia-mode-map)
    (define-key apheleia-mode-map (kbd "C-c a c") #'apheleia-check-formatters)
    (define-key apheleia-mode-map (kbd "C-c a s") #'apheleia-show-current-formatter)
    (define-key apheleia-mode-map (kbd "C-c a d") #'apheleia-toggle-temporary-disable)))

;; *** Performance Optimizations

(with-eval-after-load 'apheleia
  ;; Cache formatter availability to avoid repeated executable-find calls
  (defvar apheleia-formatter-cache (make-hash-table :test 'eq)
    "Cache for formatter availability.")
  
  (defvar apheleia-cache-expiry-time 300  ; 5 minutes
    "Time in seconds after which to expire formatter cache.")
  
  (defvar apheleia-cache-timestamp (make-hash-table :test 'eq)
    "Timestamps for cache entries.")
  
  (defun apheleia-formatter-available-cached-p (formatter)
    "Check if FORMATTER is available, using cache with expiry."
    (let* ((current-time (float-time))
           (cached-time (gethash formatter apheleia-cache-timestamp 0))
           (cached-result (gethash formatter apheleia-formatter-cache 'not-found)))
      (if (and (not (eq cached-result 'not-found))
               (< (- current-time cached-time) apheleia-cache-expiry-time))
          cached-result
        (let ((available (apheleia-formatter-available-p formatter)))
          (puthash formatter available apheleia-formatter-cache)
          (puthash formatter current-time apheleia-cache-timestamp)
          available))))
  
  ;; Clear cache when formatters are modified
  (defun apheleia-clear-formatter-cache ()
    "Clear the formatter availability cache."
    (interactive)
    (clrhash apheleia-formatter-cache)
    (clrhash apheleia-cache-timestamp)
    (message "Apheleia formatter cache cleared"))
  
  ;; Advice to use cached version for better performance
  (advice-add 'apheleia-formatter-available-p :override #'apheleia-formatter-available-cached-p))

;; *** Optional Modeline Integration

(with-eval-after-load 'apheleia
  (defun apheleia-modeline-segment ()
    "Return apheleia status for modeline."
    (when (bound-and-true-p apheleia-mode)
      (let ((formatters (ignore-errors (apheleia--get-formatters))))
        (if formatters
            (propertize " Φ" 
                        'face '(:foreground "#b8bb26" :weight bold)
                        'help-echo (format "Apheleia: %s" 
                                          (mapconcat #'symbol-name formatters ", ")))
          (propertize " Φ" 
                      'face '(:foreground "#928374")
                      'help-echo "Apheleia: no formatter")))))
  
  ;; Function to add to your modeline format if desired
  (defun apheleia-enable-modeline ()
    "Add apheleia status to modeline."
    (interactive)
    (unless (member '(:eval (apheleia-modeline-segment)) mode-line-format)
      (setq mode-line-format
            (append mode-line-format '((:eval (apheleia-modeline-segment)))))))
  
  ;; Uncomment to enable modeline integration automatically
  ;; (add-hook 'apheleia-mode-hook #'apheleia-enable-modeline)
  )

(provide 'apheleia-config)

``````
