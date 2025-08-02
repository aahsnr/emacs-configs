# 2nd Iteration

``````el
;;; -*- lexical-binding: t; -*-

;;;; ------------------------------------------------------------------
;;;; Python IDE & Scientific Computing Configuration (Iteration 2)
;;;; ------------------------------------------------------------------
;; This configuration enhances Doom's Python support, focusing on the tools
;; you've requested. It uses `after!` to modify behavior for modules already
;; loaded by your `init.el`, ensuring better integration.

;;;
;;; 1. Core Python Environment
;;; This section fine-tunes the behavior for `.py` files. Your `init.el`
;;; already enables `+pyright` for LSP and `+tree-sitter`.

;;;###+section: Formatter (Black)
;; Doom's `:editor (format +onsave)` module uses `apheleia`. We'll ensure
;; `black` is the designated formatter for Python. Apheleia is smart enough
;; to find `black` in your project's virtual environment (Poetry/venv).
(after! python
  (set-formatter! 'python-black "black --fast -")
  (set-formatter! 'python-isort "isort --stdout --profile black -"))

;; Apply formatting chain on save: first isort, then black.
(setq-hook! '(python-mode-hook python-ts-mode-hook)
  format-on-save-enabled t
  +format-with-lsp nil ; Prefer apheleia over LSP for formatting consistency
  apheleia-formatters '(python-isort python-black))

;;;###+section: Linter (Pylint)
;; We configure `flycheck` (from the `:checkers syntax` module) to prioritize `pylint`.
;; Pyright (LSP) will still provide diagnostics, giving you comprehensive coverage.
(after! flycheck
  (flycheck-add-checker 'python-pylint
    '("pylint"
      (source . t)
      (error-patterns
       ("L" (message "C" line col " " (id "C" num) ": " (message)) 1)
       ("L" (message "R" line col " " (id "R" num) ": " (message)) 1)
       ("L" (message "W" line col " " (id "W" num) ": " (message)) 2)
       ("L" (message "E" line col " " (id "E" num) ": " (message)) 3)
       ("L" (message "F" line col " " (id "F" num) ": " (message)) 3))
      (error-filter (lambda (errors)
                      (if (string-match-p "No config file found" (flycheck-error-message (car errors)))
                          nil
                        errors)))
      (modes (python-mode python-ts-mode)))))

(after! python
  ;; Add pylint to the chain and give it priority over pyright's basic linting.
  (flycheck-add-next-checker 'python-pyright 'python-pylint 'append))


;;;
;;; 2. Debugging with `dap-mode` and `debugpy`
;;;

(after! dap-python
  ;; `dap-python` automatically finds `debugpy` in your virtual env.
  (require 'dap-python)
  (dap-python-setup)
  (setq dap-python-debugger 'debugpy)

  ;; Define debug configurations for different scenarios.
  ;; Access them via `SPC m d d` (dap-debug).
  (dap-register-debug-template
   "Python: Debug Current File"
   (list :type "python"
         :request "launch"
         :name "DAP: Debug Current File"
         :program "${file}"
         :console "integratedTerminal"))

  (dap-register-debug-template
   "Python: Debug Pytest File"
   (list :type "python"
         :request "launch"
         :name "DAP: Debug Pytest File"
         :module "pytest"
         :args ["-s" "-v" "${file}"]
         :console "integratedTerminal")))


;;;
;;; 3. Org Mode & Jupyter Integration
;;;

;;;###+section: Enable IDE Features in Org Src Blocks
;; Configure `lsp-org` to activate LSP within Python source blocks. This is the
;; key to getting completion, navigation, and diagnostics.
(after! lsp-org
  (setq lsp-org-babel-enable t)
  (add-to-list 'lsp-org-major-modes 'python-ts-mode))

;;;###+section: Interactive Formatting for Org Src Blocks
(defun +python/format-org-src-block ()
  "Format the current Python org source block using black and isort."
  (interactive)
  (if (string-equal-ignore-case (org-babel-get-src-block-info 'lang) "python")
      (let ((apheleia-formatters '((python-ts-mode . (python-isort python-black)))))
        (apheleia-format-buffer)
        (message "Python source block formatted."))
    (message "Not in a Python source block.")))

;;;###+section: Jupyter & Org Babel Enhancements
(after! ob-jupyter
  ;; Common header arguments for all jupyter-python blocks.
  (setq org-babel-default-header-args:jupyter-python
        '((:session . "main")
          (:kernel . "python3")
          (:exports . "both")
          (:results . "output replace")
          (:cache . "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")))

  ;; For high-quality plots in exports.
  (setq org-babel-jupyter-image-format "svg")

  (defun +python/jupyter-restart-kernel ()
    "Restart the Jupyter kernel for the current org buffer's session."
    (interactive)
    (let ((session (org-babel-jupyter-find-session-name-in-header-args)))
      (if (org-babel-jupyter-kernel-running-p session)
          (progn
            (org-babel-jupyter-restart-kernel session)
            (message "Jupyter kernel '%s' restarted." session))
        (message "No running kernel for session '%s'." session)))))

;;;###+section: Debugging Org Src Blocks with `ob-dap`
(use-package! ob-dap
  :after org
  :config
  (add-to-list 'ob-dap-supported-languages '("python" . dap-python) 'append))

;;;
;;; 4. Testing with Pytest
;;;

(defun +python/run-pytest ()
  "Run pytest for the current project in a compilation buffer."
  (interactive)
  (let ((project-root (doom-project-root)))
    (compile (format "cd %s && poetry run pytest -v" (shell-quote-argument project-root)))))

(defun +python/run-pytest-current-file ()
  "Run pytest for the current file."
  (interactive)
  (let ((project-root (doom-project-root))
        (current-file (buffer-file-name)))
    (compile (format "cd %s && poetry run pytest -v %s"
                     (shell-quote-argument project-root)
                     (shell-quote-argument current-file)))))


;;;
;;; 5. Keybindings
;;;

;;;###+section: Python Mode Keybindings (`.py` files)
(map! :leader
      :map python-ts-mode-map
      :prefix ("m" . "major-mode")
      ;; Debugging (`SPC m d ...`)
      "dd" #'(lambda () (interactive) (dap-debug-by-name "DAP: Debug Current File"))
      "dt" #'(lambda () (interactive) (dap-debug-by-name "DAP: Debug Pytest File"))
      "dr" #'dap-debug-recent
      "dq" #'dap-quit
      ;; Testing (`SPC m t ...`)
      "tt" #'+python/run-pytest-current-file
      "tT" #'+python/run-pytest)

;;;###+section: Org Mode Src Block Keybindings
(map! :leader
      :map org-mode-map
      :localleader
      :prefix ("c" . "code")
      "d" #'ob-dap-debug                  ; Debug the current src block
      "f" #'+python/format-org-src-block  ; Format the current python src block
      "k" #'+python/jupyter-restart-kernel) ; Restart Jupyter kernel for buffer session

``````
