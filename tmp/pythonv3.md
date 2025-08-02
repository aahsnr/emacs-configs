# 3rd Iteration

``````el
;;; -*- lexical-binding: t; -*-

;;;; ------------------------------------------------------------------
;;;; Python IDE & Scientific Computing Configuration (Final Version)
;;;; ------------------------------------------------------------------
;; This configuration provides a seamless, IDE-like experience for Python
;; development. It enhances Doom's defaults by integrating your preferred
;; tools (`pyright`, `black`, `pylint`, `debugpy`) for both standard Python
;; files and literate programming in Org Mode with Jupyter.

;;;
;;; Tooling Recommendation:
;;; For the best experience, ensure your Python tools are managed by your
;;; project's package manager (e.g., Poetry). Add them as development
;;; dependencies in your `pyproject.toml`:
;;; `poetry add --group dev black isort pylint debugpy pytest`
;;; Doom's `direnv` and `+poetry` integration will then automatically
;;; make these tools available to Emacs.

;;;
;;; 1. Core Python Environment
;;; Fine-tunes the environment for `.py` files.
;;; ------------------------------------------------------------------

;;;###+section: Linter (Pylint)
;; Set the default Flycheck checker for Python to `pylint`. This is the
;; idiomatic Doom Emacs approach and is cleaner than manual checker chains.
;; Pyright (from LSP) will still provide real-time diagnostics alongside it.
(setq +python-checker 'pylint)

;;;###+section: Formatter (Black & Isort)
;; Configure a formatting chain using `apheleia` (Doom's default formatter).
;; On saving a Python buffer, `isort` will run first to sort imports,
;; followed by `black` to format the code.
(after! python
  (set-formatter! 'python-black "black --fast -")
  (set-formatter! 'python-isort "isort --stdout --profile black -"))

;; Enable the format-on-save chain for both python-mode and tree-sitter mode.
(setq-hook! '(python-mode-hook python-ts-mode-hook)
  format-on-save-enabled t  ; This is the trigger
  +format-with-lsp nil      ; Prefer apheleia over LSP for chain formatting
  apheleia-formatters '(python-isort python-black))


;;;
;;; 2. Debugging with `dap-mode` and `debugpy`
;;; ------------------------------------------------------------------

(after! dap-python
  (require 'dap-python)
  (dap-python-setup)
  (setq dap-python-debugger 'debugpy)

  ;; Define reusable debug configurations, accessible via `SPC m d d`.
  (dap-register-debug-template
   "Python: Debug Current File"
   (list :type "python"
         :request "launch"
         :name "DAP: Debug Current File"
         :program "${file}"
         :console "integratedTerminal"))

  (dap-register-debug-template
   "Python: Debug with Pytest"
   (list :type "python"
         :request "launch"
         :name "DAP: Debug with Pytest"
         :module "pytest"
         :args ["-s" "-v" "${file}"]
         :console "integratedTerminal")))


;;;
;;; 3. Org Mode & Jupyter Integration
;;; ------------------------------------------------------------------

;;;###+section: Enable IDE Features in Org Src Blocks
;; Configure `lsp-org` to enable full LSP support (completion, diagnostics,
;; code actions) within Python source blocks. Code completion via Corfu will
;; work automatically by sourcing candidates from the LSP server.
(after! lsp-org
  (setq lsp-org-babel-enable t)
  (add-to-list 'lsp-org-major-modes 'python-ts-mode))

;;;###+section: Interactive Formatting for Org Src Blocks
(defun +python/format-org-src-block ()
  "Format the current Python org source block using isort and black."
  (interactive)
  (if (string-equal-ignore-case (org-babel-get-src-block-info 'lang) "python")
      (let ((apheleia-formatters '((python-ts-mode . (python-isort python-black)))))
        (apheleia-format-buffer)
        (message "Python source block formatted."))
    (message "Not in a Python source block.")))

;;;###+section: Jupyter & Org Babel Enhancements
(after! ob-jupyter
  ;; Set convenient defaults for all jupyter-python blocks.
  (setq org-babel-default-header-args:jupyter-python
        '((:session . "default-py")
          (:kernel . "python3")
          (:exports . "both")
          (:results . "output replace")))

  ;; Ensure plots are exported as high-quality, scalable vector graphics.
  (setq org-babel-jupyter-image-format "svg")

  ;; Helper function to restart a Jupyter kernel.
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
;; This package provides the `ob-dap-debug` command, which seamlessly
;; integrates Org Babel with `dap-mode`.
(use-package! ob-dap
  :after org
  :config
  ;; Associate the "python" language with the `dap-python` debugger.
  (add-to-list 'ob-dap-supported-languages '("python" . dap-python)))

;;;
;;; 4. Testing with Pytest
;;; ------------------------------------------------------------------

(defun +python/get-test-at-point ()
  "Return the name of the pytest test function or class at point."
  (let ((ts-mode (bound-and-true-p treesit-major-mode)))
    (save-excursion
      (if ts-mode
          (let* ((node (treesit-node-at-point))
                 (query '((function_definition
                           name: (identifier) @_name
                           (#match? @_name "^test_"))
                          (class_definition
                           name: (identifier) @_name
                           (#match? @_name "^Test"))))
                 (captures (treesit-query-captures node query)))
            (when captures
              (buffer-substring-no-properties (treesit-node-start (cadar captures))
                                              (treesit-node-end (cadar captures)))))
        ;; Fallback for non-treesitter mode
        (beginning-of-line)
        (when (re-search-forward "^\\(def\\s_+\\(test_[^\\s_(]+\\)\\|class\\s_+\\(Test[^\\s_(:]+\\)\\)" (line-end-position) t)
          (or (match-string 2) (match-string 3)))))))

(defun +python/run-pytest-at-point ()
  "Run pytest on the test function or class at point."
  (interactive)
  (let* ((project-root (doom-project-root))
         (test-name (+python/get-test-at-point)))
    (if test-name
        (compile (format "cd %s && poetry run pytest -v %s::%s"
                         (shell-quote-argument project-root)
                         (shell-quote-argument (buffer-file-name))
                         test-name))
      (message "No pytest test found at point."))))

(defun +python/run-pytest-current-file ()
  "Run pytest for the current file."
  (interactive)
  (let ((project-root (doom-project-root)))
    (compile (format "cd %s && poetry run pytest -v %s"
                     (shell-quote-argument project-root)
                     (shell-quote-argument (buffer-file-name))))))

;;;
;;; 5. Consolidated Keybindings
;;; ------------------------------------------------------------------

;;;###+section: For Python Files (`.py`)
(map! :leader
      :map python-ts-mode-map
      :prefix ("m" . "Python")
      ;; Debugging (`SPC m d ...`)
      "dd" #'(lambda () (interactive) (dap-debug-by-name "DAP: Debug Current File"))
      "dt" #'(lambda () (interactive) (dap-debug-by-name "DAP: Debug with Pytest"))
      "dp" #'dap-python-debug-test-at-point ; Debug test at point
      "dr" #'dap-debug-recent
      "dq" #'dap-quit
      ;; Testing (`SPC m t ...`)
      "tp" #'+python/run-pytest-at-point    ; Test at point
      "tf" #'+python/run-pytest-current-file ; Test file
      "tP" #'(lambda () (interactive) (dired (doom-project-root)))) ; Test project (by opening dired to run manually)
      ;; REPL (`SPC m s ...`)
      "si" #'python-shell-send-region
      "sI" #'run-python)

;;;###+section: For Org Mode Python Blocks
(map! :leader
      :map org-mode-map
      :localleader
      :prefix ("c" . "code")
      "d" #'ob-dap-debug                     ; Debug current source block
      "f" #'+python/format-org-src-block     ; Format current python block
      "k" #'+python/jupyter-restart-kernel   ; Restart Jupyter kernel
      "s" #'ob-jupyter-select-kernel)        ; Select Jupyter kernel

``````
