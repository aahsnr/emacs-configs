# 1st Iteration

``````el
;;; -*- lexical-binding: t; -*-

;;;; ------------------------------------------------------------------
;;;; Python IDE & Scientific Computing Configuration
;;;; ------------------------------------------------------------------
;; This section configures Python development to be a first-class IDE
;; experience, both for standard .py files and for literate programming
;; within Org Mode source blocks.

;;;
;;; 1. Core Python Environment (`.py` files)
;;;
;; We will configure the formatter, linter, and debugger to use your
;; preferred tools. Doom's `+pyright` flag already sets up the LSP server.

(use-package! python
  :config
  ;; Ensure all python tools respect the project's virtual environment.
  ;; Doom's `+poetry` flag and `:tools direnv` module handle most of this.
  ;; Make sure to run `direnv allow` in your project directories.
  (setq python-shell-interpreter "python")
  (setq dap-python-executable "python"))

;;;###+section: Formatter (Black)
;; Doom's `:editor (format +onsave)` module uses `apheleia` under the hood.
;; We'll configure it to use `black` as the formatter for python files.
(use-package! apheleia
  :after python
  :config
  (setf (alist-get 'python apheleia-formatters) '(black :inplace t))
  (setf (alist-get 'python-ts-mode apheleia-formatters) '(black :inplace t)))

;;;###+section: Linter (Pylint)
;; Doom's `:checkers syntax` module uses `flycheck`. We'll add `pylint` to the
;; checker chain and make it the default.
(use-package! flycheck
  :after python
  :config
  (flycheck-add-next-checker 'python-pylint 'python-pyright 'append))

;;;###+section: Debugger (dap-mode with debugpy)
;; The `:tools (debugger +lsp)` module provides `dap-mode`. We'll add
;; convenient debug templates for common Python workflows.
(use-package! dap-python
  :after dap-mode
  :config
  ;; Tell dap-mode where to find the debugpy adapter. It's usually installed
  ;; within your project's poetry environment.
  (require 'dap-python)
  (dap-python-setup)

  ;; Define debug configurations for different scenarios.
  ;; These will be available via `SPC d d` (dap-debug).
  (dap-register-debug-template
   "Python: Debug Current File"
   (list :type "python"
         :request "launch"
         :name "Debug Current File"
         :program "${file}"))

  (dap-register-debug-template
   "Python: Debug Pytest File"
   (list :type "python"
         :request "launch"
         :name "Debug Pytest File"
         :module "pytest"
         :args ["${file}"]))

  (dap-register-debug-template
   "Python: Attach to Remote"
   (list :type "python"
         :request "attach"
         :name "Attach to Remote Process"
         :connect (list :host "localhost" :port 5678))))

;;;
;;; 2. Org Mode & Jupyter Integration
;;;
;; This section ensures that our IDE features are available within
;; Python source blocks in Org files.

;;;###+section: Enable LSP, Completion, and Linting in Org Src Blocks
;; `lsp-org` bridges the gap between LSP and Org's source editing buffers.
(use-package! lsp-org
  :after lsp
  :config
  ;; We want lsp-mode to be active in our python source blocks.
  (add-to-list 'lsp-org-major-modes 'python-ts-mode))

;;;###+section: Formatting Org Src Blocks
;; Since format-on-save doesn't apply to temporary src block buffers,
;; we'll create an interactive function to format them on demand.
(defun +python/format-org-src-block ()
  "Format the current Python org source block using black."
  (interactive)
  (when (string-equal-ignore-case (org-babel-get-src-block-info 'lang) "python")
    (let ((apheleia-formatters '((python-ts-mode . (black :inplace t)))))
      (apheleia-format-buffer))))

;;;###+section: Jupyter & Org Babel
;; Enhance Doom's default Jupyter integration.
(after! ob-jupyter
  ;; Set a default session so we don't have to specify `:session` on every block.
  (setq org-babel-default-header-args:jupyter-python
        '((:session . "main")
          (:kernel . "python3")))

  ;; Ensure matplotlib plots are rendered as high-quality files for export.
  (setq org-babel-jupyter-svg-emacs-conversion-command "rsvg-convert -h %h %f -o %o")
  (setq org-babel-jupyter-png-emacs-conversion-command "convert %f -resize %wx %o")
  (setq org-babel-jupyter-image-format "svg")

  ;; A helper to restart the jupyter kernel for the current session.
  (defun +python/jupyter-restart-kernel ()
    "Restart the Jupyter kernel for the current session."
    (interactive)
    (let ((session (org-babel-jupyter-find-session-name-in-header-args)))
      (if (org-babel-jupyter-kernel-running-p session)
          (progn
            (org-babel-jupyter-restart-kernel session)
            (message "Jupyter kernel '%s' restarted." session))
        (message "No running kernel for session '%s'." session)))))

;;;###+section: Debugging Org Src Blocks with DAP
;; Debugging code within an Org file requires a special approach. We will use
;; `ob-dap` to bridge Org Babel and `dap-mode`. It works by tangling the
-;; specific source block to a temporary file and launching a debug session.
(use-package! ob-dap
  :after org
  :config
  ;; Associate the "python" language with the `dap-python` debugger.
  (add-to-list 'ob-dap-supported-languages '("python" . dap-python)))

;;;
;;; 3. Keybindings
;;;
;; A unified set of keybindings for Python development across all modes.

;;;###+section: Python Mode Keybindings
(map! :leader
      :map python-mode-map
      :prefix ("c" . "compile")
      "f" #'dap-python-debug ; `f` for "file"
      "t" #'(lambda () (interactive) (dap-debug-edit-template "Python: Debug Pytest File" 'vsplit)) ; `t` for "test"
      "r" #'dap-debug-recent)

(map! :leader
      :map python-mode-map
      :prefix ("f" . "format")
      "f" #'apheleia-format-buffer)

;;;###+section: Org Mode Src Block Keybindings
(map! :leader
      :map org-mode-map
      :localleader
      :prefix ("c" . "code")
      "d" #'ob-dap-debug ; Debug the current src block
      "f" #'+python/format-org-src-block ; Format the current src block
      "k" #'+python/jupyter-restart-kernel) ; Restart Jupyter kernel

``````
