(use-package move-text
  :commands (move-text-up move-text-down)
  :init
  (move-text-default-bindings)
  :config
  (defun my/indent-region-advice (&rest ignored)
    "Auto-indent moved text for better formatting."
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  
  (advice-add 'move-text-up :after 'my/indent-region-advice)
  (advice-add 'move-text-down :after 'my/indent-region-advice)) 

