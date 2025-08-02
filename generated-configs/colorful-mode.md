``````el
(use-package colorful-mode
  ;; :diminish
  ;; :ensure t ; Optional
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

(add-hook 'post-command-hook
          (lambda ()
            "delete colorful overlay on active mark"
            (when-let* (colorful-mode
                        (beg (use-region-beginning))
                        (end (use-region-end)))
              ;; Remove full colorful overlay instead only the part where
              ;; the region is.
                  (dolist (ov (overlays-in beg end))
                    (when (overlay-get ov 'colorful--overlay)
                      (delete-overlay ov))))))

(add-hook 'deactivate-mark-hook
          (lambda ()
            "refontify deleted mark"
            (when-let* (colorful-mode
                        (beg (region-beginning))
                        (end (region-end)))
              (font-lock-flush beg end))))

``````
