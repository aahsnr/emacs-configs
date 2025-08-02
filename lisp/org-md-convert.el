(defun markdown-to-org-region (start end)
  "Convert the selected region from Markdown to Org mode using pandoc."
  (interactive "r")
  (unless (region-active-p)
    (error "No region selected"))
  (unless (executable-find "pandoc")
    (error "Pandoc not found. Please install pandoc"))
  (let ((original-buffer (current-buffer))
        (markdown-text (buffer-substring-no-properties start end)))
    (with-temp-buffer
      (insert markdown-text)
      (let ((exit-code (shell-command-on-region 
                        (point-min) (point-max)
                        "pandoc -f markdown -t org"
                        (current-buffer) t)))
        (unless (zerop exit-code)
          (error "Pandoc conversion failed"))
        (let ((org-text (string-trim (buffer-substring-no-properties 
                                      (point-min) (point-max)))))
          (with-current-buffer original-buffer
            (delete-region start end)
            (goto-char start)
            (insert org-text)
            (deactivate-mark)
            (message "Converted region from Markdown to Org mode")))))))

(defun org-to-markdown-region (start end)
  "Convert the selected region from Org mode to Markdown using pandoc."
  (interactive "r")
  (unless (region-active-p)
    (error "No region selected"))
  (unless (executable-find "pandoc")
    (error "Pandoc not found. Please install pandoc"))
  (let ((original-buffer (current-buffer))
        (org-text (buffer-substring-no-properties start end)))
    (with-temp-buffer
      (insert org-text)
      (let ((exit-code (shell-command-on-region 
                        (point-min) (point-max)
                        "pandoc -f org -t markdown"
                        (current-buffer) t)))
        (unless (zerop exit-code)
          (error "Pandoc conversion failed"))
        (let ((markdown-text (string-trim (buffer-substring-no-properties 
                                           (point-min) (point-max)))))
          (with-current-buffer original-buffer
            (delete-region start end)
            (goto-char start)
            (insert markdown-text)
            (deactivate-mark)
            (message "Converted region from Org mode to Markdown")))))))

(provide 'org-md-convert)
