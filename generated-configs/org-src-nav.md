``````el
(defun org-goto-first-src-block ()
  "Go to the beginning of the first source block in the buffer."
  (interactive)
  (let ((start-pos (point)))
    (goto-char (point-min))
    (if (re-search-forward "^[ \t]*#\\+begin_src\\b" nil t)
        (progn
          (beginning-of-line)
          (message "Moved to first source block"))
      (goto-char start-pos)
      (message "No source blocks found in buffer"))))

(defun org-goto-last-src-block ()
  "Go to the beginning of the last source block in the buffer."
  (interactive)
  (let ((start-pos (point)))
    (goto-char (point-max))
    (if (re-search-backward "^[ \t]*#\\+begin_src\\b" nil t)
        (progn
          (beginning-of-line)
          (message "Moved to last source block"))
      (goto-char start-pos)
      (message "No source blocks found in buffer"))))

(defun org-goto-next-src-block ()
  "Go to the next source block."
  (interactive)
  (let ((current-pos (point)))
    (end-of-line)
    (if (re-search-forward "^[ \t]*#\\+begin_src\\b" nil t)
        (progn
          (beginning-of-line)
          (message "Moved to next source block"))
      (goto-char current-pos)
      (message "No more source blocks found"))))

(defun org-goto-prev-src-block ()
  "Go to the previous source block."
  (interactive)
  (let ((current-pos (point)))
    (beginning-of-line)
    (if (re-search-backward "^[ \t]*#\\+begin_src\\b" nil t)
        (progn
          (beginning-of-line)
          (message "Moved to previous source block"))
      (goto-char current-pos)
      (message "No previous source blocks found"))))

  (general-evil-define-key '(normal visual) org-mode-map
    "gsg" 'org-goto-first-src-block
    "gsG" 'org-goto-last-src-block
    "gsj" 'org-goto-next-src-block
    "gsk" 'org-goto-prev-src-block)


``````
