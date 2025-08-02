(use-package! org-modern
  :after org
  :config
  ;; Custom headline bullets - this is the main customization
  (setq org-modern-star
        '("◉" "○" "◈" "◇" "◆" "▷"))

  ;; Custom list bullets
  ;; (setq org-modern-list
  ;;       '((43 . "➤")   ; +
  ;;         (45 . "–")   ; -
  ;;         (42 . "•"))) ; *

  ;; Optional: Customize specific elements if needed
  ;; Uncomment and modify as desired

  (setq org-modern-table-vertical 1
        org-modern-table-horizontal 0.1)

  (setq org-modern-block-name
        '(("src" "»" "«")
          ("example" "»–" "–«")
          ("quote" "❝" "❞"))))

  ;; Simple tag styling that works with any theme
  ;; (setq org-modern-tag-faces
  ;;       '(("work" :inverse-video t :weight bold)
  ;;         ("home" :inverse-video t :weight bold)
  ;;         ("project" :inverse-video t :weight bold)))
  

