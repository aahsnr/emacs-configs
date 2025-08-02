;;; -*- lexical-binding: t; -*-    
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)
;; Load the literate configuration
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
