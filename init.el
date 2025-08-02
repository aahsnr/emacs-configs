;;; -*- lexical-binding: t; -*-
;;; -*- init.el: t; -*-
;; Set the location for custom variables.
(setq custom-file (locate-user-emacs-file "custom.el"))
;; comment out to prevent double loading of custom.el
;;(load custom-file 'noerror)

;;;; --- Disable Native Compile Warnings ---
(setq native-comp-async-report-warnings-errors 'silent
      native-comp-verbose 0
      warning-minimum-level :error)

;;;; --- Bootstrap Packages ---
;; This section is inspired by minimal-init.el for robust package handling.

(require 'package)

;; Define package archives.
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; Set priorities to prefer GNU ELPA for core packages.
(setq package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa"  . 70)))

;; Initialize the package system.
(package-initialize)

;; Install use-package if it's not already installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package.
(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t)

;; Add custom lisp directory to load-path
;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))    
(add-to-list 'load-path "~/.emacs.d/lisp")

;;;; --- Load Literate Configuration ---
;; All custom configuration is in config.org.
;;(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
      
(org-babel-load-file "~/.emacs.d/config.org")
 
;;; init.el ends here
