;; Disable package.el completely
;; (setq package-enable-at-startup nil
;;       package--init-file-ensured t
;;       package-quickstart nil)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024)  ; 16 MB
                  gc-cons-percentage 0.1)))
;; (setq debug-on-error t)

;; Enable native compilation
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation t)

;;fixed-native--compile-async-skip-p
(defun fixed-native--compile-async-skip-p
    (native--compile-async-skip-p file load selector)
  (let* ((naive-elc-file (file-name-with-extension file "elc"))
         (elc-file       (replace-regexp-in-string
                          "\\.el\\.elc$" ".elc" naive-elc-file)))
    (or (gethash elc-file comp--no-native-compile)
        (funcall native--compile-async-skip-p file load selector))))

(advice-add 'native--compile-async-skip-p
            :around 'fixed-native--compile-async-skip-p)

;; Disable unwanted UI elements
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      warning-minimum-level :error
      create-lockfiles nil
      use-file-dialog nil
      use-dialog-box nil
      pop-up-windows nil
      visible-bell nil
      inhibit-splash-screen t)

;; Major Defaults
(setq fast-but-imprecise-scrolling 1
      pixel-scroll-precision-mode t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(save-place-mode 1)
(column-number-mode)
(global-auto-revert-mode t)  ;; Automatically show changes if the file has changed
(defalias 'yes-or-no-p 'y-or-n-p)
(auto-save-visited-mode t)
(delete-selection-mode 1)

;; Remove messages from the *Messages* buffer.
(setq-default message-log-max nil)

(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t
      redisplay-skip-fontification-on-input t
      inhibit-compacting-font-caches t)

;; Native compilation settings
(setq native-comp-deferred-compilation t
      native-comp-async-report-warnings-errors nil
      native-comp-jit-compilation t
      comp-speed 3
      native-compile-target-directory (expand-file-name "eln-cache/" user-emacs-directory))

;; Kill these buffers on startup.
(kill-buffer "*Messages*")
(kill-buffer "*scratch*")
;;(kill-buffer "*Async-native-compile-log*")
