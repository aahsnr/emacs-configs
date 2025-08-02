;;; early-init.el --- Early init optimizations -*- lexical-binding: t -*-

;; --------------------------------------------------------------------------
;; This file loads BEFORE `init.el` or `config.el` and before package.el.
;; Its sole purpose is to configure settings that speed up Emacs startup.
;; Non-performance-related settings belong in the main configuration file.
;; --------------------------------------------------------------------------

;; --- Garbage Collection Tuning ---
;; Set a high GC threshold during startup to defer garbage collection.
;; This is a major performance win. The threshold will be restored to a
;; sane value later in the configuration.
(setq gc-cons-threshold (* 128 1024 1024)) ;; 128 MB
(setq gc-cons-percentage 0.6)
(setq-default message-log-max nil) ;; Drastically reduce messages during startup

;; --- Core Performance Optimizations ---
;; Disable file handlers at startup. This can speed up I/O operations.
(setq file-name-handler-alist nil)

;; Don't check if .elc files are newer than .el files. Assumes you recompile
;; packages when necessary (e.g., after an update).
(setq load-prefer-newer nil)

;; --- Native Compilation Settings ---
;; Configure native compilation as early as possible.
(when (fboundp 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-jit-compilation t
        comp-speed 3
        native-compile-target-directory (expand-file-name "eln-cache/" user-emacs-directory)))

;; --- UI Initialization Deferral ---
;; Prevent UI elements from being created at startup, as they are often disabled
;; or reconfigured later anyway. This is more efficient than disabling them
;; with modes after they have already been drawn.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq-default fringe-mode '(10 . 10)) ;; Give some breathing room

;; Inhibit other distracting UI elements.
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      inhibit-splash-screen t)

;; --- Debugging ---
;; Make startup errors more verbose if the DEBUG environment variable is set.
(when (getenv "DEBUG")
  (setq init-file-debug t
        debug-on-error t))

;; --- Clean Initial Buffers ---
;; Start with a clean slate by killing the initial scratch and messages buffers.
(defun ar/kill-initial-buffers (&rest args)
  "Kill the *scratch* and *Messages* buffers."
  (dolist (buf '("*scratch*" "*Messages*"))
    (when (get-buffer buf)
      (kill-buffer buf))))

(add-hook 'emacs-startup-hook #'ar/kill-initial-buffers)

(message "Early-init optimizations applied.")
