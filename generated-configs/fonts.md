``````el
;; Font Configuration for Emacs with Nerd Fonts
;; Compatible with GUI Emacs, emacsclient, and terminal Emacs

;;; Configuration Variables

(defcustom my-nerd-font "JetBrainsMono Nerd Font"
  "The Nerd Font family to use in GUI Emacs."
  :type 'string
  :group 'faces)

(defcustom my-font-size 110
  "Font size (height) for GUI Emacs. 110 = 11pt."
  :type 'integer
  :group 'faces)

;;; Font Setup Functions

(defun setup-gui-fonts ()
  "Configure fonts for GUI Emacs with Nerd Font support."
  (when (find-font (font-spec :name my-nerd-font))
    ;; Configure default font
    (set-face-attribute 'default nil
                        :family my-nerd-font
                        :height my-font-size
                        :weight 'normal)
    
    ;; Configure fixed-pitch font for code and monospace content
    (set-face-attribute 'fixed-pitch nil
                        :family my-nerd-font
                        :height my-font-size)
    
    ;; Configure variable-pitch font for better text readability
    (set-face-attribute 'variable-pitch nil
                        :family "Ubuntu Nerd Font"
                        :height (+ my-font-size 10))
    
    ;; Configure fontsets for Nerd Font symbols
    (setup-fontset-unicode-coverage)
    
    (message "GUI fonts configured with %s" my-nerd-font))
  
  (unless (find-font (font-spec :name my-nerd-font))
    (message "Warning: %s not found. Please install a Nerd Font." my-nerd-font)))

(defun setup-fontset-unicode-coverage ()
  "Configure fontsets for Nerd Font symbol display."
  ;; Nerd Font private use areas (where most icons are located)
  (set-fontset-font t '(#xe000 . #xf8ff) my-nerd-font nil 'prepend)
  
  ;; Additional Unicode blocks for symbols and drawing characters
  (set-fontset-font t '(#x2190 . #x21ff) my-nerd-font nil 'prepend) ; Arrows
  (set-fontset-font t '(#x2500 . #x257f) my-nerd-font nil 'prepend) ; Box Drawing
  (set-fontset-font t '(#x25a0 . #x25ff) my-nerd-font nil 'prepend) ; Geometric Shapes
  (set-fontset-font t '(#x2600 . #x26ff) my-nerd-font nil 'prepend) ; Miscellaneous Symbols
  (set-fontset-font t '(#x2700 . #x27bf) my-nerd-font nil 'prepend) ; Dingbats
  (set-fontset-font t '(#x1f300 . #x1f5ff) my-nerd-font nil 'prepend)) ; Miscellaneous Symbols and Pictographs

(defun setup-terminal-fonts ()
  "Configure terminal Emacs for optimal UTF-8 and Unicode display."
  ;; Set comprehensive UTF-8 encoding
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  
  ;; Configure language environment
  (set-language-environment "UTF-8")
  
  ;; Optimize Unicode character display
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  
  ;; Performance optimizations for terminal display
  (setq-default bidi-display-reordering nil)
  
  (message "Terminal fonts configured for UTF-8 display"))

;;; Main Configuration Function

(defun apply-font-configuration (&optional frame)
  "Apply appropriate font configuration based on display type.
If FRAME is provided, configure fonts for that specific frame."
  (let ((target-frame (or frame (selected-frame))))
    (with-selected-frame target-frame
      (if (display-graphic-p target-frame)
          (setup-gui-fonts)
        (setup-terminal-fonts)))))

;;; Initialize Font Configuration

;; Apply configuration immediately
(apply-font-configuration)

;; Configure fonts for new frames (essential for emacsclient)
(add-hook 'after-make-frame-functions #'apply-font-configuration)

;; Apply configuration during window setup
(add-hook 'window-setup-hook #'apply-font-configuration)

;; End of configuration


``````
