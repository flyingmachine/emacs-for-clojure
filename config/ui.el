;;;;
;; UI related configurations.
;;;;


;; Go straight to scratch buffer on startup
(graphic-supported-p
 (safe-setq inhibit-startup-message t))

;; Disable menu bar
(safe-call menu-bar-mode -1)

;; Enable linum mode
(safe-call global-linum-mode)

;; Disable tool bar
(safe-call tool-bar-mode -1)

;; Disable scroll bar
(safe-call scroll-bar-mode -1)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)


;; Set font based on platform

(defmacro font-exists-p (font)
  "Return t if font exists"
  `(when (find-font (font-spec :name ,font))
     t))

(defmacro set-default-font! (font)
  `(when (font-exists-p ,font)
     (add-to-list 'default-frame-alist (cons 'font  ,font))
     (set-face-attribute 'default t :font ,font)
     (set-face-attribute 'default nil :font ,font)
     (version-supported-if <= 24.0
                           (set-frame-font ,font nil t)
                           (set-frame-font ,font))))

(defmacro set-cjk-font! (font)
  `(let ((name (car ,font))
         (size (cdr ,font)))
     (when (font-exists-p name)
       (safe-do
        set-fontset-font
        (dolist (c '(han kana cjk-misc))
          (set-fontset-font (frame-parameter nil 'font)
                            c (font-spec :family name
                                         :size size)))))))

(version-supported-p
 <= 24.0 
 (let* ((os system-type)
        (d (intern (format "private-%s-font" os)))
        (cjk (intern (format "private-%s-cjk-font" os))))
   (safe-setq* d
               (set-default-font! (symbol-value d)))
   (safe-setq* cjk
               (set-cjk-font! (symbol-value cjk)))))


;; Load themes on graphic mode
(graphic-supported-p
 (let ((themes-dir "~/.emacs.d/themes"))
   (add-to-list 'custom-theme-load-path themes-dir)
   (add-to-list 'load-path themes-dir)
   (version-supported-if >= 24.1
                         (load-theme 'tomorrow-night-eighties)
                         (load-theme 'tomorrow-night-eighties t))
   ;; don't pop up font menu
   (global-set-key (kbd "s-t") '(lambda () (interactive)))
   (desktop-save-mode 1)))


;; Line number format on Terminal
(terminal-supported-p
 (safe-setq linum-format "%2d "))


;; These settings relate to how emacs interacts with your platform

;; makes killing/yanking interact with the clipboard
(safe-setq x-select-enable-clipboard t)

;; I'm actually not sure what this does but it's recommended?
;; http://emacswiki.org/emacs/CopyAndPaste
(safe-setq x-select-enable-primary t)

;; Save clipboard strings into kill ring before replacing them.
;; When one selects something in another program to paste it into Emacs,
;; but kills something in Emacs before actually pasting it,
;; this selection is gone unless this variable is non-nil
(safe-setq x-select-enable-clipboard t)

(safe-setq save-interprogram-paste-before-kill t)

;; Mouse yank commands yank at point instead of at click.
(safe-setq mouse-yank-at-point t)

;; Shows all options when running apropos. For more info,
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
;;apropos-do-all t
(safe-setq apropos-do-all t)

;; No cursor blinking, it's distracting
(safe-call blink-cursor-mode 0)

;; full path in title bar
(safe-setq*
 'frame-title-format
 (setq-default frame-title-format "%b (%f)"))

;; Ignore ring bell
(safe-setq ring-bell-function 'ignore)

