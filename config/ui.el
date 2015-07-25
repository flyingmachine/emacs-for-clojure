(menu-bar-mode -1)

(when (fboundp 'global-line-mode)
  (global-linum-mode))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))


(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (display-graphic-p)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (add-to-list 'load-path "~/.emacs.d/themes")
  (load-theme 'tomorrow-night-bright t))

(defun set-frame-size-via-resolution ()
  (interactive)
  (if (display-graphic-p)
      (progn
	(if (> (x-display-pixel-width) 1280)
	    (progn
	      (add-to-list 'default-frame-alist (cons 'width 120))
	      (set-face-attribute 'default nil :height 140))
	    (progn
	      (add-to-list 'default-frame-alist (cons 'width 80))
	      (set-face-attribute 'default nil :height 120)))
	(add-to-list 'default-frame-alist
		     (cons 'height (/ (- (x-display-pixel-height) 200)
				      (frame-char-height)))))))

(when (display-graphic-p)
  (set-frame-size-via-resolution))
	  

;; increase font size for better readability
;;(set-face-attribute 'default nil :height 140)

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it
;;(setq initial-frame-alist '((top . 0) (left . 0) (width . 80) (height . 53)))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)
