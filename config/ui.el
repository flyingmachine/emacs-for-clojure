;; UI related configurations.

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (fboundp 'global-linum-mode)
  (global-linum-mode))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(defun set-frame-size-via-resolution ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (let* ((w (/ (x-display-pixel-width) (* 2 (frame-char-width))))
               (w? (>= w 90))
               (w1 (if w? 90 80))
               (h? (> (x-display-pixel-width) 1280))
               (h1 (if h? 112 96)))
          (add-to-list 'default-frame-alist (cons 'width w1))
          (set-face-attribute 'default nil :height h1))

        (let* ((h (/ (x-display-pixel-height) (frame-char-height)))
               (h1 (if (>= h 48) 48 24)))
          (add-to-list 'default-frame-alist (cons 'height h1))))))


(when (display-graphic-p)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (add-to-list 'load-path "~/.emacs.d/themes")
  (load-theme 'tomorrow-night-bright t)
  (set-frame-size-via-resolution)
  (desktop-save-mode 1))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      ;; http://emacswiki.org/emacs/CopyAndPaste
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
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode 0))

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)
