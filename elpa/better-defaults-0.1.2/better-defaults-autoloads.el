;;; better-defaults-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "better-defaults" "better-defaults.el" (21709
;;;;;;  4304 0 0))
;;; Generated autoloads from better-defaults.el

(ido-mode t)

(setq ido-enable-flex-matching t)

(menu-bar-mode -1)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'uniquify)

(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)

(setq-default save-place t)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)

(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(global-set-key (kbd "C-M-s") 'isearch-forward)

(global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)

(setq-default indent-tabs-mode nil)

(setq x-select-enable-clipboard t x-select-enable-primary t save-interprogram-paste-before-kill t apropos-do-all t mouse-yank-at-point t save-place-file (concat user-emacs-directory "places") backup-directory-alist `(("." \, (concat user-emacs-directory "backups"))))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; better-defaults-autoloads.el ends here
