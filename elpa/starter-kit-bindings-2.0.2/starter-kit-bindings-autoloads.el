;;; starter-kit-bindings-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "starter-kit-bindings" "starter-kit-bindings.el"
;;;;;;  (20184 56041))
;;; Generated autoloads from starter-kit-bindings.el

(global-set-key (kbd "C-c f") 'find-file-in-project)

(global-set-key (kbd "C-M-h") 'backward-kill-word)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-c n") 'esk-cleanup-buffer)

(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

(define-key global-map (kbd "C-+") 'text-scale-increase)

(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)

(global-set-key (kbd "") 'isearch-backward-regexp)

(global-set-key (kbd "C-M-s") 'isearch-forward)

(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "C-x C-i") 'imenu)

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

(global-set-key (kbd "C-c y") 'bury-buffer)

(global-set-key (kbd "C-c r") 'revert-buffer)

(windmove-default-keybindings)

(global-set-key (kbd "C-x O") (lambda nil (interactive) (other-window -1)))

(global-set-key (kbd "C-x C-o") (lambda nil (interactive) (other-window 2)))

(global-set-key (kbd "C-x m") 'eshell)

(global-set-key (kbd "C-x M") (lambda nil (interactive) (eshell t)))

(global-set-key (kbd "C-x C-m") 'shell)

(global-set-key (kbd "C-c x") 'execute-extended-command)

(global-set-key (kbd "C-h a") 'apropos)

(global-set-key (kbd "C-c e") 'esk-eval-and-replace)

(global-set-key (kbd "C-c q") 'join-line)

(global-set-key (kbd "C-c g") 'magit-status)

(eval-after-load 'vc (define-key vc-prefix-map "i" '(lambda nil (interactive) (if (not (eq 'Git (vc-backend buffer-file-name))) (vc-register) (shell-command (format "git add %s" buffer-file-name)) (message "Staged changes.")))))

(define-key isearch-mode-map (kbd "C-o") (lambda nil (interactive) (let ((case-fold-search isearch-case-fold-search)) (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;;;***

;;;### (autoloads nil nil ("starter-kit-bindings-pkg.el") (20184
;;;;;;  56041 900829))

;;;***

(provide 'starter-kit-bindings-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; starter-kit-bindings-autoloads.el ends here
