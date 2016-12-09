;;;;
;; Shell
;;;;


;; Disable paredit for shell script: some strange issues when using ()
;; (add-hook 'shell-mode-hook 'enable-paredit-mode)

;; Setup shell environment base on OS

(platform-supported-p
 darwin
 (exec-path-from-shell-initialize))


(platform-supported-p
 gnu/linux
 (unless (getenv "SHELL")
   (setenv "SHELL" "/bin/bash")
   (exec-path-from-shell-initialize)))

(platform-supported-p
 windows-nt
 (defadvice shell (before shell-before compile)
   (when (bin-exists-p "bash")
     (unless (file-exists-p "~/.emacs_bash")
       (copy-file "~/.emacs.d/config/.emacs_bash"
                  "~/.emacs_bash"))
     (add-to-list 'exec-path
                  (file-name-directory
                   (windows-nt-path (bin-path "bash"))))
     (setq shell-file-name "bash")
     (setenv "SHELL" (shell-command-to-string "type -P bash")))))


;; disable linum on term/shell/eshell
(add-hook 'term-mode-hook (lambda () (linum-mode -1)))
(add-hook 'shell-mode-hook (lambda () (linum-mode -1)))
(add-hook 'eshell-mode-hook (lambda () (linum-mode -1)))

