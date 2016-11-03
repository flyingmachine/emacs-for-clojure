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
 (when (bin-exists-p "bash")
   (unless (file-exists-p "~/.emacs_bash")
     (copy-file "~/.emacs.d/config/.emacs_bash"
                "~/.emacs_bash"))
   (setq shell-file-name "bash")
   (setenv "SHELL" shell-file-name)
   (safe-setq explicit-shell-file-name shell-file-name)))

