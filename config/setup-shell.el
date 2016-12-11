;;;;
;; Shell
;;;;


;; Setup shell environment base on OS


(defmacro set-path-env ()
  "Set PATH and exec-path in Emacs.
  `TODO': other shell, duplicated path"
  `(let* ((p (shell-command-to-string ". ~/.bashrc; echo -n $PATH"))
          (x (split-string p ":")))
     (setenv "PATH" p)
     (setq exec-path (append exec-path x))))

;; set PATH on darwin
(platform-supported-p darwin (set-path-env))

;; set PATH on Linux
(platform-supported-p
 gnu/linux
 (unless (getenv "SHELL")
   (setenv "SHELL" "/bin/bash")
   (set-path-env)))

;; set PATH on Windows
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

