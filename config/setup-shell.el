;;;;
;; Shell
;;;;

;; Enable paredit for Shell script
(add-hook 'shell-mode-hook 'enable-paredit-mode)

;; OS special settings
(cond ((eq system-type 'darwin)
       (progn
         (init-exec-path-from-shell)
         (compile-and-load-elisp-files
          '("gud-lldb-patch.el") "config/")))
      ((eq system-type 'gnu/linux)
       (progn
         (when (not (getenv "SHELL"))
           (setenv "SHELL" "/bin/bash"))
         (init-exec-path-from-shell)
         (when (zerop (shell-command "type -p lldb"))
           (compile-and-load-elisp-files
            '("gud-lldb-patch.el") "config/"))))
      ((eq system-type 'windows-nt)
       (progn
         (let ((git-bash "c:/program files/git/git-bash.exe"))
           (when (file-exists-p git-bash)
             (setenv "SHELL" git-bash))))))
