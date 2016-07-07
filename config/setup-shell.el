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
       (let* ((win-bash
               "C:/Program Files/Git/usr/bin/bash.exe")
              (win-bash-path (file-name-directory win-bash)))
         ;; no better solution for bash on Windows
         (when (file-exists-p win-bash)
           (unless (file-exists-p "~/.emacs_bash")
             (copy-file "~/.emacs.d/config/.emacs_bash"
                        "~/.emacs_bash"))
           (setenv "PATH" (concat win-bash-path (getenv "PATH")))
           (setq shell-file-name "bash")
           (setenv "SHELL" shell-file-name)
           (when (boundp 'explicit-shell-file-name)
             (setq explicit-shell-file-name shell-file-name))
           (add-hook
            'comint-output-filter-function 'comint-strip-ctrl-m)))))
