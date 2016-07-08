;;;;
;; Shell
;;;;

;; Enable paredit for shell script
(add-hook 'shell-mode-hook 'enable-paredit-mode)

;; Setup shell environment
(support-package-p
 (message "enter setup-shell")
 (when (package-installed-p 'exec-path-from-shell)
   (support-plateform-p
    'darwin
    (exec-path-from-shell-initialize)
    (compile-and-load-elisp-files
     '("gud-lldb-patch.el") "config/"))
   
   (support-plageform-p
    'gnu/linux
    (when (not (getenv "SHELL"))
      (setenv "SHELL" "/bin/bash"))
    (exec-path-from-shell-initialize)
    (when (zerop (shell-command "type -p lldb"))
      (compile-and-load-elisp-files
       '("gud-lldb-patch.el") "config/"))))

 (support-plateform-p
  'windows-nt
  (let* ((win-bash "C:/Program Files/Git/usr/bin/bash.exe")
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
