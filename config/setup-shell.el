;;;;
;; Shell
;;;;


;; Setup shell environment base on OS


(defmacro set-default-shell (shell rshell)
  "Set default SHELL"
  `(progn
     (when (or (null (getenv "SHELL"))
               (not (string-match ,rshell (getenv "SHELL"))))
       (setenv "SHELL" ,shell))
     (when (or (null shell-file-name)
               (not (string-match ,rshell shell-file-name)))
       (setq shell-file-name ,shell))))


(defmacro set-path-env ()
  "Set PATH and exec-path in Emacs."
  `(let* ((p (shell-command-to-string
              "$SHELL -i -c 'echo -n $PATH' 2>/dev/null"))
          (x (split-string p ":")))
     (setenv "PATH" p)
     (while (car x)
       (when (not (member (car x) exec-path))
         (add-to-list 'exec-path (car x) t))
       (setq x (cdr x)))))


;; set PATH on darwin
(platform-supported-p darwin (set-path-env))

;; set PATH on Linux
(platform-supported-p
    gnu/linux
  (graphic-supported-p
    (set-default-shell "/bin/bash" "\/bash$")
    (set-path-env)))


;; set shell on Windows
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
      (setenv "SHELL" (bin-path "bash")))))

