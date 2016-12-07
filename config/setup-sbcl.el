;;;;
;; sbcl
;;;;

;; sbcl path
(platform-supported-p
 windows-nt
 (let* ((home (getenv "SBCL_HOME"))
        (path (windows-nt-path
               (file-name-directory
                (if home home (bin-path "sbcl"))))))
   (add-to-list 'exec-path path)
   (setq inferior-lisp-program "sbcl")))

(platform-supported-unless
 windows-nt
 (setq inferior-lisp-program (bin-path "sbcl")))

;; disable linum-mode on sbcl debugger buffer
(add-hook 'sldb-hook (lambda () (linum-mode -1)))

;; disable linum-mode on slime repl
(add-hook 'slime-repl-mode-hook (lambda () (linum-mode -1)))

;; setup
(slime-setup '(slime-fancy slime-asdf))
