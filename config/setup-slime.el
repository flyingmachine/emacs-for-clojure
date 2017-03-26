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
    (safe-setq-inferior-lisp-program "sbcl" t)))



(defun common-lisp-implementations ()
  "Returns a list of common-lisp implementations."
  (let ((ecl (when (bin-exists-p "ecl") (bin-path "ecl")))
        (sbcl (when (bin-exists-p "sbcl") (bin-path "sbcl"))))
    (remove nil (list (when sbcl (list 'sbcl (list sbcl)))
                      (when ecl (list 'ecl (list ecl)))))))



;; setup sbcl, it's slow process so be adviced
(defadvice slime (before slime-before compile)
  (set-default 'slime-lisp-implementations (common-lisp-implementations))
  (add-hook
   'slime-repl-mode-hook
   (lambda ()
     (safe-do-when slime-close-all-parens-in-sexp
       (local-set-key (kbd "C-c C-]")
                      'slime-close-all-parens-in-sexp))
     (safe-do-when slime-selector 
       (global-set-key (kbd "C-c s")
                       'slime-selector))))
  (slime-setup '(slime-fancy slime-asdf)))









