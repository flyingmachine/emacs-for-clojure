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
  (let ((ecl (when has-ecl (bin-path "ecl")))
        (sbcl (when has-sbcl (bin-path "sbcl"))))
    (remove nil (list (when ecl (list 'ecl (list ecl)))
                      (when sbcl (list 'sbcl (list sbcl)))))))



;; setup sbcl, it's slow process so be adviced
(defadvice slime (before slime-before compile)
  (set-default 'slime-lisp-implementations (common-lisp-implementations))
  (add-hook
   'slime-repl-mode-hook
   (lambda ()
     (safe-do slime-close-all-parens-in-sexp
       (local-set-key (kbd "C-c C-]")
                      'slime-close-all-parens-in-sexp))
     (safe-do slime-selector 
       (global-set-key (kbd "C-c s")
                       'slime-selector))))
  (slime-setup '(slime-fancy slime-asdf)))









