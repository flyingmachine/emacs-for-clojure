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


(platform-supported-unless
 windows-nt
 (safe-setq-inferior-lisp-program (bin-path "sbcl") t))


(comment
 ;; Stop SLIME's REPL from grabbing DEL,
 ;; which is annoying when backspacing over a '('
 (defun override-slime-repl-bindings-with-paredit ()
   (enable-paredit-mode)
   (aggressive-indent-mode)
   (rainbow-delimiters-mode)
   (when (and (boundp 'slime-repl-mode-map)
              (boundp 'paredit-backward-delete-key))
     (define-key slime-repl-mode-map
       (read-kbd-macro paredit-backward-delete-key) nil))))


;; setup sbcl, it's slow process so be adviced
(defadvice slime (before slime-before compile)
  (add-hook
   'slime-repl-mode-hook
   (lambda ()
     (safe-do slime-close-all-parens-in-sexp
              (local-set-key (kbd "C-c C-]")
                             'slime-close-all-parens-in-sexp))
     (safe-do slime-selector 
              (global-set-key (kbd "C-c s")
                              'slime-selector))
     (comment
      (override-slime-repl-bindings-with-paredit))))
  (slime-setup '(slime-fancy slime-asdf)))









