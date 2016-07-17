(defmacro enable-eldoc-mode ()
  "After Emacs 24.4 `turn-on-eldoc-mode is obsoleted, use `eldoc-mode indeed.
  `eldoc-mode shows documentation in the minibuffer when writing code.
  http://www.emacswiki.org/emacs/ElDoc"
  `(if (< 24.0 (string-to-number emacs-version))
       'eldoc-mode
     'turn-on-eldoc-mode))

;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (package-supported-p (safe-call enable-paredit-mode))
            (funcall (enable-eldoc-mode))
            (cond ((string= "*scratch*" (buffer-name))
                   (local-set-key (kbd "RET")
                                  (lambda () (interactive)
                                    (eval-print-last-sexp)
                                    (newline)))
                   (local-set-key (kbd "TAB")
                                  #'complete-symbol)))))

(add-hook 'ielm-mode-hook
          (lambda ()
            (package-supported-p (safe-call enable-paredit-mode))
            (funcall (enable-eldoc-mode))))

(package-supported-p
 ;; minibuffer
 (cond
  ((eq system-type 'gnu/linux)
   (add-hook 'minibuffer-setup-hook
             #'enable-paredit-mode t))
  (t (add-hook 'eval-expression-minibuffer-setup-hook
               #'enable-paredit-mode)))
 ;; lisp mode
 (add-hook 'lisp-mode-hook
           #'enable-paredit-mode)
 (add-hook 'lisp-interaction-mode-hook
           #'enable-paredit-mode))
