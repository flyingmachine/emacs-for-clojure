(defmacro enable-eldoc-mode ()
  "After Emacs 24.4 `turn-on-eldoc-mode is obsoleted, use `eldoc-mode indeed.
  `eldoc-mode shows documentation in the minibuffer when writing code.
  http://www.emacswiki.org/emacs/ElDoc"
  `(if (< 24.0 (string-to-number emacs-version))
       'eldoc-mode
     'turn-on-eldoc-mode))

;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit

;; *scratch* 
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (funcall (enable-eldoc-mode))
            (package-supported-p
              (safe-call enable-paredit-mode)
              (aggressive-indent-mode)
              (rainbow-delimiters-mode))
            (local-set-key (kbd "TAB") #'complete-symbol)
            (cond ((string= "*scratch*" (buffer-name))
                   (local-set-key (kbd "RET")
                                  (lambda () (interactive)
                                    (eval-print-last-sexp)
                                    (newline)))))))

;; Interactive Elisp mode
(add-hook 'ielm-mode-hook
          (lambda ()
            (package-supported-p (safe-call enable-paredit-mode))
            (funcall (enable-eldoc-mode))))


;; Enable paredit in minibuffer
(package-supported-p
 (cond
  ((eq system-type 'gnu/linux)
   (add-hook 'minibuffer-setup-hook
             #'enable-paredit-mode t))
  (t (add-hook 'eval-expression-minibuffer-setup-hook
               #'enable-paredit-mode))))
