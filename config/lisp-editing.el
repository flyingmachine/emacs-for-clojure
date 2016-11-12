(defmacro enable-eldoc-mode ()
  "After Emacs 24.4 `turn-on-eldoc-mode is obsoleted, use `eldoc-mode indeed.
  `eldoc-mode shows documentation in the minibuffer when writing code.
  http://www.emacswiki.org/emacs/ElDoc"
  `(version-supported-if < 24.0
                         (eldoc-mode)
                         (turn-on-eldoc-mode)))

;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit

(defmacro add-lisp-mode-hook (hook &rest body)
  (declare (indent 1) (debug t))
  `(add-hook ,hook
             (lambda ()
               (package-supported-p
                (safe-call enable-paredit-mode)
                (safe-call aggressive-indent-mode)
                (safe-call rainbow-delimiters-mode))
               ,@body)))



;; *scratch* 
(add-lisp-mode-hook 'emacs-lisp-mode-hook
  (progn
    (enable-eldoc-mode)
    (local-set-key (kbd "TAB") #'complete-symbol)
    (cond ((string= "*scratch*" (buffer-name))
           (local-set-key (kbd "RET")
                          (lambda () (interactive)
                            (eval-print-last-sexp)
                            (newline)))))))


;; Interactive Elisp mode
(add-lisp-mode-hook 'ielm-mode-hook (enable-eldoc-mode))


;; Enable paredit for scheme
(add-lisp-mode-hook 'scheme-mode-hook)


;; Enable paredit in minibuffer on gnu/linux platform
(platform-supported-p
 gnu/linux
 (add-hook 'minibuffer-setup-hook
           #'enable-paredit-mode t))

;; Enable paredit in minbuffer on windows/darwin platform
(platform-supported-unless
 gnu/linux
 (add-hook 'eval-expression-minibuffer-setup-hook
           #'enable-paredit-mode))
