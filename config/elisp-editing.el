;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode
  "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(defmacro enable-eldoc-mode ()
  "After Emacs 24.4 `turn-on-eldoc-mode is obsoleted, use `eldoc-mode indeed.
  `eldoc-mode shows documentation in the minibuffer when writing code.
  http://www.emacswiki.org/emacs/ElDoc"
  `(if (and (>= emacs-major-version 24)
           (>= emacs-minor-version 4))
      'eldoc-mode
    'turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (paredit-mode)
                                  (funcall (enable-eldoc-mode))
                                  (cond ((string= "*scratch*" (buffer-name))
                                         (local-set-key (kbd "RET")
                                                        (lambda () (interactive)
                                                          (eval-print-last-sexp)
                                                          (newline)))
                                         (local-set-key (kbd "TAB")
                                                        #'complete-symbol)))))

(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)

(add-hook 'ielm-mode-hook (lambda ()
                            (paredit-mode)
                            (funcall (enable-eldoc-mode))))

(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

