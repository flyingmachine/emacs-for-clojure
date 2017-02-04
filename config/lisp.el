;;;;
;; Lisp
;;;;



;; *scratch*
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (enable-paredit-mode)
            (aggressive-indent-mode)
            (rainbow-delimiters-mode)))


;; Interactive Elisp mode
(add-hook 'ielm-mode-hook
          (lambda ()
            (enable-paredit-mode)
            (aggressive-indent-mode)
            (rainbow-delimiters-mode)))



;; Enable lisp mode for scheme
(add-hook 'scheme-mode-hook
          (lambda ()
            (enable-eldoc-mode)
            (enable-paredit-mode)
            (aggressive-indent-mode)
            (rainbow-delimiters-mode)))


;; Enable lisp mode for scheme
(add-hook 'lisp-mode-hook
          (lambda ()
            (enable-eldoc-mode)
            (enable-paredit-mode)
            (aggressive-indent-mode)
            (rainbow-delimiters-mode)))


;; Enable lisp mode for scheme
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (enable-eldoc-mode)
            (enable-paredit-mode)
            (aggressive-indent-mode)
            (rainbow-delimiters-mode)))


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


