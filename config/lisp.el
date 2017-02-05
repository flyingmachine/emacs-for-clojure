;;;;
;; Lisp
;;;;


;; basic lisp mode 
(dolist (hook '(emacs-lisp-mode-hook
                ielm-mode-hook
                scheme-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook))
  (add-hook hook
            (lambda ()
              (enable-paredit-mode)
              (aggressive-indent-mode)
              (rainbow-delimiters-mode))))


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


