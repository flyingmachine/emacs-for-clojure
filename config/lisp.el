;;;;
;; Lisp
;;;;



;; Interactive Elisp mode
(add-lisp-mode-hook 'ielm-mode-hook
  (enable-eldoc-mode)
  (toggle-linum-mode -1))


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
