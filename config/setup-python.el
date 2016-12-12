;;;;
;; Python
;;;;

(add-hook 'python-mode-hook
          (lambda ()
            ;; Enable paredit for Python
            (enable-paredit-mode)
            ;; Working with camel-case tokens
            (subword-mode)))

(add-hook 'inferior-python-mode-hook
          (lambda () (toggle-linum-mode -1)))

