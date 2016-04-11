;;;;
;; Python
;;;;

;; Enable paredit for Python
(add-hook 'python-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'python-mode-hook 'subword-mode)

;; Using IPython as the Python shell
(when (boundp 'python-shell-interpreter)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter "-i"))


