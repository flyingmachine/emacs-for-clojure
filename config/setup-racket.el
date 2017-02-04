;;;;
;; Scheme
;;;;


;; Enable lisp mode for scheme
(add-hook 'scheme-mode-hook
	  (lambda ()
	    (enable-eldoc-mode)
	    (enable-paredit-mode)
	    (aggressive-indent-mode)
	    (rainbow-delimiters-mode)))
