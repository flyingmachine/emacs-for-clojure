;; javascript / html
(setup (:package tagedit)
  (:hook-into html-mode))

(setup subword-mode
  (:hook-into js-mode
	      html-mode
	      coffee-mode))

(setq js-indent-level 2)

;; coffeescript
(setup coffee-mode
  (:hook highlight-indentation-current-column-mode
	 (defun coffee-mode-newline-and-indent ()
	   (define-key coffee-mode-map "\C-j" 'coffee-newline-and-indent)
	   (setq coffee-cleanup-whitespace nil))))

(custom-set-variables
 '(coffee-tab-width 2))
