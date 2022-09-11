;; paredit enables structural editing of just about any lisp
;; https://www.emacswiki.org/emacs/ParEdit
(setup (:package paredit)
  (:hook-into emacs-lisp-mode
	      eval-expression-minibuffer-setup
	      ielm-mode
	      lisp-mode
	      lisp-interaction-mode
	      scheme-mode))

(setup turn-on-eldoc-mode
  (:hook-into emacs-lisp-mode
	 lisp-interaction-mode
	 iel-mode))
