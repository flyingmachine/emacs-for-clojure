;;;;
;; LFE (Lisp Flavoured Erlang)
;;;;

;; Enable paredit for LFE
(add-hook 'lfe-mode-hook (lambda ()
                           (enable-paredit-mode)))

(add-to-list 'auto-mode-alist '("\\.lfe\\'" . lfe-mode))
