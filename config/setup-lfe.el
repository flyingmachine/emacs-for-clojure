;;;;
;; LFE (Lisp Flavoured Erlang)
;;;;

;; Enable paredit for LFE
(add-hook 'lfe-mode-hook #'enable-paredit-mode)

;; For .lfe files
(add-to-list 'auto-mode-alist '("\\.lfe\\'" . lfe-mode))

