;; See:  https://clojure-lsp.io/
;; also: https://emacs-lsp.github.io/lsp-mode/
(setup (:package lsp-mode lsp-ui lsp-ivy lsp-treemacs)
  (:hook lsp-enable-which-key-integration)
  (:bind "M-<f7>" lsp-find-references))

;; clojure-mode is (naturally) the major mode for editing
;; Clojure and ClojureScript. subword-mode allows words
;; in camel case to be treated as separate words for
;; movement and editing commands.
;; https://github.com/clojure-emacs/clojure-mode
;; subword-mode is useful for working with camel-case tokens,
;; like names of Java classes (e.g. JavaClassName)
(setup (:package clojure-mode)
  (:hook subword-mode
         paredit-mode
         lsp))

;; CIDER is a whole interactive development environment for
;; Clojure. There is a ton of functionality here, so be sure
;; to check out the excellent documentation at
;; https://docs.cider.mx/cider/index.html
(setup (:package cider)
  (:bind "C-c u" cider-user-ns
         "C-M-r" cider-refresh)
  (:option cider-show-error-buffer t
           cider-auto-select-error-buffer t
           cider-repl-history-file "~/.emacs.d/cider-history"
           cider-repl-pop-to-buffer-on-connect t
           cider-repl-wrap-history t))

;; company provides auto-completion for CIDER
;; see https://docs.cider.mx/cider/usage/code_completion.html
(setup (:package company)
  (:hook-into cider-mode
	      cider-repl-mode))

;; hydra provides a nice looking menu for commands
;; to see what's available, use M-x and the prefix cider-hydra
;; https://github.com/clojure-emacs/cider-hydra
(setup (:package cider-hydra)
  (:hook-into clojure-mode))

;; additional refactorings for CIDER
;; e.g. add missing libspec, extract function, destructure keys
;; https://github.com/clojure-emacs/clj-refactor.el
(setup (:package clj-refactor)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (:hook-into clojure-mode))

;; enable paredit in your REPL
(setup cider-repl-mode
  (:hook paredit-mode))

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (cider-load-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

