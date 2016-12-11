;;;;
;; Clojure
;;;;


;; clojure mode hooks
(add-hook 'clojure-mode-hook
          (lambda ()
            ;; (safe-setq inferior-lisp-program "boot repl")
            ;; enable paredit 
            (enable-paredit-mode)
            ;; enable camel case support for editing commands
            (subword-mode)
            ;; hilighting parentheses,brackets,and braces in minor mode
            (rainbow-delimiters-mode)
            ;; enable automatically adjust the identation of code
            (aggressive-indent-mode)))


;; use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))


;;;;
;; Cider
;;;;


;; Provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (enable-eldoc-mode)
            (linum-mode -1)))

;; Go right to the REPL buffer when it's finished connecting
(safe-setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(safe-setq cider-show-error-buffer t)
(safe-setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(safe-setq cider-repl-history-file
 "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(safe-setq cider-repl-wrap-history t)

;; enable paredit for Cider
(add-hook 'cider-repl-mode-hook #'paredit-mode)


;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (safe-call cider-load-current-buffer)
  (safe-do cider-current-ns
           (let ((ns (cider-current-ns)))
             (safe-call cider-repl-set-ns ns)
             (safe-do
              cider-interactive-eval
              (cider-interactive-eval
               (format "(println '(def server (%s/start))) (println 'server)"
                       ns))
              (cider-interactive-eval
               (format "(def server (%s/start)) (println server)"
                       ns))))))


(defun cider-refresh ()
  (interactive)
  (safe-call cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (safe-call cider-repl-set-ns "user"))

(defmacro figwheel-after-load-cider ()
  "Enable Figwheel: cider-jack-in-clojurescript"
  `(safe-setq cider-cljs-lein-repl
              "(do (require 'figwheel-sidecar.repl-api)
                 (figwheel-sidecar.repl-api/start-figwheel!)
                 (figwheel-sidecar.repl-api/cljs-repl))"))

(eval-after-load 'cider
  '(progn
     (safe-setq* 'clojure-mode-map
                 (define-key clojure-mode-map
                   (kbd "C-c C-v") 'cider-start-http-server)
                 (define-key clojure-mode-map
                   (kbd "C-M-r") 'cider-refresh))
     (safe-setq* 'cider-mode-map
                 (define-key cider-mode-map
                   (kbd "C-c u") 'cider-user-ns))
     (figwheel-after-load-cider)))
