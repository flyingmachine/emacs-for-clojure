;;;;
;; Clojure
;;;;

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (when (boundp 'inferior-lisp-program)
              (setq inferior-lisp-program "lein repl"))
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

;;;;
;; Cider
;;;;

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(when (boundp 'cider-repl-pop-to-buffer-on-connect)
  (setq cider-repl-pop-to-buffer-on-connect t))

;; When there's a cider error, show its buffer and switch to it
(when (boundp 'cider-show-error-buffer)
  (setq cider-show-error-buffer t))
(when (boundp 'cider-auto-select-error-buffer)
  (setq cider-auto-select-error-buffer t))

;; Where to store the cider history.
(when (boundp 'cider-repl-history-file)
  (setq cider-repl-history-file "~/.emacs.d/cider-history"))

;; Wrap when navigating history.
(when (boundp 'cider-repl-wrap-history)
  (setq cider-repl-wrap-history t))

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))


;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (when (fboundp 'cider-load-current-buffer)
    (cider-load-current-buffer))
  (when (fboundp 'cider-current-ns)
    (let ((ns (cider-current-ns)))
      (when (fboundp 'cider-repl-set-ns)
        (cider-repl-set-ns ns))
      (when (fboundp 'cider-interactive-eval)
        (cider-interactive-eval
         (format "(println '(def server (%s/start))) (println 'server)" ns))
        (cider-interactive-eval
         (format "(def server (%s/start)) (println server)" ns))))))


(defun cider-refresh ()
  (interactive)
  (when (fboundp 'cider-interactive-eval)
    (cider-interactive-eval (format "(user/reset)"))))

(defun cider-user-ns ()
  (interactive)
  (when (fboundp 'cider-repl-set-ns)
    (cider-repl-set-ns "user")))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (when (boundp 'cider-mode-map)
       (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns))))
