;;;;
;; Clojure
;;;;


;; clojure mode hooks
(add-hook 'clojure-mode-hook
          (lambda ()
            ;; (safe-setq inferior-lisp-program "boot repl")
            (enable-eldoc-mode)
            (enable-paredit-mode)
            (subword-mode)
            (rainbow-delimiters-mode)
            (aggressive-indent-mode)
            (inf-clojure-minor-mode)))


;; use clojure mode for other extensions
;; (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))


;;;;
;; inferior
;;;;

;; minor modes for inf-clojure
(add-hook 'inf-clojure-mode-hook
          (lambda ()
            (enable-eldoc-mode)
            (enable-paredit-mode)
            (subword-mode)
            (rainbow-delimiters-mode)
            (aggressive-indent-mode)))


(defadvice inf-clojure (before inf-clojure-before compile)
  (platform-supported-p
      windows-nt
    ;; Fix returning nothing in Windows
    (let ((jlinerc "~/.jline.rc"))
      (when (not (file-exists-p jlinerc))
        (write-region "jline.terminal=unsupported" "" jlinerc)))))


;;;;
;; Cider
;;;;


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

;; minor modes for cider
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (enable-eldoc-mode)
            (enable-paredit-mode)
            (rainbow-delimiters-mode)
            (aggressive-indent-mode)))


;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (safe-call cider-load-current-buffer)
  (safe-do-when cider-current-ns
    (let ((ns (cider-current-ns)))
      (safe-call cider-repl-set-ns ns)
      (safe-do-when
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


;;;;
;; Figwheel `https://github.com/bhauman/lein-figwheel'
;;;;


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
