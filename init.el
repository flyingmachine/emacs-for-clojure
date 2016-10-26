(defmacro comment (&rest body)
  "Ignores body, yields nil."
  nil)

(defmacro time (expr)
  "Evaluates expr and prints the time it took. Returns the value of expr."
  `(let ((start (current-time))
         (return ,expr))
     (print (format "Elapsed %f secs."
                    (float-time
                     (time-subtract (current-time) start))))
     return))

(defconst loading-start-time
  (current-time) "The start time at loading init.el")

(defun compile-and-load-elisp-files (files subdir)
  "Compile and load the elisp files under the subdir."
  (let* ((d (concat "~/.emacs.d/" subdir))
         (v (concat d emacs-version "/")))
    (when (not (file-exists-p v)) (make-directory v t))
    (dolist (f files)
      (let* ((from (concat d f))
             (c (replace-regexp-in-string "\.el$" "\.elc" f))
             (to (concat v c)))
        (when (or (not (file-exists-p to))
                  (file-newer-than-file-p from to))
          (byte-compile-file from)
          (rename-file (concat d c) to t))
        (load to)))))

(defmacro package-supported-p (&rest body)
  "Run body code if the Emacs supports package."
  `(when (>= emacs-major-version 24) ,@body))

(defmacro platform-supported-p (os &rest body)
  "Run body code if the Emacs on specified OS platform."
  `(when (eq system-type ,os) ,@body))

(defmacro version-supported-p (c v &rest body)
  "Run body code if the Emacs on specified version."
  `(when (,c ,v (string-to-number emacs-version)) ,@body))

(defmacro bin-exists-p (b)
  "Returns true if b exists in env."
  `(if (eq system-type 'windows-nt)
       (zerop (shell-command (concat "where " ,b " >nul 2>&1")))
     (zerop (shell-command (concat "hash " ,b " &>/dev/null")))))

(defmacro safe-call (fn &rest args)
  "Call fn with args when fn is bound"
  `(when (fboundp (quote ,fn)) (apply (quote ,fn) (quote ,args))))

(defmacro safe-do (fn &rest body)
  "Do body when fn is bound"
  `(when (fboundp (quote ,fn)) ,@body))

(defmacro safe-set! (x val)
  "Set x when x is bound"
  `(when (boundp (quote ,x)) (setq ,x ,val)))

(defmacro safe-do! (x &rest body)
  "Do body when x is bound"
  `(when (boundp (quote ,x)) ,@body))

(message "PATH=%s" (getenv "PATH"))


;; First to load UI part
(compile-and-load-elisp-files '("ui.el") "config/")


;; Start loading ...
(package-supported-p
 ;; define package repositories
 (setq package-archives 
       '(("gnu" . "http://elpa.gnu.org/packages/")
         ("melpa-stable" . "http://stable.melpa.org/packages/")))
   
 (require 'package)
 (package-initialize)

 ;; install exec-path-from-shell for PATH loading
 (when (not (package-installed-p 'exec-path-from-shell))
   (package-refresh-contents)
   (package-install 'exec-path-from-shell))
 (compile-and-load-elisp-files '("setup-shell.el") "config/")
 (message "PATH=%s" (getenv "PATH"))

  ;; packages based on existings
 (defconst has-docker (bin-exists-p "docker"))
 (defconst has-erlang (bin-exists-p "erl"))
 (defconst has-latex (bin-exists-p "latex"))
 (defconst has-java (bin-exists-p "java"))
 (defconst has-racket (bin-exists-p "racket"))

 ;; guarantee all packages are installed on start
 (defconst installed-packages
   (let* ((basic '(aggressive-indent
                   bing-dict
                   ido-ubiquitous
                   markdown-mode
                   paredit
                   rainbow-delimiters
                   smex
                   tagedit))
          (docker '(dockerfile-mode))
          (erlang '(erlang lfe-mode))
          (latex '(auctex))
          (java '(cider clojure-mode clojure-mode-extra-font-locking))
          (racket '(geiser)))
     (append basic
             (version-supported-p <= 24.4 (when has-docker docker))
             (version-supported-p <= 24.4 '(magit))
             (version-supported-p <= 23.2 (when has-racket racket))
             (when has-erlang erlang)
             (when has-latex latex)
             (when has-java java)
	     )))

 (let ((not-installed-packages
        (delete t (mapcar #'(lambda (p) (if (package-installed-p p) t p))
                          installed-packages))))
   (when not-installed-packages
     (package-refresh-contents)
     (message "#Installing the missing %d packages: %s"
              (length not-installed-packages)
              not-installed-packages)
     (mapcar #'(lambda (i) (package-install i))
             not-installed-packages)))

  (compile-and-load-elisp-files
   ;; compile and load basic elisp files
   (let* ((basic '("misc.el"
                   "navigation.el"
                   "setup-python.el"))
          (clojure (when has-java '("setup-clojure.el")))
          (lfe (when has-erlang '("setup-lfe.el"))))
     (append basic clojure lfe)) "config/"))
  
 ;; ^ end of support-package-p


(compile-and-load-elisp-files
 ;; compile and load non-package-required elisp files
 '("editing.el"
   "lisp-editing.el"
   "setup-debugger.el") "config/")

(compile-and-load-elisp-files
 ;; compile and load private non-package-required elisp files
 '("financial.el"
   "utils.el") "private/n/")

;; After loaded ...
(let ((elapsed
       (float-time
        (time-subtract (current-time) loading-start-time))))
  (message "#Loading init.el ... done (%.3fs)" elapsed))

(put 'upcase-region 'disabled nil)

