(defmacro comment (&rest body)
  "Ignores body, yields nil"
  nil)

(defmacro time (expr)
  "Evaluates expr and prints the time it took. Returns the value of expr"
  `(let ((start (current-time))
         (return ,expr))
     (print (format "Elapsed %f secs."
                    (float-time (time-subtract (current-time)
                                               start))))
     return))

(defconst loading-start-time
  (current-time) "The start time at loading init.el")

(defun compile-and-load-elisp-files (files subdir)
  "Compile and load the elisp files under the subdir"
  (let ((d (concat "~/.emacs.d/" subdir)))
    (dolist (f files)
      (let* ((from (concat d f))
             (to (replace-regexp-in-string
                  "\.el$" "\.elc" from)))
        (when (or (not (file-exists-p to))
                  (file-newer-than-file-p from to))
          (setq compiled (byte-compile-file from)))
        (load to)))))

(compile-and-load-elisp-files '("ui.el") "config/")

(defmacro package-supported-p (&rest body)
  `(when (>= emacs-major-version 24)
     (progn ,@body)))

(defmacro plateform-supported-p (os &rest body)
  `(when (eq system-type ,os)
     (progn ,@body)))

(package-supported-p
 ;; Define package repositories
 (setq package-archives 
       '(("gnu" . "http://elpa.gnu.org/packages/")
         ("melpa-stable" . "http://stable.melpa.org/packages/")))
 ;; Guarantee all packages are installed on start
 (defconst installed-packages
   (delete nil
           (list
            'aggressive-indent
            'bing-dict
            'cider
            'clojure-mode
            'clojure-mode-extra-font-locking
            'exec-path-from-shell
            'ido-ubiquitous
            'lfe-mode
            'markdown-mode
            'paredit
            'rainbow-delimiters
            'smex
            'tagedit
            (when (<= 24.4
                      (+ emacs-major-version (* 0.1 emacs-minor-version)))
              ;; magit requires the emacs-24.4 package
              'magit))))
   
 (require 'package)
 (package-initialize)

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
   '("editing.el"
     "elisp-editing.el"
     "misc.el"
     "navigation.el"
     "setup-clojure.el"
     "setup-lfe.el"
     "setup-python.el"
     "setup-shell.el") "config/"))
 ;; ^ end of support-package-p

(compile-and-load-elisp-files
 ;; compile and load non-package-required elisp files
 '("financial.el"
   "utils.el") "private/n/")

;; After loaded ...
(let ((elapsed (float-time
                (time-subtract (current-time)
                               loading-start-time))))
  (message "#Loading init.el ... done (%.3fs)" elapsed))

(put 'upcase-region 'disabled nil)
