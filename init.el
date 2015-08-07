(defmacro comment (&rest body) nil)
(defun compile-and-load-elc (files subdir)
  (let ((d (concat "~/.emacs.d/" subdir)))
    (dolist (f files)
      (let* ((from (concat d f))
             (to (replace-regexp-in-string "\.el$" "\.elc" from)))
        (when (or (not (file-exists-p to))
                  (file-newer-than-file-p from to)
                  (equal (nth 4 (file-attributes from)) '(list 0 0)))
          (setq compiled (byte-compile-file from)))
        (load to)))))

;; (add-to-list 'load-path "~/.emacs.d/config")
(compile-and-load-elc '("ui.el") "config/")

;; Define package repositories
(when (>= emacs-major-version 24)
  (setq package-archives 
	'(("gnu" . "http://elpa.gnu.org/packages/")
	  ("melpa-stable" . "http://stable.melpa.org/packages/")))

  ;; Guarantee all packages are installed on start
  (defconst installed-packages
    (delete nil
            (list
             'exec-path-from-shell
             'paredit
             'clojure-mode
             'clojure-mode-extra-font-locking
             'cider
             'ido-ubiquitous
             'smex
             'projectile
             'rainbow-delimiters
             'tagedit
             (if (>= emacs-minor-version 4)
                 ;; magit requires the emacs-24.4 package
                 'magit nil)
             )))

  (require 'package)
  (package-initialize)

  (let ((not-installed-packages
	 (delete t (mapcar #'(lambda (p)
			      (if (package-installed-p p) t p))
			   installed-packages))))
    (when not-installed-packages
      (progn
        (package-refresh-contents)
        (message "#Installing the missing %d packages: %s"
                 (length not-installed-packages) not-installed-packages)
        (mapcar #'(lambda (i) (package-install i))
                not-installed-packages))))

  (when (eq system-type 'darwin)
    (exec-path-from-shell-initialize))
  
  (compile-and-load-elc '("navigation.el"
                          "editing.el"
                          "elisp-editing.el"
                          "setup-clojure.el"
                          "misc.el") "config/"))
