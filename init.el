(defmacro comment (&rest body) nil)
(defun compile-and-load-elc (files)
  (dolist (f files)
    (let* ((from (concat (getenv "HOME") "/.emacs.d/config/" f))
           (to (replace-regexp-in-string "\.el$" "\.elc" from)))
      (when (or (not (file-exists-p to))
                (file-newer-than-file-p from to)
                (equal (nth 4 (file-attributes from)) '(list 0 0)))
        (setq compiled (byte-compile-file from)))
      (load to))))

;; (add-to-list 'load-path "~/.emacs.d/config")
(compile-and-load-elc '("ui.el"))

;; Define package repositories
(when (and (>= emacs-major-version 24) (>= emacs-minor-version 4))
  (setq package-archives 
	'(("gnu" . "http://elpa.gnu.org/packages/")
	  ("melpa-stable" . "http://stable.melpa.org/packages/")))

  ;; Guarantee all packages are installed on start
  (defconst packages-list
    '(exec-path-from-shell
      paredit
      clojure-mode
      clojure-mode-extra-font-locking
      cider
      ido-ubiquitous
      smex
      projectile
      rainbow-delimiters
      tagedit
      magit)
    "List of packages needs to be installed at launch")

  (require 'package)
  (package-initialize)

  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (message "Installing the missing %s package" p)
      (package-install p)))

  (when (eq system-type 'darwin)
    (exec-path-from-shell-initialize))
  
  (compile-and-load-elc '("navigation.el"
                          "editing.el"
                          "elisp-editing.el"
                          "setup-clojure.el"
                          "misc.el")))
