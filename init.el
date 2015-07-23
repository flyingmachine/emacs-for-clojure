;; Define comment macro
(defmacro comment (&rest body) nil)

(add-to-list 'load-path "~/.emacs.d/config")

(load "ui.el")

;; Define package repositories
(when (and (>= emacs-major-version 24) (>= emacs-minor-version 4))
  (setq package-archives 
	'(("gnu" . "http://elpa.gnu.org/packages/")
	  ("melpa-stable" . "http://stable.melpa.org/packages/")))
  ;; Guarantee all packages are installed on start
  (defconst packages-list
    '(paredit)
    "List of packages needs to be installed at launch")

  (require 'package)
  (package-initialize)

  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (message "Installing the missing %s package" p)
      (package-install p))))


