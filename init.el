(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;;add dependecies here, download them if none-exists
(defvar my-packages '(better-defaults
                      projectile
                      clojure-mode
                      cider
                      ac-nrepl
                      projectile))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(load "~/.emacs.d/user.el")
