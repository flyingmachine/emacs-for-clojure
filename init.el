;; PACKAGES


;; Adding some repositories to package-archives list.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; this would set the value of package-archives to the following items
;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")
;;                          ("melpa" . "http://melpa-stable.milkbox.net/packages/")))


;; laod and active emacs packages. Need to do this first off so that
;; the packages are loaded before they are modified. This also sets
;; the laod path for customisations.
(package-initialize)



;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))



;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;


;; doesn't allow the system to switch windows horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; sets up windmove , which allows us to move windows around the screen
;; with the shift keys , so S-<right-arrow> to move the window to the right.
;; no need to install , just active the default bindings
(windmove-default-keybindings)

;; maximise screen on load
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (which-key ido-completing-read+ cider ido-ubiquitous magit paredit pkg-info-dummy-package
     tagedit clojurescript-mode projectile git-auto-commit-mode epl dash cl-lib-highlight 
     clojure-mode-extra-font-locking clojure-mode rainbow-delimiters smex))))


;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")


;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")


;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")


;; These customizations make editing a bit nicer.
(load "editing.el")


;; Hard-to-categorize customizations
(load "misc.el")


;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
