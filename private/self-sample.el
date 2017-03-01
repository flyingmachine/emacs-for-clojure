;;;;
;; self-sample.el: specified yourself private configuration elisp file
;;                 and named it with self.el
;;;;



(comment
 (platform-supported-p
     gnu/linux
   (defvar self-gnu/linux-font "White Rabbit-12"
     "default font-size for gnu/linux")
   (defvar self-gnu/linux-cjk-font (cons "Microsoft Yahei" 12)
     "default cjk font for gnu/linux")
   (defvar self-gnu/linux-theme 'tomorrow-night-eighties
     "default theme for linux")
   (defvar self-gnu/linux-post-do
     (lambda () (message "#self post-do ...")))))

(comment
 (platform-supported-p
     darwin
   (defvar self-darwin-font "Monaco-13"
     "default font-size for darwin")
   (defvar self-darwin-theme 'tomorrow-night-eighties
     "default theme for darwin")
   (defvar self-darwin-packages '(geiser clojure lfe)
     "default packages for darwin")
   (defvar self-darwin-post-do 
     (lambda () (message "#self post-do ...")))))

(comment
 (platform-supported-p
     windows-nt
   (defvar self-windows-nt-font "Consolas-13"
     "default font-size for windows nt")
   (defvar self-windows-nt-cjk-font (cons "Microsoft Yahei" 12))
   (defvar self-windows-nt-post-do
     (lambda () (message "#self post-do ...")))))
