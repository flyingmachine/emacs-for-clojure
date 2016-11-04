;;;;
;; self-sample.el: specified yourself private configuration elisp file
;;                 and named it with self.el
;;;;



(comment
 (platform-supported-p
  gnu/linux
  (defconst private-gnu/linux-font "White Rabbit-12"
    "default font-size for gnu/linux")
  (defconst private-gnu/linux-cjk-font (cons "Microsoft Yahei" 12)
    "default cjk font for gnu/linux")))

(comment
 (platform-supported-p
  darwin
  (defconst private-darwin-font "Monaco-13"
    "default font-size for darwin")))

(comment
 (platform-supported-p
  windows-nt
  (defconst private-windows-nt-font "Consolas-13"
    "default font-size for windows nt")
  (defconst private-windows-nt-cjk-font (cons "Microsoft Yahei" 12))))
