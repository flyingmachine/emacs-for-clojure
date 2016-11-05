;;;;
;; self-sample.el: specified yourself private configuration elisp file
;;                 and named it with self.el
;;;;



(comment
 (platform-supported-p
  gnu/linux
  (defconst self-gnu/linux-font "White Rabbit-12"
    "default font-size for gnu/linux")
  (defconst self-gnu/linux-cjk-font (cons "Microsoft Yahei" 12)
    "default cjk font for gnu/linux")))

(comment
 (platform-supported-p
  darwin
  (defconst self-darwin-font "Monaco-13"
    "default font-size for darwin")
  (defconst self-darwin-theme 'tomorrow-night-eighties
    "default theme for darwin")))

(comment
 (platform-supported-p
  windows-nt
  (defconst self-windows-nt-font "Consolas-13"
    "default font-size for windows nt")
  (defconst self-windows-nt-cjk-font (cons "Microsoft Yahei" 12))))
