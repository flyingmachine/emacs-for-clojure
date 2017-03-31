;;
;; Debugger setup script
;;

;; lldb on darwin
(platform-supported-p
 darwin
 (compile-and-load-elisp-files
  '("gud-lldb-patch.el") "config/"))

;; lldb on linux
(platform-supported-p
 gnu/linux
 (when (bin-exists-p "lldb")
   (compile-and-load-elisp-files
    '("gud-lldb-patch.el") "config/")))
