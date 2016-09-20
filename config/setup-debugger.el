;;
;; Debugger setup script
;;

;; lldb on darwin
(plateform-supported-p
   'darwin
   (compile-and-load-elisp-files
    '("gud-lldb-patch.el") "config/"))

;; lldb on linux
(plateform-supported-p
   'gnu/linux
   (when (bin-exists-p "lldb")
     (compile-and-load-elisp-files
      '("gud-lldb-patch.el") "config/")))
