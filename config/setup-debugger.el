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
   (when (zerop (shell-command "type -p lldb"))
     (compile-and-load-elisp-files
      '("gud-lldb-patch.el") "config/")))