;;; ido-ubiquitous-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ido-ubiquitous" "ido-ubiquitous.el" (22235
;;;;;;  10838 393594 319000))
;;; Generated autoloads from ido-ubiquitous.el

(defvar ido-ubiquitous-debug-mode nil "\
Non-nil if Ido-Ubiquitous-Debug mode is enabled.
See the command `ido-ubiquitous-debug-mode' for a description of this minor mode.")

(custom-autoload 'ido-ubiquitous-debug-mode "ido-ubiquitous" nil)

(autoload 'ido-ubiquitous-debug-mode "ido-ubiquitous" "\
If non-nil, ido-ubiquitous will print debug info.

Debug info is printed to the *Messages* buffer.

\(fn &optional ARG)" t nil)

(define-obsolete-variable-alias 'ido-ubiquitous 'ido-ubiquitous-mode "ido-ubiquitous 0.8")

(define-obsolete-function-alias 'ido-ubiquitous 'ido-ubiquitous-mode "ido-ubiquitous 0.8")

(defvar ido-ubiquitous-mode nil "\
Non-nil if Ido-Ubiquitous mode is enabled.
See the command `ido-ubiquitous-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ido-ubiquitous-mode'.")

(custom-autoload 'ido-ubiquitous-mode "ido-ubiquitous" nil)

(autoload 'ido-ubiquitous-mode "ido-ubiquitous" "\
Use `ido-completing-read' instead of `completing-read' almost everywhere.

If this mode causes problems for a function, you can customize
when ido completion is or is not used by customizing
`ido-ubiquitous-command-overrides' or
`ido-ubiquitous-function-overrides'.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ido-ubiquitous-autoloads.el ends here
