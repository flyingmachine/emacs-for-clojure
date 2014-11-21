;;; ido-ubiquitous-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ido-ubiquitous-mode) "ido-ubiquitous" "ido-ubiquitous.el"
;;;;;;  (21450 38086))
;;; Generated autoloads from ido-ubiquitous.el

(define-obsolete-variable-alias 'ido-ubiquitous 'ido-ubiquitous-mode "0.8")

(define-obsolete-function-alias 'ido-ubiquitous 'ido-ubiquitous-mode "0.8")

(defvar ido-ubiquitous-mode nil "\
Non-nil if Ido-Ubiquitous mode is enabled.
See the command `ido-ubiquitous-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ido-ubiquitous-mode'.")

(custom-autoload 'ido-ubiquitous-mode "ido-ubiquitous" nil)

(autoload 'ido-ubiquitous-mode "ido-ubiquitous" "\
Use `ido-completing-read' instead of `completing-read' almost everywhere.

  This mode has no effect unles `ido-mode' is also enabled.

  If this mode causes problems for a function, you can customize
  when ido completion is or is not used by customizing
  `ido-ubiquitous-command-overrides' or
  `ido-ubiquitous-function-overrides'.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("ido-ubiquitous-pkg.el") (21450 38086
;;;;;;  769286))

;;;***

(provide 'ido-ubiquitous-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ido-ubiquitous-autoloads.el ends here
