;;; rainbow-delimiters-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-rainbow-delimiters-mode rainbow-delimiters-mode-enable
;;;;;;  rainbow-delimiters-mode) "rainbow-delimiters" "rainbow-delimiters.el"
;;;;;;  (21697 51856 0 0))
;;; Generated autoloads from rainbow-delimiters.el

(autoload 'rainbow-delimiters-mode "rainbow-delimiters" "\
Highlight nested parentheses, brackets, and braces according to their depth.

\(fn &optional ARG)" t nil)

(autoload 'rainbow-delimiters-mode-enable "rainbow-delimiters" "\


\(fn)" nil nil)

(defvar global-rainbow-delimiters-mode nil "\
Non-nil if Global-Rainbow-Delimiters mode is enabled.
See the command `global-rainbow-delimiters-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-rainbow-delimiters-mode'.")

(custom-autoload 'global-rainbow-delimiters-mode "rainbow-delimiters" nil)

(autoload 'global-rainbow-delimiters-mode "rainbow-delimiters" "\
Toggle Rainbow-Delimiters mode in all buffers.
With prefix ARG, enable Global-Rainbow-Delimiters mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Rainbow-Delimiters mode is enabled in all buffers where
`rainbow-delimiters-mode-enable' would do it.
See `rainbow-delimiters-mode' for more information on Rainbow-Delimiters mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("rainbow-delimiters-pkg.el") (21697 51857
;;;;;;  19874 0))

;;;***

(provide 'rainbow-delimiters-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rainbow-delimiters-autoloads.el ends here
