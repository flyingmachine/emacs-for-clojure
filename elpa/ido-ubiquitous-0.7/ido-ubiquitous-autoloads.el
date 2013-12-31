;;; ido-ubiquitous-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ido-ubiquitous-function-exceptions ido-ubiquitous-command-exceptions
;;;;;;  ido-ubiquitous ido-ubiquitous) "ido-ubiquitous" "ido-ubiquitous.el"
;;;;;;  (20166 22146))
;;; Generated autoloads from ido-ubiquitous.el

(let ((loads (get 'ido-ubiquitous 'custom-loads))) (if (member '"ido-ubiquitous" loads) nil (put 'ido-ubiquitous 'custom-loads (cons '"ido-ubiquitous" loads))))

(defvar ido-ubiquitous nil "\
Non-nil if Ido-Ubiquitous mode is enabled.
See the command `ido-ubiquitous' for a description of this minor mode.")

(custom-autoload 'ido-ubiquitous "ido-ubiquitous" nil)

(autoload 'ido-ubiquitous "ido-ubiquitous" "\
Use `ido-completing-read' instead of `completing-read' almost everywhere.

  This mode has no effect unles `ido-mode' is also enabled.

  If this mode causes problems for a function, you can force the
  function to use the original completing read by using the macro
  `ido-ubiquitous-disable-in'. For example, if a
  function `foo' cannot work with ido-style completion, evaluate
  the following (for example by putting it in your .emacs file):

    (ido-ubiquitous-disable-in foo)

\(fn &optional ARG)" t nil)

(defvar ido-ubiquitous-command-exceptions 'nil "\
List of commands that should not be affected by `ido-ubiquitous'.

Even when `ido-ubiquitous' mode is enabled, these commands will
continue to use `completing-read' instead of
`ido-completing-read'.

Only *interactive* commands should go here. To disable
ido-ubiquitous in non-interactive functions, customize
`ido-ubiquitous-function-exceptions'.")

(custom-autoload 'ido-ubiquitous-command-exceptions "ido-ubiquitous" t)

(defvar ido-ubiquitous-function-exceptions '(grep-read-files) "\
List of functions in which to disable ido-ubiquitous.

Certain functions, such as `read-file-name', always have
ido-ubiquitous disabled, and cannot be added here. (They are
effectively permanently part of this list already.)")

(custom-autoload 'ido-ubiquitous-function-exceptions "ido-ubiquitous" nil)

;;;***

;;;### (autoloads nil nil ("ido-ubiquitous-pkg.el") (20166 22146
;;;;;;  727855))

;;;***

(provide 'ido-ubiquitous-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ido-ubiquitous-autoloads.el ends here
