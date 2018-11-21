;;; ido-completing-read+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ido-completing-read+" "ido-completing-read+.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ido-completing-read+.el

(defvar ido-cr+-minibuffer-depth -1 "\
Minibuffer depth of the most recent ido-cr+ activation.

If this equals the current minibuffer depth, then the minibuffer
is currently being used by ido-cr+, and ido-cr+ feature will be
active. Otherwise, something else is using the minibuffer and
ido-cr+ features will be deactivated to avoid interfering with
the other command.

This is set to -1 by default, since `(minibuffer-depth)' should
never return this value.")

(defvar ido-cr+-replace-completely nil "\
If non-nil, replace `ido-completeing-read' completely with ido-cr+.

Enabling this may interfere with or cause errors in other
packages that use `ido-completing-read'. If you discover any such
incompatibilities, please file a bug report at
https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues")

(custom-autoload 'ido-cr+-replace-completely "ido-completing-read+" t)

(defsubst ido-cr+-active nil "\
Returns non-nil if ido-cr+ is currently using the minibuffer." (>= ido-cr+-minibuffer-depth (minibuffer-depth)))

(autoload 'ido-completing-read+ "ido-completing-read+" "\
ido-based method for reading from the minibuffer with completion.

See `completing-read' for the meaning of the arguments.

This function is a wrapper for `ido-completing-read' designed to
be used as the value of `completing-read-function'. Importantly,
it detects edge cases that ido cannot handle and uses normal
completion for them.

\(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil)

(autoload 'ido-completing-read@ido-cr+-replace "ido-completing-read+" "\
This advice allows ido-cr+ to completely replace `ido-completing-read'.

See the varaible `ido-cr+-replace-completely' for more information.

\(fn ORIG-FUN &rest ARGS)" nil nil)

(advice-add 'ido-completing-read :around #'ido-completing-read@ido-cr+-replace)

(autoload 'call-interactively@ido-cr+-record-current-command "ido-completing-read+" "\
Let-bind the command being interactively called.

See `ido-cr+-current-command' for more information.

\(fn ORIG-FUN COMMAND &rest ARGS)" nil nil)

(advice-add 'call-interactively :around #'call-interactively@ido-cr+-record-current-command)

(defvar ido-ubiquitous-mode nil "\
Non-nil if Ido-Ubiquitous mode is enabled.
See the `ido-ubiquitous-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ido-ubiquitous-mode'.")

(custom-autoload 'ido-ubiquitous-mode "ido-completing-read+" nil)

(autoload 'ido-ubiquitous-mode "ido-completing-read+" "\
Use ido completion instead of standard completion almost everywhere.

If this mode causes problems for a function, you can customize
when ido completion is or is not used by customizing
`ido-cr+-function-blacklist'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ido-completing-read+" '("ido-" "minibuf-eldef-update-minibuffer@ido-cr+-compat" "define-ido-internal-var")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ido-completing-read+-autoloads.el ends here
