;;; ido-completing-read+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ido-completing-read+" "ido-completing-read+.el"
;;;;;;  (22235 10835 872590 323000))
;;; Generated autoloads from ido-completing-read+.el

(defvar ido-cr+-enable-next-call nil "\
If non-nil, then the next call to `ido-completing-read' is by `ido-completing-read+'.")

(defvar ido-cr+-enable-this-call nil "\
If non-nil, then the current call to `ido-completing-read' is by `ido-completing-read+'")

(defvar ido-cr+-replace-completely nil "\
If non-nil, replace `ido-completeing-read' completely with ido-cr+.

Enabling this may interfere with or cause errors in other
packages that use `ido-completing-read'. If you discover any such
incompatibilities, please file a bug report at
https://github.com/DarwinAwardWinner/ido-ubiquitous/issues")

(custom-autoload 'ido-cr+-replace-completely "ido-completing-read+" t)

(autoload 'ido-completing-read+ "ido-completing-read+" "\
ido-based method for reading from the minibuffer with completion.

See `completing-read' for the meaning of the arguments.

This function is a wrapper for `ido-completing-read' designed to
be used as the value of `completing-read-function'. Importantly,
it detects edge cases that ido cannot handle and uses normal
completion for them.

\(fn PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)" nil nil)

(defadvice ido-completing-read (around ido-cr+ activate) "\
This advice handles application of ido-completing-read+ features.

First, it ensures that `ido-cr+-enable-this-call' is set
properly. This variable should be non-nil during execution of
`ido-completing-read' if it was called from
`ido-completing-read+'.

Second, if `ido-cr+-replace-completely' is non-nil, then this
advice completely replaces `ido-completing-read' with
`ido-completing-read+'." (when (not (featurep (quote ido-completing-read+))) (require (quote ido-completing-read+))) (let ((ido-cr+-enable-this-call ido-cr+-enable-next-call) (ido-cr+-enable-next-call nil)) (if (or ido-cr+-enable-this-call (not ido-cr+-replace-completely)) ad-do-it (message "Replacing ido-completing-read") (setq ad-return-value (apply (function ido-completing-read+) (ad-get-args 0))))))

(defvar ido-context-switch-command nil "\
Variable holding the command used for switching to another completion mode.

This variable is originally declared in `ido.el', but it is not
given a value (or a docstring). This documentation comes from a
re-declaration in `ido-completing-read+.el' that initializes it
to nil, which should suppress some byte-compilation warnings in
Emacs 25. Setting another package's variable is not safe in
general, but in this case it should be, because ido always
let-binds this variable before using it, so the initial value
shouldn't matter.")

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ido-completing-read+-autoloads.el ends here
