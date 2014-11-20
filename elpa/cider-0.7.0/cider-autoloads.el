;;; cider-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (cider-connect cider-jack-in cider-version) "cider"
;;;;;;  "cider.el" (21475 55184 0 0))
;;; Generated autoloads from cider.el

(autoload 'cider-version "cider" "\
Display CIDER's version.

\(fn)" t nil)

(autoload 'cider-jack-in "cider" "\
Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.

\(fn &optional PROMPT-PROJECT)" t nil)

(autoload 'cider-connect "cider" "\
Connect to an nREPL server identified by HOST and PORT.

\(fn HOST PORT)" t nil)

(eval-after-load 'clojure-mode '(progn (define-key clojure-mode-map (kbd "C-c M-j") 'cider-jack-in) (define-key clojure-mode-map (kbd "C-c M-c") 'cider-connect)))

;;;***

;;;### (autoloads (cider-inspect) "cider-inspector" "cider-inspector.el"
;;;;;;  (21475 55183 0 0))
;;; Generated autoloads from cider-inspector.el

(autoload 'cider-inspect "cider-inspector" "\
Eval the string EXPRESSION and inspect the result.

\(fn EXPRESSION)" t nil)

;;;***

;;;### (autoloads (cider-macroexpand-all cider-macroexpand-1) "cider-macroexpansion"
;;;;;;  "cider-macroexpansion.el" (21475 55184 0 0))
;;; Generated autoloads from cider-macroexpansion.el

(autoload 'cider-macroexpand-1 "cider-macroexpansion" "\
Invoke 'macroexpand-1' on the expression preceding point.
If invoked with a PREFIX argument, use 'macroexpand' instead of
'macroexpand-1'.

\(fn &optional PREFIX)" t nil)

(autoload 'cider-macroexpand-all "cider-macroexpansion" "\
Invoke 'clojure.walk/macroexpand-all' on the expression preceding point.

\(fn)" t nil)

;;;***

;;;### (autoloads (cider-mode cider-mode-line) "cider-mode" "cider-mode.el"
;;;;;;  (21475 55184 0 0))
;;; Generated autoloads from cider-mode.el

(defvar cider-mode-line '(" cider" (:eval (format "[%s]" (cider-current-ns)))) "\
Mode line ligher for `cider-mode'.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
details about mode line templates.

Customize this variable to change how `cider-mode' displays its
status in the mode line.  The default value displays the current ns.
Set this variable to nil to disable the mode line
entirely.")

(custom-autoload 'cider-mode-line "cider-mode" t)

(autoload 'cider-mode "cider-mode" "\
Minor mode for REPL interaction from a Clojure buffer.

\\{cider-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (cider-scratch) "cider-scratch" "cider-scratch.el"
;;;;;;  (21475 55183 0 0))
;;; Generated autoloads from cider-scratch.el

(autoload 'cider-scratch "cider-scratch" "\
Create a scratch buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (cider-selector) "cider-selector" "cider-selector.el"
;;;;;;  (21475 55183 0 0))
;;; Generated autoloads from cider-selector.el

(autoload 'cider-selector "cider-selector" "\
Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer.  The `?' character describes then
available methods.  OTHER-WINDOW provides an optional target.

See `def-cider-selector-method' for defining new methods.

\(fn &optional OTHER-WINDOW)" t nil)

;;;***

;;;### (autoloads nil nil ("cider-client.el" "cider-doc.el" "cider-eldoc.el"
;;;;;;  "cider-interaction.el" "cider-pkg.el" "cider-repl.el" "cider-stacktrace.el"
;;;;;;  "cider-test.el" "cider-util.el" "nrepl-client.el") (21475
;;;;;;  55184 367849 0))

;;;***

(provide 'cider-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cider-autoloads.el ends here
