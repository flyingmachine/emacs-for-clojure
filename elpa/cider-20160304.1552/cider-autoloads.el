;;; cider-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "cider" "cider.el" (22235 10854 702620 171000))
;;; Generated autoloads from cider.el

(autoload 'cider-version "cider" "\
Display CIDER's version.

\(fn)" t nil)

(autoload 'cider-jack-in "cider" "\
Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.
If CLJS-TOO is non-nil, also start a ClojureScript REPL session with its
own buffer.

\(fn &optional PROMPT-PROJECT CLJS-TOO)" t nil)

(autoload 'cider-jack-in-clojurescript "cider" "\
Start a nREPL server and connect to it both Clojure and ClojureScript REPLs.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.

\(fn &optional PROMPT-PROJECT)" t nil)

(autoload 'cider-connect "cider" "\
Connect to an nREPL server identified by HOST and PORT.
Create REPL buffer and start an nREPL client connection.

When the optional param PROJECT-DIR is present, the connection
gets associated with it.

\(fn HOST PORT &optional PROJECT-DIR)" t nil)

(eval-after-load 'clojure-mode '(progn (define-key clojure-mode-map (kbd "C-c M-j") #'cider-jack-in) (define-key clojure-mode-map (kbd "C-c M-J") #'cider-jack-in-clojurescript) (define-key clojure-mode-map (kbd "C-c M-c") #'cider-connect)))

;;;***

;;;### (autoloads nil "cider-apropos" "cider-apropos.el" (22235 10853
;;;;;;  868618 849000))
;;; Generated autoloads from cider-apropos.el

(autoload 'cider-apropos "cider-apropos" "\
Show all symbols whose names match QUERY, a regular expression.
The search may be limited to the namespace NS, and may optionally search doc
strings, include private vars, and be case sensitive.

\(fn QUERY &optional NS DOCS-P PRIVATES-P CASE-SENSITIVE-P)" t nil)

(autoload 'cider-apropos-documentation "cider-apropos" "\
Shortcut for (cider-apropos <query> nil t).

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider-browse-ns" "cider-browse-ns.el" (22235
;;;;;;  10853 681618 552000))
;;; Generated autoloads from cider-browse-ns.el

(autoload 'cider-browse-ns "cider-browse-ns" "\
List all NAMESPACE's vars in BUFFER.

\(fn NAMESPACE)" t nil)

(autoload 'cider-browse-ns-all "cider-browse-ns" "\
List all loaded namespaces in BUFFER.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider-classpath" "cider-classpath.el" (22235
;;;;;;  10854 813620 347000))
;;; Generated autoloads from cider-classpath.el

(autoload 'cider-classpath "cider-classpath" "\
List all classpath entries.

\(fn)" t nil)

(autoload 'cider-open-classpath-entry "cider-classpath" "\
Open a classpath entry.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider-debug" "cider-debug.el" (22235 10853
;;;;;;  769618 692000))
;;; Generated autoloads from cider-debug.el

(autoload 'cider-debug-defun-at-point "cider-debug" "\
Instrument the \"top-level\" expression at point.
If it is a defn, dispatch the instrumented definition.  Otherwise,
immediately evaluate the instrumented expression.

While debugged code is being evaluated, the user is taken through the
source code and displayed the value of various expressions.  At each step,
a number of keys will be prompted to the user.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider-grimoire" "cider-grimoire.el" (22235
;;;;;;  10854 314619 556000))
;;; Generated autoloads from cider-grimoire.el

(autoload 'cider-grimoire-web "cider-grimoire" "\
Open grimoire documentation in the default web browser.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates.

\(fn &optional ARG)" t nil)

(autoload 'cider-grimoire "cider-grimoire" "\
Open grimoire documentation in a popup buffer.

Prompts for the symbol to use, or uses the symbol at point, depending on
the value of `cider-prompt-for-symbol'.  With prefix arg ARG, does the
opposite of what that option dictates.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "cider-inspector" "cider-inspector.el" (22235
;;;;;;  10854 201619 376000))
;;; Generated autoloads from cider-inspector.el

(autoload 'cider-inspect-last-sexp "cider-inspector" "\
Inspect the result of the the expression preceding point.

\(fn)" t nil)

(autoload 'cider-inspect-defun-at-point "cider-inspector" "\
Inspect the result of the \"top-level\" expression at point.

\(fn)" t nil)

(autoload 'cider-inspect-read-and-inspect "cider-inspector" "\
Read an expression from the minibuffer and inspect its result.

\(fn)" t nil)

(autoload 'cider-inspect "cider-inspector" "\
Inspect the result of the preceding sexp.

With a prefix argument ARG it inspects the result of the \"top-level\" form.
With a second prefix argument it prompts for an expression to eval and inspect.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "cider-macroexpansion" "cider-macroexpansion.el"
;;;;;;  (22235 10854 591619 995000))
;;; Generated autoloads from cider-macroexpansion.el

(autoload 'cider-macroexpand-1 "cider-macroexpansion" "\
Invoke \\=`macroexpand-1\\=` on the expression preceding point.
If invoked with a PREFIX argument, use \\=`macroexpand\\=` instead of
\\=`macroexpand-1\\=`.

\(fn &optional PREFIX)" t nil)

(autoload 'cider-macroexpand-all "cider-macroexpansion" "\
Invoke \\=`clojure.walk/macroexpand-all\\=` on the expression preceding point.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider-mode" "cider-mode.el" (22235 10853 946618
;;;;;;  972000))
;;; Generated autoloads from cider-mode.el

(defvar cider-mode-line '(:eval (format " cider[%s]" (cider--modeline-info))) "\
Mode line lighter for `cider-mode'.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
details about mode line templates.

Customize this variable to change how `cider-mode' displays its
status in the mode line.  The default value displays the current connection.
Set this variable to nil to disable the mode line
entirely.")

(custom-autoload 'cider-mode-line "cider-mode" t)

(autoload 'cider-mode "cider-mode" "\
Minor mode for REPL interaction from a Clojure buffer.

\\{cider-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "cider-scratch" "cider-scratch.el" (22235 10854
;;;;;;  46619 131000))
;;; Generated autoloads from cider-scratch.el

(autoload 'cider-scratch "cider-scratch" "\
Go to the scratch buffer named `cider-scratch-buffer-name'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "cider-selector" "cider-selector.el" (22235
;;;;;;  10854 758620 259000))
;;; Generated autoloads from cider-selector.el

(autoload 'cider-selector "cider-selector" "\
Select a new buffer by type, indicated by a single character.
The user is prompted for a single character indicating the method by
which to choose a new buffer.  The `?' character describes then
available methods.  OTHER-WINDOW provides an optional target.

See `def-cider-selector-method' for defining new methods.

\(fn &optional OTHER-WINDOW)" t nil)

;;;***

;;;### (autoloads nil nil ("cider-client.el" "cider-common.el" "cider-compat.el"
;;;;;;  "cider-doc.el" "cider-eldoc.el" "cider-interaction.el" "cider-overlays.el"
;;;;;;  "cider-pkg.el" "cider-popup.el" "cider-repl.el" "cider-resolve.el"
;;;;;;  "cider-stacktrace.el" "cider-test.el" "cider-util.el" "nrepl-client.el")
;;;;;;  (22235 10854 969556 290000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cider-autoloads.el ends here
