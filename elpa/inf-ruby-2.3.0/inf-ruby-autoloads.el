;;; inf-ruby-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (inf-ruby-console-default inf-ruby-console-gem
;;;;;;  inf-ruby-console-rails inf-ruby-console-auto inf-ruby-switch-setup
;;;;;;  run-ruby inf-ruby inf-ruby-minor-mode inf-ruby-setup-keybindings)
;;;;;;  "inf-ruby" "inf-ruby.el" (21031 23290 0 0))
;;; Generated autoloads from inf-ruby.el

(autoload 'inf-ruby-setup-keybindings "inf-ruby" "\
Hook up `inf-ruby-minor-mode' to each of `ruby-source-modes'.

\(fn)" nil nil)

(autoload 'inf-ruby-minor-mode "inf-ruby" "\
Minor mode for interacting with the inferior process buffer.

\(fn &optional ARG)" t nil)

(autoload 'inf-ruby "inf-ruby" "\
Run an inferior Ruby process in a buffer.
With prefix argument, prompts for which Ruby implementation
\(from the list `inf-ruby-implementations') to use. Runs the
hooks `inf-ruby-mode-hook' (after the `comint-mode-hook' is
run).

\(fn &optional IMPL)" t nil)

(autoload 'run-ruby "inf-ruby" "\
Run an inferior Ruby process, input and output via buffer *ruby*.
If there is a process already running in `*ruby*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `ruby-program-name').  Runs the hooks `inferior-ruby-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn &optional COMMAND NAME)" t nil)

(autoload 'inf-ruby-switch-setup "inf-ruby" "\
Modify `rspec-compilation-mode' and `ruby-compilation-mode'
keymaps to bind `inf-ruby-switch-from-compilation' to `!-x C-q'.

\(fn)" nil nil)

(autoload 'inf-ruby-console-auto "inf-ruby" "\
Automatically determine the appropriate Ruby console command
and the directory to run it from.

\(fn)" t nil)

(autoload 'inf-ruby-console-rails "inf-ruby" "\
Run Rails console in DIR.

\(fn DIR)" t nil)

(autoload 'inf-ruby-console-gem "inf-ruby" "\
Run IRB console for the gem in DIR.
The main module should be loaded automatically. If DIR contains a
Gemfile, it should use the `gemspec' instruction.

\(fn DIR)" t nil)

(autoload 'inf-ruby-console-default "inf-ruby" "\
Run racksh, custom console.rb, or just IRB, in DIR.

\(fn DIR)" t nil)
 (inf-ruby-setup-keybindings)

;;;***

;;;### (autoloads nil nil ("inf-ruby-pkg.el") (21031 23290 158755
;;;;;;  0))

;;;***

(provide 'inf-ruby-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; inf-ruby-autoloads.el ends here
