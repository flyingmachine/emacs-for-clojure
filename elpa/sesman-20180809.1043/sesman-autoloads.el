;;; sesman-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "sesman" "sesman.el" (23424 32867 181973 236000))
;;; Generated autoloads from sesman.el

(autoload 'sesman-start "sesman" "\
Start a Sesman session.

\(fn)" t nil)

(autoload 'sesman-restart "sesman" "\
Restart sesman session.
When WHICH is nil, restart the current session; when a single universal
argument or 'linked, restart all linked sessions; when a double universal
argument, t or 'all, restart all sessions. For programmatic use, WHICH can also
be a session or a name of the session, in which case that session is restarted.

\(fn &optional WHICH)" t nil)

(autoload 'sesman-quit "sesman" "\
Terminate a Sesman session.
When WHICH is nil, kill only the current session; when a single universal
argument or 'linked, kill all linked sessions; when a double universal argument,
t or 'all, kill all sessions. For programmatic use, WHICH can also be a session
or a name of the session, in which case that session is killed.

\(fn &optional WHICH)" t nil)

(autoload 'sesman-info "sesman" "\
Display linked sessions info.
When ALL is non-nil, show info for all sessions.

\(fn &optional ALL)" t nil)

(autoload 'sesman-link-with-buffer "sesman" "\
Associate SESSION with BUFFER.
BUFFER defaults to current buffer. On universal argument, or if BUFFER is 'ask,
ask for buffer.

\(fn &optional BUFFER SESSION)" t nil)

(autoload 'sesman-link-with-directory "sesman" "\
Associate a SESSION with DIR.
DIR defaults to `default-directory'. On universal argument, or if DIR is 'ask,
ask for directory.

\(fn &optional DIR SESSION)" t nil)

(autoload 'sesman-link-with-project "sesman" "\
Link the SESSION with PROJECT.
PROJECT defaults to current project. On universal argument, or if PROJECT is
'ask, ask for the project.

\(fn &optional PROJECT SESSION)" t nil)

(autoload 'sesman-unlink "sesman" "\
Break any of the previously created links.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "sesman-browser" "sesman-browser.el" (23424
;;;;;;  32867 237974 481000))
;;; Generated autoloads from sesman-browser.el

(autoload 'sesman-browser "sesman-browser" "\
Display an interactive session browser.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("sesman-pkg.el") (23424 32866 865966 208000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; sesman-autoloads.el ends here
