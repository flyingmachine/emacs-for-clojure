;; LaTeX

(set-variable (quote latex-run-command) "pdflatex")
(set-variable (quote tex-dvi-view-command) "evince")

(defun tex-view ()
    (interactive)
    (tex-send-command "evince" (tex-append tex-print-file ".pdf")))

(defun tex-print (&optional alt)
  (interactive "P")
  (let ((print-file-name-dvi (tex-append tex-print-file ".pdf"))
        test-name)
    (if (and (not (equal (current-buffer) tex-last-buffer-texed))
             (buffer-file-name)
             ;; Check that this buffer's printed file is up to date.
             (file-newer-than-file-p
              (setq test-name (tex-append (buffer-file-name) ".pdf"))
              (buffer-file-name)))
        (setq print-file-name-dvi test-name))
    (if (not (file-exists-p print-file-name-dvi))
        (error "No appropriate `.dvi' file could be found")
      (if (tex-shell-running)
          (tex-kill-job)
        (tex-start-shell))
      (tex-send-command
       (if alt tex-alt-dvi-print-command tex-dvi-print-command)
       print-file-name-dvi t))))

