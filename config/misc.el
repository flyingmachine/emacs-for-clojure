;;;;
;; Misc
;;;;


(defun pprint (form)
  "Pretty printed a Lisp FORM in current buffer"
  (safe-call cl-prettyprint form))


(defun clean-compiled-files ()
  "Clean all compiled files, then you need restart emacs"
  (let* ((home "~/.emacs.d/")
         (config (concat home "config/" emacs-version "/"))
         (private (concat home "private/" emacs-version "/")))
    (dolist (d (list config private))
      (dolist (f (directory-files d nil "\\.elc$"))
        (message "deleting file: %s" f)
        (delete-file (concat d f))))))

(defun clone-themes ()
  "Clone themes from github, call it in elisp env."
  (let* ((url "https://github.com/chriskempson/tomorrow-theme/trunk/GNU%20Emacs")
         (dir "~/.emacs.d/themes")
         (saved (if (file-exists-p dir)
                    (let ((mv (format "mv %s %s.b0" dir dir)))
                      (zerop (shell-command mv)))
                  t)))
    (when (and saved (zerop (shell-command "type -p svn")))
      (let* ((clone (format "svn export %s %s" url dir))
             (done (if (zerop (shell-command clone)) "done" "failed")))
        (message "Clone themes ... %s" done)))))
