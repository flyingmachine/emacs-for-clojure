;;
;; Clone themes
;;

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
             (done (zerop (shell-command clone))))
        (when done
          (compile-and-load-elisp-files
           (directory-files dir nil ".*.el") "themes/"))))))
