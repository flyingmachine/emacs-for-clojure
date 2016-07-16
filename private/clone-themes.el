;;
;; Clone themes
;;

(defun clone-themes ()
  "Clone themes from github, call it in elisp env."
  (let* ((url "https://github.com/chriskempson/tomorrow-theme/trunk/GNU%20Emacs")
         (dir "~/.emacs.d/themes")
         (saved (if (file-exists-p dir)
                    (let ((mv (concat "mv " dir " " (concat dir ".b0"))))
                      (zerop (shell-command mv)))
                  t)))
    (when (and saved (zerop (shell-command "type -p svn")))
      (let ((cp (concat "svn export " url " " dir)))
        (shell-command cp)))))
