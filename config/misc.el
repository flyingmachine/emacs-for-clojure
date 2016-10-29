;;;;
;; Misc
;;;;


(defun pprint (form)
  ;; Pretty printed a Lisp FORM in current buffer
  (safe-call cl-prettyprint form))


(defun clean-compiled-files ()
  (let* ((home "~/.emacs.d/")
         (config (concat home "config/" emacs-version "/"))
         (private (concat home "private/n/" emacs-version "/")))
    (dolist (d (list config private))
      (dolist (f (directory-files d nil "\\.elc$"))
        (message "deleting file: %s" f)
        (delete-file (concat d f))))))
