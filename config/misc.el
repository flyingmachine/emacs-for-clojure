;;;;
;; Misc
;;;;


(defmacro start-socks (&optional port server version)
  `(progn
     (setq url-gateway-method 'socks)
     (setq socks-server
           (list "Default server"
                 (if ,server ,server "127.0.0.1")
                 (if ,port ,port 32000)
                 (if ,version ,version 5)))))

(defmacro stop-socks (&optional method)
  `(setq url-gateway-method
         (if ,method  ,method 'native)))

;; "Pretty printed a Lisp FORM in current buffer"
(safe-do
 cl-prettyexpand
 (fset 'pprint 'cl-prettyexpand))


(defun clone-themes ()
  "Clone themes from github, call it in elisp env."
  (let* ((url
          "https://github.com/chriskempson/tomorrow-theme/trunk/GNU%20Emacs")
         (dir "~/.emacs.d/themes")
         (saved (if (file-exists-p dir)
                    (let ((mv (format "mv %s %s.b0" dir dir)))
                      (bin-exists-p mv))
                  t)))
    (when (and saved (bin-exists-p "svn"))
      (let* ((clone (format "svn export %s %s" url dir))
             (done (if (bin-exists-p clone) "done" "failed")))
        (message "#Clone themes ... %s" done)))))
