;;;;
;; Utils
;;;;


(defun take (n seq)
  "Returns a sequence of the first n itmes in seq, or all items if
   there are fewer than n."
  (let ((acc nil) (n1 n) (s1 seq))
    (while (and (> n1 0) s1)
      (setq acc (cons (car s1) acc))
      (setq n1 (1- n1) s1 (cdr s1)))
    (nreverse acc)))

(safe-do
 number-sequence
 (fset 'range 'number-sequence))

(safe-do
 cl-prettyexpand
 (fset 'pprint 'cl-prettyexpand))


(defmacro start-socks (&optional port server version)
  "Switch on url-gateway to socks, u should start socks proxy server outside Emacs."
  `(progn
     (setq url-gateway-method 'socks)
     (setq socks-server
           (list "Default server"
                 (if ,server ,server "127.0.0.1")
                 (if ,port ,port 32000)
                 (if ,version ,version 5)))))

(defmacro stop-socks (&optional method)
  "Switch off url-gateway to native."
  `(setq url-gateway-method
         (if ,method  ,method 'native)))


(defun int-to-binary-string (i)
  "Display an integer in binary string representation."
  (let ((s ""))
    (while (not (= i 0))
      (setq s (concat (if (= 1 (logand i 1)) "1" "0") s))
      (setq i (lsh i -1)))
    (concat "#b" (if (string= s "") (setq s "0") s))))


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
