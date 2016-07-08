;;;;
;; Python
;;;;

(add-hook 'python-mode-hook
          (lambda ()
            ;; Enable paredit for Python
            (enable-paredit-mode)
            ;; Working with camel-case tokens
            (subword-mode)))

;; Using IPython as the Python shell
(when (boundp 'python-shell-interpreter)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter "-i"))

;; run-python on Windows has bugs if u met run-python-nt insdeed
(when (eq system-type 'windows-nt)
  (defun python-shell-parse-command-nt ()
    (let ((process-environment (python-shell-calculate-process-environment))
          (exec-path (python-shell-calculate-exec-path)))
      (format "%s %s"
              ;;(shell-quote-argument)
              (executable-find python-shell-interpreter)
              python-shell-interpreter-args)))
  (defun run-python-nt (cmd &optional dedicated show)
    (interactive
     (if current-prefix-arg
         (list
          (read-string "Run Python: " (python-shell-parse-command-nt))
          (y-or-n-p "Make dedicated process? ")
          (= (prefix-numeric-value current-prefix-arg) 4))
       (list (python-shell-parse-command-nt) nil t)))
    (python-shell-make-comint
     cmd (python-shell-get-process-name dedicated) show)
    dedicated))










