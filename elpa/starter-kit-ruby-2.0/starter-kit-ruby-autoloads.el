;;; starter-kit-ruby-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "starter-kit-ruby" "starter-kit-ruby.el" (20184
;;;;;;  56042))
;;; Generated autoloads from starter-kit-ruby.el

(eval-after-load 'ruby-mode '(progn (ignore-errors (require 'ruby-compilation)) (setq ruby-use-encoding-map nil) (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent) (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)))

(global-set-key (kbd "C-h S-r") 'ri)

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))

(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))

(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(add-to-list 'completion-ignored-extensions ".rbc")

(defun pcomplete/rake nil "\
Completion rules for the `ssh' command." (pcomplete-here (pcmpl-rake-tasks)))

(defun pcmpl-rake-tasks nil "\
Return a list of all the rake tasks defined in the current
projects.  I know this is a hack to put all the logic in the
exec-to-string command, but it works and seems fast" (delq nil (mapcar (quote (lambda (line) (if (string-match "rake \\([^ ]+\\)" line) (match-string 1 line)))) (split-string (shell-command-to-string "rake -T") "[
]"))))

(defun rake (task) (interactive (list (completing-read "Rake (default: default): " (pcmpl-rake-tasks)))) (shell-command-to-string (concat "rake " (if (= 0 (length task)) "default" task))))

(eval-after-load 'ruby-compilation '(progn (defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist)) (let ((comp-buffer-name (format "*%s*" name))) (when (get-buffer comp-buffer-name) (with-current-buffer comp-buffer-name (delete-region (point-min) (point-max)))))) (ad-activate 'ruby-do-run-w/compilation)))

(add-hook 'ruby-mode-hook 'esk-run-coding-hook)

(add-hook 'ruby-mode-hook 'esk-paredit-nonlisp)

(setq rinari-major-modes (list 'mumamo-after-change-major-mode-hook 'dired-mode-hook 'ruby-mode-hook 'css-mode-hook 'yaml-mode-hook 'javascript-mode-hook))

;;;***

;;;### (autoloads nil nil ("starter-kit-ruby-pkg.el") (20184 56042
;;;;;;  635313))

;;;***

(provide 'starter-kit-ruby-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; starter-kit-ruby-autoloads.el ends here
