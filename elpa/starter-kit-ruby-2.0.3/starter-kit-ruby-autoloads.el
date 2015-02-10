;;; starter-kit-ruby-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "starter-kit-ruby" "starter-kit-ruby.el" (21721
;;;;;;  36890 0 0))
;;; Generated autoloads from starter-kit-ruby.el

(eval-after-load 'ruby-mode '(progn (ignore-errors (require 'ruby-compilation)) (setq ruby-use-encoding-map nil) (add-hook 'ruby-mode-hook 'inf-ruby-keys) (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent) (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)))

(global-set-key (kbd "C-h S-r") 'ri)

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))

(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))

(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))

(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(add-to-list 'completion-ignored-extensions ".rbc")

(add-to-list 'completion-ignored-extensions ".rbo")

(defun pcomplete/rake nil "\
Completion rules for the `ssh' command." (pcomplete-here (pcmpl-rake-tasks)))

(defun pcmpl-rake-tasks nil "\
Return a list of all the rake tasks defined in the current
projects.  I know this is a hack to put all the logic in the
exec-to-string command, but it works and seems fast" (delq nil (mapcar (quote (lambda (line) (if (string-match "rake \\([^ ]+\\)" line) (match-string 1 line)))) (split-string (shell-command-to-string "rake -T") "[
]"))))

(defun rake (task) (interactive (list (completing-read "Rake (default: default): " (pcmpl-rake-tasks)))) (shell-command-to-string (concat "rake " (if (= 0 (length task)) "default" task))))

(eval-after-load 'ruby-compilation '(progn (defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist)) (let ((comp-buffer-name (format "*%s*" name))) (when (get-buffer comp-buffer-name) (with-current-buffer comp-buffer-name (delete-region (point-min) (point-max)))))) (ad-activate 'ruby-do-run-w/compilation)))

(setq rinari-major-modes (list 'mumamo-after-change-major-mode-hook 'dired-mode-hook 'ruby-mode-hook 'css-mode-hook 'yaml-mode-hook 'javascript-mode-hook))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; starter-kit-ruby-autoloads.el ends here
