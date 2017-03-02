;;
;; Editing buffer that related to configurations.
;;


;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)


;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


;; Highlights matching parenthesis
(show-paren-mode 1)


;; Interactive search key bindings.
;; By default, C-s runs isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Interactive query replace key bindings.
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace-regexp)


;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat "~/.emacs.d" "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist
      `(("." . ,(concat "~/.emacs.d" "backups"))))
(setq auto-save-default nil)


;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
                               (line-end-position)))
(graphic-supported-p
  (global-set-key (kbd "C-;") 'toggle-comment-on-line))
(terminal-supported-p
  (global-set-key (kbd "C-c ;") 'toggle-comment-on-line))


;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (safe-do ns-get-selection-internal
    (condition-case nil
        (ns-get-selection-internal 'CLIPBOARD)
      (quit nil))))


;; No need for ~ files when editing
(safe-setq create-lockfiles nil)


;; Don't use hard tabs
(setq-default indent-tabs-mode nil)


;; Disable electric indent mode 
(safe-setq electric-indent-mode nil)


;; Enable column number mode
(safe-setq column-number-mode t)


;; Default tab-width
(setq-default tab-width 4)


;; cc mode indent level
(add-hook 'c-mode-common-hook
          (lambda ()
            (safe-setq indent-tabs-mode nil)
            (safe-setq c-basic-offset 4)))


;; shell script mode tab-width
(add-hook 'sh-mode-hook (lambda () (safe-setq tab-width 2)))

(comment
 ;; Makefile tab-width
 (add-hook 'makefile-mode-hook (lambda () (safe-setq tab-width 4))))


;; preferred coding system
(prefer-coding-system 'utf-8)


;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(safe-setq recentf-save-file (concat "~/.emacs.d/" ".recentf"))
(safe-setq recentf-max-menu-items 40)
;; manually: (recentf-cleanup), view list: recentf-list
(setq-default recentf-auto-cleanup 'never)
(recentf-mode 1)


;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)


;; bing dict
(safe-do bing-dict-brief
  (global-set-key (kbd "C-c d") 'bing-dict-brief))


;; Greek letters C-x 8 <RET> greek small letter lambda
;; (global-set-key (kbd "C-c l") "λ")


;; magit
(safe-do magit-status
  (global-set-key (kbd "C-c g s") 'magit-status))
(safe-do magit-pull
  (global-set-key (kbd "C-c g p") 'magit-pull))
(safe-do magit-log
  (global-set-key (kbd "C-c g l") 'magit-log))
(safe-do magit-log-buffer-file
  (global-set-key (kbd "C-c g b") 'magit-log-buffer-file))
