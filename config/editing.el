;;
;; Editing buffer that related to configurations.
;;


;; Enable linum mode
(enable-global-linum-mode)

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

;; Under Terminal mode
(terminal-supported-p
 ;; current line highlight and background #3e4446
 ;; (face-attribute `hl-line' :background)
 ;; (set-face-background `hl-line' "#3e4446")
 ;; (version-supported-p <= 24 (global-hl-line-mode 1))
 ;; backspace may be a `c-h-' key
 ;;(normal-erase-is-backspace-mode)
 )

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)


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
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; yay rainbows!
;; (global-rainbow-delimiters-mode t)

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


;; prefer utf8
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


;; c/c++
(setq-default c-basic-offset 2)
(setq-default c-indentation 2)


;; Latin letters
(global-set-key (kbd "M-g a") "α")
(global-set-key (kbd "M-g b") "β")
(global-set-key (kbd "M-g g") "γ")
(global-set-key (kbd "M-g d") "δ")
(global-set-key (kbd "M-g e") "ε")
(global-set-key (kbd "M-g z") "ζ")
(global-set-key (kbd "M-g h") "η")
(global-set-key (kbd "M-g q") "θ")
(global-set-key (kbd "M-g i") "ι")
(global-set-key (kbd "M-g k") "κ")
(global-set-key (kbd "M-g l") "λ")
(global-set-key (kbd "M-g m") "μ")
(global-set-key (kbd "M-g n") "ν")
(global-set-key (kbd "M-g x") "ξ")
(global-set-key (kbd "M-g o") "ο")
(global-set-key (kbd "M-g p") "π")
(global-set-key (kbd "M-g r") "ρ")
(global-set-key (kbd "M-g s") "σ")
(global-set-key (kbd "M-g t") "τ")
(global-set-key (kbd "M-g u") "υ")
(global-set-key (kbd "M-g f") "ϕ")
(global-set-key (kbd "M-g j") "φ")
(global-set-key (kbd "M-g c") "χ")
(global-set-key (kbd "M-g y") "ψ")
(global-set-key (kbd "M-g w") "ω")
(global-set-key (kbd "M-g A") "Α")
(global-set-key (kbd "M-g B") "Β")
(global-set-key (kbd "M-g G") "Γ")
(global-set-key (kbd "M-g D") "Δ")
(global-set-key (kbd "M-g E") "Ε")
(global-set-key (kbd "M-g Z") "Ζ")
(global-set-key (kbd "M-g H") "Η")
(global-set-key (kbd "M-g Q") "Θ")
(global-set-key (kbd "M-g I") "Ι")
(global-set-key (kbd "M-g K") "Κ")
(global-set-key (kbd "M-g L") "Λ")
(global-set-key (kbd "M-g M") "Μ")
(global-set-key (kbd "M-g N") "Ν")
(global-set-key (kbd "M-g X") "Ξ")
(global-set-key (kbd "M-g O") "Ο")
(global-set-key (kbd "M-g P") "Π")
(global-set-key (kbd "M-g R") "Ρ")
(global-set-key (kbd "M-g S") "Σ")
(global-set-key (kbd "M-g T") "Τ")
(global-set-key (kbd "M-g U") "Υ")
(global-set-key (kbd "M-g F") "Φ")
(global-set-key (kbd "M-g J") "Φ")
(global-set-key (kbd "M-g C") "Χ")
(global-set-key (kbd "M-g Y") "Ψ")
(global-set-key (kbd "M-g W") "Ω")

