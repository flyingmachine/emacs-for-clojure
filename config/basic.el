;;;;
;; Basic
;;;;


(defmacro enable-eldoc-mode ()
  "After Emacs 24.4 `turn-on-eldoc-mode is obsoleted, use `eldoc-mode indeed.
  `eldoc-mode shows documentation in the minibuffer when writing code.
  http://www.emacswiki.org/emacs/ElDoc"
  `(version-supported-if < 24.0
                         (eldoc-mode)
                         (turn-on-eldoc-mode)))


(defmacro linum-mode-supported-p ()
  "Check Emacs version supports linum mode. "
  `(version-supported-p <= 23.1 t))


;; Default web browser: eww `C-d C-d h'
(when (eq browse-url-browser-function
          'browse-url-default-browser)
  (safe-do-when eww-browse-url
    (setq browse-url-browser-function 'eww-browse-url)))


;; Toggle linum mode 
(when (linum-mode-supported-p)
  (defun toggle-linum-mode ()
    "Toggle linum-mode."
    (interactive)
    (if (or (not (boundp 'linum-mode))
            (null linum-mode))
        (linum-mode t)
      (linum-mode -1)))
  (global-set-key (kbd "C-c l") 'toggle-linum-mode))


;; elisp basic setting
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (enable-eldoc-mode)
            (version-supported-if
             <= 24.4
             (local-set-key (kbd "TAB") #'completion-at-point)
             (local-set-key (kbd "TAB") #'lisp-complete-symbol))
            (cond
             ((string= "*scratch*" (buffer-name))
              (local-set-key (kbd "RET")
                             (lambda ()
                               (interactive)
                               (eval-print-last-sexp)
                               (newline)))
              (version-supported-p > 24
                (local-set-key (kbd "C-j")
                               (lambda ()
                                 (interactive)
                                 (newline-and-indent))))))))


;; ielm 
(add-hook 'ielm-mode-hook
          (lambda () (enable-eldoc-mode)))


(defmacro safe-setq-inferior-lisp-program (lisp &optional force)
  "Safe set inferior-lisp-program var, it must be set before slime start."
  `(if (boundp 'inferior-lisp-program)
       (if ,force
           (setq inferior-lisp-program ,lisp)
         (when (or (not (string= ,lisp inferior-lisp-program))
                   (string= "lisp" inferior-lisp-program))
           (setq inferior-lisp-program ,lisp)))
     (setq-default inferior-lisp-program ,lisp)))


;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") #'ibuffer)


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)


;; Where to save ido.last
(let ((d (make-vdir ".ido/")))
  (setq-default ido-save-directory-list-file (concat d "ido.last")))

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(safe-setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(safe-setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(safe-setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(safe-setq ido-use-virtual-buffers t)



;; on Drawin: ls does not support --dired;
;; see `dired-use-ls-dired' for more defails
(platform-supported-p
    darwin
  (setq-default ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp))
