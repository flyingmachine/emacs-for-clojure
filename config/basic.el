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
  (safe-do eww-browse-url
           (setq browse-url-browser-function 'eww-browse-url)))


;; Toggle linum mode 
(when (linum-mode-supported-p)
  (defun toggle-linum-mode ()
    "Toggle linum-mode."
    (interactive)
    (if (or (not (boundp 'linum-mode))
            (null linum-mode))
        (linum-mode t)
      (linum-mode -1))))


;; elisp basic setting
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (enable-eldoc-mode)
            (local-set-key (kbd "TAB") #'complete-symbol)
            (cond
             ((string= "*scratch*" (buffer-name))
              (local-set-key (kbd "RET")
                             (lambda ()
                               (interactive)
                               (eval-print-last-sexp)
                               (newline)))))))


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


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") #'ibuffer)
