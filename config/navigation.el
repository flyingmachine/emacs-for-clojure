;;;;
;; Navigation
;;;;


;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.



;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode 1)


;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
;; https://github.com/nonsequitur/smex
(global-set-key [(meta x)]
                (lambda ()
                  (interactive)
                  (or (boundp 'smex-cache)
                      (smex-initialize))
                  (global-set-key [(meta x)] 'smex)
                  (smex)))
(safe-setq smex-save-file (concat user-emacs-directory ".smex-items"))



;; on Drawin: ls does not support --dired;
;; see `dired-use-ls-dired' for more defails
(platform-supported-p
    darwin
  (setq-default ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp))

