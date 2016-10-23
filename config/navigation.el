;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(safe-set! recentf-save-file
           (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(safe-set! recentf-max-menu-items 40)


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(safe-set! ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(safe-set! ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(safe-set! ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(safe-set! ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode 1)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(safe-set! smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(comment
 ;; projectile everywhere!
 (when (package-installed-p 'projectile)
   (defun projectile-project-root ()
     "Retrieves the root directory of a project if available.
      The current directory is assumed to be the project's 
      root otherwise."
     (let ((dir default-directory))
       (or (--reduce-from
            (or acc
                (let* ((cache-key (format "%s-%s" it dir))
                       (cache-value
                        (gethash
                         cache-key
                         projectile-project-root-cache)))
                  (if cache-value
                      (if (eq cache-value 'no-project-root)
                          nil
                        cache-value)
                    (let ((value
                           (funcall it (file-truename dir))))
                      (puthash cache-key
                               (or value 'no-project-root)
                               projectile-project-root-cache)
                      value))))
            nil
            projectile-project-root-files-functions)
           (if projectile-require-project-root
               (error "You're not in a project")
             default-directory))))
   (projectile-global-mode)))
