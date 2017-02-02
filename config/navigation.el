;;;;
;; Navigation
;;;;


;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


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

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode 1)


;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
;; https://github.com/nonsequitur/smex
(safe-setq smex-save-file (concat user-emacs-directory ".smex-items"))
;; (smex-initialize)
(global-set-key (kbd "M-x") #'smex)

