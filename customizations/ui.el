;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

(tooltip-mode -1)                 ;; disable tooltips
(tool-bar-mode -1)                ;; the toolbar is pretty ugly
(scroll-bar-mode -1)              ;; disable visible scrollbar
(blink-cursor-mode 0)             ;; turn off blinking cursor. distracting!
(setq create-lockfiles nil)       ;; no need for ~ files when editing
(fset 'yes-or-no-p 'y-or-n-p)     ;; changes all yes/no questions to y/n type
(setq inhibit-startup-message t)  ;; go straight to scratch buffer on startup
(setq ring-bell-function 'ignore) ;; turn off audible bell

;; show full path in title bar
(setq-default frame-title-format "%b (%f)")

;; initial frame height and width
(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(width . 100))

;; increase font size for better readability
(set-face-attribute 'default nil :height 140)

;; on a Mac, don't pop up font menu
(when (string-equal system-type "darwin") 'ok
  (global-set-key (kbd "s-t") '(lambda () (interactive))))

;; doom is a whole Emacs distribution unto itself,
;; but it's got some really nice packages that you
;; can use a-la-carte. doom-modeline is simply a more
;; modern and more beautiful modeline.
;; doom-modeline uses nice icons from all-the-icons
(setup (:package all-the-icons))

;; for some reason, this crashes Emacs on Windows. Argh!
(setup (when (not (string-equal system-type "windows-nt"))
         (:package doom-modeline)
         (doom-modeline-mode t)))

;; Lots of great themes, both light ones
;; and dark ones. Use M-x load-theme to select one.
;; The first time you load one, it asks for
;; confirmation. You can see what they all
;; look like here:
;; https://github.com/doomemacs/themes/tree/screenshots
(setup (:package doom-themes)
  (when (not custom-enabled-themes)
    (load-theme 'doom-dracula t)))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; CUSTOMIZE

;; You can uncomment this to remove the graphical toolbar at the top. After
;; awhile, you won't need the toolbar.
;; (tool-bar-mode -1)

;; Your choice of font is very personal, and you must have installed it
;; on your system before you specify it here,
;; Some font suggestions:  https://www.creativebloq.com/features/the-best-monospace-fonts-for-coding
;; (set-face-attribute 'default nil :font "Fira Code")

