(defmacro comment (&rest body)
  "Ignores body, yields nil."
  nil)

(defmacro time (expr &optional what)
  "Evaluates expr and prints the time it took. Returns the value of expr."
  `(let ((start (current-time))
         (return ,expr))
     (print (format "Elapsed %f secs%s"
                    (float-time
                     (time-subtract (current-time) start))
                    (if ,what (concat " <- " ,what " .") ".")))
     return))

(defvar loading-start-time
  (current-time) "The start time at loading init.el")

(defvar g-or-t? (if (display-graphic-p) "g_" "t_")
  "g_ or t_ prefix for compiled directory")


(defmacro compile-and-load-elisp-files (files subdir)
  "Compile and load the elisp files under the subdir."
  `(let* ((d (concat "~/.emacs.d/" ,subdir))
          (v (concat d g-or-t? emacs-version "/")))
     (when (not (file-exists-p v)) (make-directory v t))
     (dolist (f ,files)
       (let ((from (concat d f)))
         (if (file-exists-p from)
             (let* ((c (replace-regexp-in-string "\.el$" "\.elc" f))
                    (to (concat v c))
                    (s (concat v f)))
               (when (or (not (file-exists-p to))
                         (file-newer-than-file-p from to))
                 (copy-file from s t)
                 (byte-compile-file s))
               (load to))
           (message "#Skip compile and load %s.done" from))))))


(defmacro package-supported-p (&rest body)
  "Run body code if the Emacs supports package."
  (declare (indent 0))
  (when (>= emacs-major-version 24)
    `(progn ,@body)))

(defmacro platform-supported-p (os &rest body)
  "Run body code if the Emacs on specified OS platform."
  (declare (indent 1))
  (when (eq system-type os)
    `(progn ,@body)))

(defmacro platform-supported-unless (os &rest body)
  "Run body code if the Emacs on specified unless OS platforms"
  (declare (indent 1))
  (unless (eq system-type os)
    `(progn ,@body)))

(defmacro version-supported-p (c v &rest body)
  "Run body code if the Emacs on specified version."
  (declare (indent 2))
  (when (funcall c v (string-to-number emacs-version))
    `(progn ,@body)))

(defmacro version-supported-if (c v then &optional else)
  "Do else if test yields nil, if true do then"
  (if (funcall c v (string-to-number emacs-version))
      `,then
    `,else))

(defmacro graphic-supported-p (&rest body)
  "Run body code if the Emacs on graphic mode."
  (declare (indent 0))
  (when (display-graphic-p)
    `(progn ,@body)))

(defmacro terminal-supported-p (&rest body)
  "Run body code if the Emacs on terminal mode."
  (declare (indent 0))
  (unless (display-graphic-p)
    `(progn ,@body)))

(defmacro bin-exists-p (b)
  "Returns true if b exists in env."
  (if (eq system-type 'windows-nt)
      `(zerop (shell-command (concat "where " ,b " >nul 2>&1")))
    `(zerop (shell-command (concat "hash " ,b " &>/dev/null")))))

(defmacro bin-path (b)
  "Returns the path of b in env."
  (if (eq system-type 'windows-nt)
      `(shell-command-to-string (concat "where " ,b))
    `(shell-command-to-string (concat "type -P " ,b))))

(defmacro windows-nt-path (p)
  "Return the path that windows-nt can recoganized."
  `(replace-regexp-in-string "\\\\" "/" ,p))

(defmacro safe-call (fn &rest args)
  "Call fn with args when fn has been bound"
  (declare (indent 1))
  `(when (fboundp ',fn) (,fn ,@args)))

(defmacro safe-do (fn &rest body)
  "Do body when fn is bound"
  (declare (indent 1))
  `(when (fboundp ',fn) ,@body))

(defmacro safe-setq (x val)
  "Set x when x has been bound"
  `(when (boundp ',x) (setq ,x ,val)))

(defmacro safe-setq* (x &rest body)
  "Do body when x is bound"
  (declare (indent 1))
  `(when (boundp ,x) ,@body))

(defmacro self-symbol (name)
  `(intern (format "self-%s-%s" system-type ,name)))

(defmacro clean-compiled-files ()
  "Clean all compiled files, need restart Emacs."
  `(let* ((home "~/.emacs.d/")
          (config (concat home "config/"
                          g-or-t? emacs-version "/"))
          (private (concat home "private/"
                           g-or-t? emacs-version "/")))
     (dolist (d (list config private))
       (dolist (f (directory-files d nil "\\.elc$"))
         (message "#Clean compiled file: %s" f)
         (delete-file (concat d f))))))

(defmacro clean-saved-desktop ()
  "Clean saved desktop, need restart Emacs."
  `(let ((d "~/.emacs.d/.emacs.desktop"))
     (when (file-exists-p d)
       (message "#Clean desktop file: %s" d)
       (delete-file d))))

(defmacro reset-emacs ()
  "Clean all compiled file and desktop, then restart Emacs."
  `(progn
     (clean-compiled-files)
     (graphic-supported-p (clean-saved-desktop))
     (kill-emacs 0)))



(message "PATH=%s" (getenv "PATH"))

;; First to load self, env parts
(compile-and-load-elisp-files '("self.el") "private/")
(compile-and-load-elisp-files '("ui.el"
                                "basic.el"
                                "setup-shell.el")
                              "config/")

(message "PATH=%s" (getenv "PATH"))

;; Start loading ...
(package-supported-p
  ;; define package repositories
  (setq package-archives
        (append (list '("gnu" . "http://elpa.gnu.org/packages/")
                      '("melpa-stable" . "http://stable.melpa.org/packages/"))
                (version-supported-p
                    <= 25.1
                  (list '("melpa" . "http://melpa.org/packages/")))))

  (version-supported-p
      <= 25.1
    (safe-setq package-archive-priorities
               (list '("gnu" . 10)
                     '("melpa-stable" . 80)
                     '("melpa" . 0))))
  
  (require 'package)
  (package-initialize)

  ;; packages based on existings
  (defvar has-docker (bin-exists-p "docker"))
  (defvar has-erlang (bin-exists-p "erl"))
  (defvar has-latex (bin-exists-p "latex"))
  (defvar has-java (bin-exists-p "java"))
  (defvar has-racket (bin-exists-p "racket"))
  (defvar has-sbcl (bin-exists-p "sbcl"))

  ;; guarantee all packages are installed on start
  (defvar installed-packages
    (let* ((basic '(aggressive-indent
                    bing-dict
                    ido-ubiquitous
                    markdown-mode
                    paredit
                    rainbow-delimiters
                    smex
                    tagedit))
           (docker '(dockerfile-mode docker-tramp))
           (erlang '(erlang lfe-mode))
           (latex '(auctex))
           (java '(cider clojure-mode clojure-mode-extra-font-locking))
           (racket '(geiser))
           (sbcl '(slime))
           (self (let ((ss (self-symbol "packages")))
                   (safe-setq* ss (symbol-value ss)))))
      (append basic
              (version-supported-p <= 25.1 '(ereader))
              (version-supported-p <= 24.4 (when has-docker docker))
              (version-supported-p <= 24.4 '(magit))
              (version-supported-p <= 24.4 (when has-java java))
              (version-supported-p <= 23.2 (when has-racket racket))
              (when has-erlang erlang)
              (when has-latex latex)
              (when has-sbcl sbcl)
              (when self self))))

  (version-supported-p
      <= 25.1
    (safe-setq package-selected-packages installed-packages))

  (let ((not-installed-packages
         (delete t (mapcar #'(lambda (p)
                               (if (package-installed-p p) t p))
                           installed-packages))))
    (when not-installed-packages
      (package-refresh-contents)
      (message "#Installing the missing %d packages: %s"
               (length not-installed-packages)
               not-installed-packages)
      (mapcar #'(lambda (i) (package-install i))
              not-installed-packages)))

  (compile-and-load-elisp-files
   ;; compile and load basic elisp files
   (let* ((basic '("lisp.el"
                   "navigation.el"
                   "setup-python.el"))
          (lfe (when has-erlang '("setup-lfe.el")))
          (clojure (when has-java '("setup-clojure.el")))
          (sbcl (when has-sbcl '("setup-sbcl.el"))))
     (append basic lfe clojure sbcl))
   "config/"))
  
 ;; ^ end of support-package-p


(compile-and-load-elisp-files
 ;; compile and load non-package-required elisp files
 '("editing.el"
   "setup-debugger.el") "config/")

(compile-and-load-elisp-files
 ;; compile and load private non-package-required elisp files
 '("financial.el"
   "utils.el") "private/")


;; Self post do ...
(let ((ss (self-symbol "post-do")))
  (safe-setq* ss (funcall (symbol-value ss))))


;; After loaded ...
(let ((elapsed
       (float-time
        (time-subtract (current-time) loading-start-time))))
  (message "#Loading init.el ... done (%.3fs)" elapsed))

;; ^ End of init.el
