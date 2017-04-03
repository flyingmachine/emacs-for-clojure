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

(defvar v-dir (concat (if (display-graphic-p) "g_" "t_")
                      emacs-version)
  "versionized dir based on grahpic/terminal mode and Emacs's version")

(defmacro make-vdir (subdir)
  "Make the versionized subdir under ~/.emacs.d and returns it. "
  (let ((_vdir_ (concat "~/.emacs.d/" subdir v-dir "/")))
    `(progn
       (when (not (file-exists-p ,_vdir_))
         (make-directory ,_vdir_ t))
       ,_vdir_)))

(defmacro compile-and-load-elisp-files (files subdir)
  "Compile and load the elisp files under the subdir."
  `(let ((d (concat "~/.emacs.d/" ,subdir))
         (v (make-vdir ,subdir)))
     (dolist (f ,files)
       (let ((from (concat d f)))
         (if (file-exists-p from)
             (let ((c (replace-regexp-in-string "\.el$" "\.elc" f)))
               (when (or (not (file-exists-p (concat v c)))
                         (file-newer-than-file-p from (concat v c)))
                 (copy-file from (concat v f) t)
                 (byte-compile-file (concat v f)))
               (load (concat v c)))
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

(defmacro graphic-supported-if (then &rest else)
  (declare (indent 1))
  (if (display-graphic-p)
      `,then
    `(progn ,@else)))

(defmacro bin-exists-p (b)
  "Returns true if b exists in env."
  (if (eq system-type 'windows-nt)
      `(zerop (shell-command (concat "where " ,b " >nul 2>&1")))
    `(zerop (shell-command (concat "hash " ,b " &>/dev/null")))))

(defmacro shell-command-to-string-no-newline (c)
  `(replace-regexp-in-string "\n$" "" (shell-command-to-string ,c)))

(defmacro bin-path (b)
  "Returns the path of b in env."
  (if (eq system-type 'windows-nt)
      `(shell-command-to-string-no-newline (concat "where " ,b))
    `(shell-command-to-string-no-newline (concat "type -P " ,b))))

(defmacro windows-nt-path (p)
  "Return the path that windows-nt can recoganized."
  `(replace-regexp-in-string "\\\\" "/" ,p))

(defmacro safe-call (fn &rest args)
  "Call fn with args when fn has been bound"
  (declare (indent 1))
  `(when (fboundp ',fn) (,fn ,@args)))

(defmacro safe-do-when (fn &rest body)
  "Do body when fn is bound"
  (declare (indent 1))
  `(when (fboundp ',fn) ,@body))

(defmacro safe-do-if (fn then &rest else)
  "Do body when fn is bound"
  (declare (indent 2))
  (if (fboundp fn)
      `,then
    `(progn ,@else)))

(defmacro safe-setq (x val)
  "Set x when x has been bound"
  `(when (boundp ',x) (setq ,x ,val)))

(defmacro safe-setq* (x &rest body)
  "Do body when x is bound"
  (declare (indent 1))
  `(when (boundp ,x) ,@body))

(defmacro self-symbol (name)
  `(intern (format "self-%s-%s" system-type ,name)))

(defmacro start-socks (&optional port server version)
  "Switch on url-gateway to socks"
  `(version-supported-p < 22
     (require 'url)
     (setq url-gateway-method 'socks)
     (setq-default socks-server
                   (list "Default server"
                         (if ,server ,server "127.0.0.1")
                         (if ,port ,port 32000)
                         (if ,version ,version 5)))))

(defmacro stop-socks (&optional method)
  "Switch off url-gateway to native."
  `(version-supported-p < 22
     (require 'url)
     (setq url-gateway-method
           (if ,method  ,method 'native))))

(defmacro clean-compiled-files ()
  "Clean all compiled files, need restart Emacs."
  `(let* ((config (make-vdir "config/"))
          (private (make-vdir "private/")))
     (dolist (d (list config private))
       (dolist (f (directory-files d nil "\\.elc$"))
         (message "#Clean compiled file: %s" f)
         (delete-file (concat d f))))))

(defmacro clean-saved-user-files ()
  "Clean saved desktop, need restart Emacs."
  `(let ((dirs (list (make-vdir ".auto-save/")
                     (make-vdir ".desktop/")
                     (make-vdir ".bookmarks/")
                     (make-vdir ".ido/")
                     (make-vdir ".recentf/")
                     (make-vdir ".places/"))))
     (dolist (d dirs)
       (when (file-exists-p d)
         (dolist (f (directory-files d nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
           (message "#Clean saved user file: %s" (concat d f))
           (delete-file (concat d f)))))
     (dolist (f (directory-files "~/.emacs.d/" nil "^recentf*"))
       (when (file-exists-p f)
         (message "#Clean saved user file: %s" f)
         (delete-file f)))))

(defmacro reset-emacs ()
  "Clean all compiled file and desktop, then restart Emacs."
  `(progn
     (clean-compiled-files)
     (clean-saved-user-files)
     (kill-emacs 0)))



;; (message "PATH=%s" (getenv "PATH"))

;; First to load self, env parts
(setq-default recentf-save-file (concat (make-vdir ".recentf/") "recentf"))
(setq-default savehist-file (concat (make-vdir ".minibuffer/") "history"))
(compile-and-load-elisp-files '("self.el") "private/")
(compile-and-load-elisp-files '("ui.el"
                                "shell.el"
                                "basic.el")
                              "config/")

;; Self do prelogue ...
(let ((ss (self-symbol "prelogue")))
  (safe-setq* ss (funcall (symbol-value ss))))


;; Start loading ...
(package-supported-p
  ;; define package user dir
  (setq package-user-dir (concat user-emacs-directory "elpa/" v-dir))
  ;; define package repositories
  (setq package-archives
        (append (list '("gnu" . "http://elpa.gnu.org/packages/")
                      '("melpa-stable" . "http://stable.melpa.org/packages/"))
                (version-supported-p
                    <= 25.1
                  (list '("melpa" . "http://melpa.org/packages/")))))

  (version-supported-p
      <= 25.1
    (setq-default package-archive-priorities
                  (list '("melpa-stable" . 10)
                        '("melpa" . 5)
                        '("gnu" . 0))))
  
  (require 'package)
  (package-initialize)

  ;; load self packages spec
  (setf (symbol-value (self-symbol "packages"))
        (when (fboundp 'self-package-spec)
          (self-package-spec)))

  ;; guarantee all packages are installed on start
  (defvar installed-packages
    (let* ((basic '(aggressive-indent
                    bing-dict
                    ido-ubiquitous
                    markdown-mode
                    paredit
                    rainbow-delimiters
                    smex
                    sx
                    tagedit))
           (self (let ((ss (self-symbol "packages")))
                   (safe-setq* ss (symbol-value ss)))))
      (append basic (when self self))))

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
   (let* ((basic '("setup-lisp.el"
                   "setup-navigation.el"
                   "setup-python.el"))
          (self (let ((ss (self-symbol "packages-setup")))
                  (safe-setq* ss (symbol-value ss)))))
     (append basic self))
   "config/"))
  
 ;; ^ end of support-package-p


(compile-and-load-elisp-files
 ;; compile and load non-package-required elisp files
 '("editing.el"
   "debugger.el") "config/")

(compile-and-load-elisp-files
 ;; compile and load private non-package-required elisp files
 '("financial.el"
   "utils.el") "private/")


;; Self do epilogue ...
(let ((ss (self-symbol "epilogue")))
  (safe-setq* ss (funcall (symbol-value ss))))


;; After loaded ...
(let ((elapsed
       (float-time
        (time-subtract (current-time) loading-start-time))))
  (message "#Loading init.el ... done (%.3fs)" elapsed))

;; ^ End of init.el
