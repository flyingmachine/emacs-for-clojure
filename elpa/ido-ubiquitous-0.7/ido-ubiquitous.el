
;;; ido-ubiquitous.el --- Use ido (nearly) everywhere.

;; Author: Ryan C. Thompson
;; URL: https://github.com/DarwinAwardWinner/ido-ubiquitous
;; Version: 0.7
;; Created: 2011-09-01
;; Keywords: convenience
;; EmacsWiki: InteractivelyDoThings

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; You may have seen the `ido-everywhere' variable in ido.el and got
;; excited that you could use ido completion for everything. Then you
;; were probably disappointed when you realized that it only applied
;; to *file names* and nothing else. Well, ido-ubiquitous is here to
;; fulfill the original promise and let you use ido completion for
;; (almost) any command that uses `completing-read' to offer you a
;; choice of several alternatives.

;; This even works in M-x, but for that, you might prefer the "smex"
;; package instead.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'ido)

;;;###autoload
(defgroup ido-ubiquitous nil
  "Use ido for (almost) all completion."
  :group 'ido)

;;;###autoload
(define-minor-mode ido-ubiquitous
  "Use `ido-completing-read' instead of `completing-read' almost everywhere.

  This mode has no effect unles `ido-mode' is also enabled.

  If this mode causes problems for a function, you can force the
  function to use the original completing read by using the macro
  `ido-ubiquitous-disable-in'. For example, if a
  function `foo' cannot work with ido-style completion, evaluate
  the following (for example by putting it in your .emacs file):

    (ido-ubiquitous-disable-in foo)"

  nil
  :global t
  :group 'ido-ubiquitous)

;;;###autoload
(defcustom ido-ubiquitous-command-exceptions '()
  "List of commands that should not be affected by `ido-ubiquitous'.

Even when `ido-ubiquitous' mode is enabled, these commands will
continue to use `completing-read' instead of
`ido-completing-read'.

Only *interactive* commands should go here. To disable
ido-ubiquitous in non-interactive functions, customize
`ido-ubiquitous-function-exceptions'."
  :type '(repeat (symbol :tag "Command"))
  :group 'ido-ubiquitous)

(define-obsolete-variable-alias 'ido-ubiquitous-exceptions
  'ido-ubiquitous-command-exceptions "0.4")

(defadvice completing-read (around ido-ubiquitous activate)
  (if (or (not ido-mode)
          (not ido-ubiquitous)
          (memq this-command ido-ubiquitous-command-exceptions)
          ;; Avoid infinite recursion from ido calling completing-read
          (boundp 'ido-cur-item))
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      ;; Only use ido completion if there are actually any completions
      ;; to offer.
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt allcomp
                                     nil require-match initial-input hist def))
        ad-do-it))))

(defmacro ido-ubiquitous-disable-in (func)
  "Disable ido-ubiquitous in FUNC."
  (let ((docstring
         (format "Disable ido-ubiquitous in %s" func)))
    `(defadvice ,func (around disable-ido-ubiquitous activate)
       ,docstring
       (let (ido-ubiquitous) ad-do-it))))

(define-obsolete-function-alias
  'disable-ido-ubiquitous-in
  'ido-ubiquitous-disable-in
  "0.4")

(defmacro ido-ubiquitous-enable-in (func)
  "Re-enable ido-ubiquitous in FUNC.

  This reverses the effect of `ido-ubiquitous-disable-in'."
  ;; In my experience, simply using `ad-remove-advice' or
  ;; `ad-disable-advice' doesn't work correctly (in Emacs 23).
  ;; Instead, I've found that one must redefine the advice under the
  ;; same name ("disable-ido-ubiquitous") to simply call the original
  ;; function with no modifications. This has the same effect
  ;; (disables the advice), but is presumably less efficient.
  (let ((docstring
         (format "DO NOT disable ido-ubiquitous in %s" func)))
    `(defadvice ,func (around disable-ido-ubiquitous activate)
       ,docstring
       ad-do-it)))

(define-obsolete-function-alias
  'enable-ido-ubiquitous-in
  'ido-ubiquitous-enable-in
  "0.4")

;; Always disable ido-ubiquitous in `find-file' and similar functions,
;; because they are not supposed to use ido.
(defvar ido-ubiquitous-permanent-function-exceptions
  '(read-file-name)
  "Functions in which ido-ubiquitous should always be disabled.

If you want to disable ido in a specific function or command, do
not modify this variable. Instead, try `M-x customize-group
ido-ubiquitous..")

(dolist (func ido-ubiquitous-permanent-function-exceptions)
  (eval `(ido-ubiquitous-disable-in ,func)))

(defun ido-ubiquitous--set-difference (list1 list2)
  "Replacement for `set-difference' from `cl'."
  (apply #'nconc
         (mapcar (lambda (elt) (unless (memq elt list2) (list elt)))
                 list1)))

(defun ido-ubiquitous-set-function-exceptions (sym newval)
  (let* ((oldval (when (boundp sym) (eval sym))))
    ;; Filter out permanent fixtures
    (setq oldval (ido-ubiquitous--set-difference oldval ido-ubiquitous-permanent-function-exceptions))
    (setq newval (ido-ubiquitous--set-difference newval ido-ubiquitous-permanent-function-exceptions))
    ;; Re-enable ido-ubiquitous on all old functions, in case they
    ;; were removed from the list.
    (dolist (oldfun oldval)
      (eval `(ido-ubiquitous-enable-in ,oldfun)))
    ;; Set the new value
    (set-default sym newval)
    ;; Disable ido-ubiquitous on all new functions
    (dolist (newfun newval)
      (eval `(ido-ubiquitous-disable-in ,newfun)))))

;;;###autoload
(defcustom ido-ubiquitous-function-exceptions
  '(grep-read-files)
  "List of functions in which to disable ido-ubiquitous.

Certain functions, such as `read-file-name', always have
ido-ubiquitous disabled, and cannot be added here. (They are
effectively permanently part of this list already.)"
  :group 'ido-ubiquitous
  :type '(repeat :tag "Functions"
                 (symbol :tag "Function"))
  :set 'ido-ubiquitous-set-function-exceptions)

(defadvice ido-exit-minibuffer (around required-allow-empty-string activate)
  "Emulate a quirk of `completing-read'.

Apparently, `completing-read' used to request the default item by
returning an empty string when RET was pressed with an empty input.
This forces `ido-completing-read' to do the same (instead of returning
the first choice in the list).

This has no effect when ido is completing buffers or files."
  (if (and (eq ido-cur-item 'list)
           ido-require-match
           (string= ido-text ""))
      (ido-select-text)
    ad-do-it))

(provide 'ido-ubiquitous) ;;; ido-ubiquitous.el ends here
