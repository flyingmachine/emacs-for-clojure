;; -*- lexical-binding: t -*-

;;; ido-ubiquitous.el --- Use ido (nearly) everywhere.

;; Author: Ryan C. Thompson
;; URL: https://github.com/DarwinAwardWinner/ido-ubiquitous
;; Version: 20140526.1306
;; X-Original-Version: 2.13
;; Created: 2011-09-01
;; Keywords: convenience
;; EmacsWiki: InteractivelyDoThings
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; If you use the excellent `ido-mode' for efficient completion of
;; file names and buffers, you might wonder if you can get ido-style
;; completion everywhere else too. Well, that's what this package
;; does! ido-ubiquitous is here to enable ido-style completion for
;; (almost) every function that uses the standard completion function
;; `completing-read'.

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

(defconst ido-ubiquitous-version "2.13"
  "Currently running version of ido-ubiquitous.

Note that when you update ido-ubiquitous, this variable may not
be updated until you restart Emacs.")

(eval-when-compile
  (when (or (not (boundp 'completing-read-function))
            (< emacs-major-version 24))
    (error "Could not find required variable `completing-read-function'. Are you using Emacs version 24 or higher? If you have Emacs 23 or lower, please downgrade to ido-ubiquitous version 1.7.")))

(require 'ido)
(require 'advice)
(require 'cl)
;; Only exists in emacs 24.4 and up; we use a workaround for earlier
;; versions.
(require 'nadvice nil 'noerror)

;; Declare this ahead of time to quiet the compiler
(defvar ido-ubiquitous-fallback-completing-read-function)

;;; Internal utility functions

(defun ido-ubiquitous--as-string (sym-or-str)
  "Return name of symbol, return string as is."
  (if (symbolp sym-or-str)
      (symbol-name sym-or-str)
    sym-or-str))

(defun ido-ubiquitous--as-symbol (sym-or-str)
  "Return name of symbol, return string as is."
  (if (symbolp sym-or-str)
      sym-or-str
    (intern sym-or-str)))

;;; Custom widget definitions

;; We need to define some custom widget types for use in the override
;; variables.

(define-widget 'lazy-notag 'lazy
  "Like lazy widget, but does not display its tag, only its value."
  :format "%v")

;; Define matcher functions and widgets for match specifications
(defvar ido-ubiquitous-match-spec-widget-types nil
  "List of widget names for match specs.")
(defvar ido-ubiquitous-spec-matchers nil
  "Alist of functions for matching function specs against function names.")
(loop for (widget-name widget-tag key field-type matcher) in
         '((exact-match "Exact match" exact string string=)
           (prefix-match "Prefix match" prefix string string-prefix-p)
           (regexp-match "Regexp match" regexp regexp string-match-p))
         do (define-widget (ido-ubiquitous--as-symbol widget-name) 'lazy-notag widget-tag
              :menu-tag widget-tag
              :type `(list :tag ,widget-tag :format "%v"
                           (const :format ""
                                  :tag ,widget-tag
                                  ,key)
                           (,field-type :tag ,widget-tag)))
         do (add-to-list 'ido-ubiquitous-match-spec-widget-types
                         widget-name 'append)
         do (add-to-list 'ido-ubiquitous-spec-matchers
                         (cons key matcher) 'append))

(define-widget 'ido-ubiquitous-match-spec 'lazy-notag
  "Choice of exact, prefix, or regexp match."
  :type `(choice :tag "Match type"
                 ,@ido-ubiquitous-match-spec-widget-types))

(define-widget 'ido-ubiquitous-command-override-spec 'lazy-notag
  "Choice of override action plus match specification."
  :type '(cons :tag "Override rule"
               (choice :tag "For matching commands"
                       (const :menu-tag "Disable"
                              :tag "Disable ido-ubiquitous"
                              disable)
                       (const :menu-tag "Enable"
                              :tag "Enable ido-ubiquitous in normal default mode"
                              enable)
                       (const :menu-tag "Enable old-style default"
                              :tag "Enable ido-ubiquitous in old-style default mode"
                              enable-old))
               ido-ubiquitous-match-spec))

(define-widget 'ido-ubiquitous-function-override-spec 'lazy-notag
  "Choice of override action and function name. (Exact match only.)"
  :type '(list :tag "Override rule"
               (choice :tag "Do the following"
                       (const :menu-tag "Disable"
                              :tag "Disable ido-ubiquitous"
                              disable)
                       (const :menu-tag "Enable"
                              :tag "Enable ido-ubiquitous in normal default mode"
                              enable)
                       (const :menu-tag "Enable old-style default"
                              :tag "Enable ido-ubiquitous in old-style default mode"
                              enable-old))
               (const :format "" exact)
               (string :tag "For function")))

;;; Custom Declarations

(defgroup ido-ubiquitous nil
  "Use ido for (almost) all completion."
  :group 'ido)

;;;###autoload
(define-obsolete-variable-alias 'ido-ubiquitous
  'ido-ubiquitous-mode "0.8")
;;;###autoload
(define-obsolete-function-alias 'ido-ubiquitous
  'ido-ubiquitous-mode "0.8")

;;;###autoload
(define-minor-mode ido-ubiquitous-mode
  "Use `ido-completing-read' instead of `completing-read' almost everywhere.

  This mode has no effect unles `ido-mode' is also enabled.

  If this mode causes problems for a function, you can customize
  when ido completion is or is not used by customizing
  `ido-ubiquitous-command-overrides' or
  `ido-ubiquitous-function-overrides'."

  nil
  :global t
  :group 'ido-ubiquitous
  ;; Handle warning about ido disabled
  (when ido-ubiquitous-mode
    (ido-ubiquitous-warn-about-ido-disabled))
  ;; Ensure emacs 23 code disabled (in case user upgraded in this session)
  (ignore-errors
    (ad-disable-advice 'completing-read 'around 'ido-ubiquitous-legacy)
    (ad-activate 'completing-read))
  ;; Actually enable/disable the mode
  (setq completing-read-function
        (if ido-ubiquitous-mode
            'completing-read-ido
          (or ido-ubiquitous-fallback-completing-read-function
              'completing-read-default))))

(defcustom ido-ubiquitous-max-items 30000
  "Max collection size to use ido-ubiquitous on.

If `ido-ubiquitous-mode' is active and `completing-read' is
called on a COLLECTION with greater than this number of items in
it, the fallback completion method will be used instead. To
disable fallback based on collection size, set this to nil."
  :type '(choice (const :tag "No limit" nil)
                 (integer
                  :tag "Limit" :value 5000
                  :validate
                  (lambda (widget)
                    (let ((v (widget-value widget)))
                      (if (and (integerp v)
                               (> v 0))
                          nil
                        (widget-put widget :error "This field should contain a positive integer")
                        widget)))))
  :group 'ido-ubiquitous)

(defcustom ido-ubiquitous-fallback-completing-read-function
  ;; Initialize to the current value of `completing-read-function',
  ;; unless that is already set to the ido completer, in which case
  ;; use `completing-read-default'.
  (if (eq completing-read-function 'completing-read-ido)
      'completing-read-default
    completing-read-function)
  "Alternate completing-read function to use when ido is not wanted.

This will be used for functions that are incompatibile with ido
or if ido cannot handle the completion arguments.

If you turn off ido-ubiquitous mode, `completing-read-function'
will be set back to this."
  :type '(choice (const :tag "Standard emacs completion"
                        completing-read-default)
                 (function :tag "Other function"))
  :group 'ido-ubiquitous)

(define-obsolete-variable-alias
  'ido-ubiquitous-enable-compatibility-globally
  'ido-ubiquitous-enable-old-style-default
  "2.0")

(defcustom ido-ubiquitous-enable-old-style-default t
  "Allow ido to emulate a quirk of `completing-read'.

From the `completing-read' docstring:

> If the input is null, `completing-read' returns DEF, or the
> first element of the list of default values, or an empty string
> if DEF is nil, regardless of the value of REQUIRE-MATCH.

If this variable is non-nil, then ido-ubiquitous will attempt to
emulate this behavior. Specifically, if RET is pressed
immediately upon entering completion, an empty string will be
returned instead of the first element in the list. This behavior
is only enabled when ido is being used as a substitute for
`completing-read', and not when it is used directly.

This odd behavior is required for compatibility with an old-style
usage pattern whereby the default was requested by returning an
empty string. In this mode, the caller receives the empty string
and handles the default case manually, while `completing-read'
never has any knowledge of the default. This is a problem for
ido, which always returns the first element in the list when the
input is empty. Without knowledge of the default, it cannot
ensure that the default is first on the list, so returning the
first item is not the correct behavior. Instead, it must return
an empty string like `completing-read'.

You can termporarily invert this behavior by prefixing \"RET\"
with \"C-u\".

If you want to enable old-style default selection selectively for
specific commands or functions, set appropriate overrides in
`ido-ubiquitous-command-overrides' or
`ido-ubiquitous-function-overrides'."
  :type 'boolean
  :group 'ido-ubiquitous)

(defconst ido-ubiquitous-default-command-overrides
  '(;; If you want ido for M-x, install smex
    (disable exact "execute-extended-command")
    ;; https://github.com/technomancy/ido-ubiquitous/issues/13#issuecomment-8033522
    (enable prefix "wl-")
    ;; https://github.com/technomancy/ido-ubiquitous/issues/7
    (enable-old prefix "Info-")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/4
    (enable exact "webjump")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/28
    (enable regexp "\\`\\(find\\|load\\|locate\\)-library\\'")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/37
    ;; Org and Magit already support ido natively
    (disable prefix "org-")
    (disable prefix "magit-")
    ;; https://github.com/bbatsov/prelude/issues/488
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/44
    ;; tmm implements its own non-standard completion mechanics
    (disable prefix "tmm-")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/47
    ;; theme functions don't need old-style compatibility
    (enable regexp "\\`\\(load\\|enable\\|disable\\|describe\\|custom-theme-visit-theme\\)-theme\\'")
)
  "Default value of `ido-ubiquitous-command-overrides'.

You can restore these using the command `ido-ubiquitous-restore-default-overrides'.")

(defconst ido-ubiquitous-default-function-overrides
  '((disable exact "read-file-name")
    (disable exact "read-file-name-internal")
    (disable exact "read-buffer")
    (disable exact "gnus-emacs-completing-read")
    (disable exact "gnus-iswitchb-completing-read")
    (disable exact "grep-read-files")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/36
    (enable exact "bookmark-completing-read")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/4
    (enable-old exact "webjump-read-choice")
    (enable-old exact "webjump-read-url-choice")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/9
    (disable exact "isearchp-read-unicode-char")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/37
    (disable exact "org-completing-read")
    (disable exact "org-completing-read-no-i")
    (disable exact "org-iswitchb-completing-read")
    (disable exact "org-icompleting-read")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/38
    (enable exact "read-char-by-name")
    ;; https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/39
    (disable exact "Info-read-node-name")
    ;; https://github.com/purcell/emacs.d/issues/182#issuecomment-44212927
    (disable exact "tmm-menubar"))
  "Default value of `ido-ubiquitous-function-overrides'.

You can restore these using the command `ido-ubiquitous-restore-default-overrides'.")

(defcustom ido-ubiquitous-command-overrides ido-ubiquitous-default-command-overrides
  "List of command override specifications for ido-ubiquitous

Each override specification describes how ido-ubiquitous should
behave one or many commands. A specification has the
form `(BEHAVIOR MATCH-TYPE MATCH-TEXT)'. BEHAVIOR is one of the
following:

  * `disable': ido-ubiquitous should not be used at all for the
    specified commands;
  * `enable': ido-ubiquitous may be used with the specified
    commands, without emulating the old-style default selection
    of `completing-read';
  * `enable-old': ido-ubiquitous may be used with the specified
    commands, and should emulate the old-style default selection
    of `completing-read'.

MATCH-TYPE affects how MATCH-TEXT is interpreted, as follows:

  * `exact': the specification only affects the one command
    whose name is MATCH-TEXT;
  * `prefix': the specification affects any command whose name
    starts with MATCH-TEXT (This is useful for specifying a
    certain behavior for an entire package);
  * `regexp': the specification affects any command whose name
    matches MATCH-TEXT (with MATCH-TEXT being interpreted as a
    regular expression)

MATCH-TEXT should be a string.

Since this variable's has a somewhat complex structure, it is
recommended that you set this variable through Customize.

Note that this variable only affects *commands*, which are
functions marked as interactive. See
`ido-ubiquitous-function-overrides' for how to modify the
behavior of ido-ubiquitous for arbitrary functions.

If you need to add a new specification to this list, please also
file a bug report at https://github.com/DarwinAwardWinner/ido-ubiquitous/issues"
  :type '(repeat ido-ubiquitous-command-override-spec)
  :group 'ido-ubiquitous)

(defmacro ido-ubiquitous-with-override (override &rest body)
  "Eval BODY with specicified OVERRIDE in place.

The OVERRIDE argument is evaluated normally, so if it is a
literal symbol, it must be quoted.

See `ido-ubiquitous-command-overrides' for valid override types."
  ;; Eval override
  `(let ((ido-ubiquitous-next-override ,override))
     ,@body))
(put 'ido-ubiquitous-with-override 'lisp-indent-function
     (get 'prog1 'lisp-indent-function))

(defun ido-ubiquitous-apply-function-override (func override)
  "Set the override property on FUNC to OVERRIDE and set up advice to apply the override."
  (setq func (ido-ubiquitous--as-symbol func)
        override (ido-ubiquitous--as-symbol override))
  (put func 'ido-ubiquitous-override override)
  (when override
    (let ((docstring
           (format "Override ido-ubiquitous behavior in %s if its `ido-ubiquitous-override' property is non-nil." func)))
      (eval
       `(defadvice ,func (around ido-ubiquitous-override activate)
          ,docstring
          (ido-ubiquitous-with-override
              (get ',func 'ido-ubiquitous-override)
            ad-do-it))))))

(defun ido-ubiquitous-set-function-overrides (sym newval)
  "Custom setter function for `ido-ubiquitous-function-overrides'.

In addition to setting the variable, this also sets up advice on
each function to apply the appropriate override."
  ;; Unset all previous overrides
  (when (boundp sym)
    (let ((oldval (eval sym)))
      (loop for (_action _match-type func) in oldval
               do (ido-ubiquitous-apply-function-override func nil))))
  ;; Ensure that function names are strings, not symbols
  (setq newval
        (loop for (action match-type func) in newval
                 collect (list action match-type
                               (ido-ubiquitous--as-string func))))
  (set-default sym newval)
  ;; set new overrides
  (loop for (action _match-type func) in (eval sym)
           do (ido-ubiquitous-apply-function-override func action)))

(defcustom ido-ubiquitous-function-overrides ido-ubiquitous-default-function-overrides
  "List of function override specifications for ido-ubiquitous

Function override specifications have a similar structure to
command override specifications (see
`ido-ubiquitous-command-overrides'). A function override
specification has the form `(BEHAVIOR MATCH-TYPE MATCH-TEXT)'.
However, `MATCH-TYPE' may ONLY be `exact'; No other match type is
supported.

If you need to add a new specification to this list, please also file a
bug report at https://github.com/DarwinAwardWinner/ido-ubiquitous/issues

Setting this variable directly has no effect. You must set it
through Customize."
  :type '(repeat ido-ubiquitous-function-override-spec)
  :set 'ido-ubiquitous-set-function-overrides
  :group 'ido-ubiquitous)

(defcustom ido-ubiquitous-allow-on-functional-collection nil
  "Allow ido completion when COLLECTION is a function.

The `completing-read' function allows its COLLECTION argument to
be a function instead of a list of choices. Some such functions
simply return a list of completions and are suitable for use with
ido, but others implement more complex behavior and will result
in incorrect behavior if used with ido. Since there is no way to
tell the difference, this preference defaults to nil, which means
that ido-ubiquitous will not work when COLLECTION is a function
unless there is a specific override in effect. To disable this
safeguard and guarantee breakage on some functions, you may set
this to non-nil, but this is not recommended."
  :type 'boolean
  :group 'ido-ubiquitous)

;;; ido-ubiquitous core

(defvar ido-ubiquitous-next-call-replaces-completing-read nil
  "If t, then the next call to `ido-completing-read' is by ido-ubiquitous.")
(defvar ido-ubiquitous-this-call-replaces-completing-read nil
  "If t, then the current call to `ido-completing-read' is by ido-ubiquitous.")
(defvar ido-ubiquitous-next-override nil
  "This holds the override to be applied on the next call to `completing-read'.")
(defvar ido-ubiquitous-active-override nil
  "This holds the override being applied to the current call to `completing-read'.")

(defun ido-ubiquitous-completing-read (&rest args)
  "Wrapper for `ido-completing-read' that enables ido-ubiquitous features."
  (let ((ido-ubiquitous-next-call-replaces-completing-read t))
    (apply 'ido-completing-read args)))

(defadvice ido-completing-read (around detect-replacing-cr activate)
  "Enable workarounds if this call was done through ido-ubiquitous.

This advice implements the logic required for
`ido-completing-read' to handle a number of special cases that
`completing-read' can handle. It only has an effect if
`ido-completing-read' is called through
`ido-ubiquitous-completing-read', so other packages that use
`ido-completing-read', such as `smex', will not be affected."
  (let* ((ido-ubiquitous-this-call-replaces-completing-read ido-ubiquitous-next-call-replaces-completing-read)
         (ido-ubiquitous-next-call-replaces-completing-read nil)
         (error-during-setup nil))
    (when ido-ubiquitous-this-call-replaces-completing-read
      (condition-case nil
          (progn
            ;; ido doesn't natively handle DEF being a list. If DEF is
            ;; a list, prepend it to CHOICES and set DEF to just the
            ;; car of the default list.
            (when (and def (listp def))
              (setq choices
                    (append def
                            (nreverse (cl-set-difference choices def)))
                    def (car def)))
            ;; Work around a bug in ido when both INITIAL-INPUT and
            ;; DEF are provided More info:
            ;; https://github.com/technomancy/ido-ubiquitous/issues/18
            (let ((initial (cond ((null initial-input) "")
                                 ((stringp initial-input) initial-input)
                                 ((consp initial-input) (car initial-input))
                                 (t initial-input))))
              (when (and def initial
                         (stringp initial)
                         (not (string= initial "")))
                ;; Both default and initial input were provided. So
                ;; keep the initial input and preprocess the choices
                ;; list to put the default at the head, then proceed
                ;; with default = nil.
                (setq choices (cons def (remove def choices))
                      def nil))))
        (error
         (progn
           (warn "ido-ubiquitous: failed during setup. Falling back to standard completion")
           (setq error-during-setup t)))))
    ;; For ido-ubiquitous, only attempt ido completion if setup
    ;; completed without error
    (if (not error-during-setup)
        ad-do-it
      (setq ad-return-value
            (funcall
             ido-ubiquitous-fallback-completing-read-function
             prompt choices predicate require-match initial-input
             hist def inherit-input-method)))))

(defun completing-read-ido (prompt collection &optional predicate
                                   require-match initial-input
                                   hist def inherit-input-method)
  "ido-based method for reading from the minibuffer with completion.

See `completing-read' for the meaning of the arguments.

This function is a wrapper for `ido-completing-read' designed to
be used as the value of `completing-read-function'. Importantly,
it detects edge cases that ido cannot handle and uses normal
completion for them."
  (let* (;; Set the active override and clear the "next" one so it
         ;; doesn't apply to nested calls.
         (ido-ubiquitous-active-override ido-ubiquitous-next-override)
         (ido-ubiquitous-next-override nil)
         ;; Check for conditions that ido can't or shouldn't handle
         (ido-allowed
          (and ido-mode
               ido-ubiquitous-mode
               ;; Check for disable override
	       (not (eq ido-ubiquitous-active-override 'disable))
               ;; Can't handle this arg
               (not inherit-input-method)
               ;; Can't handle this being set
               (not (bound-and-true-p completion-extra-properties))))
         ;; Check if ido can handle this collection. If collection is
         ;; a function, require an override to be ok. Also,
         ;; collection-ok should never be true when ido-allowed is
         ;; false.
         (collection-ok
          (and ido-allowed
               (or ido-ubiquitous-allow-on-functional-collection
                   (not (functionp collection))
                   (memq ido-ubiquitous-active-override '(enable enable-old)))))
         ;; Pre-expand list of possible completions, but only if we
         ;; have a chance of using ido. This is executed after the
         ;; ido-allowed check to avoid unnecessary work if ido isn't
         ;; going to used.
         (_ignore ;; (Return value doesn't matter).
          (when (and ido-allowed collection-ok)
            (setq collection (all-completions "" collection predicate)
                  ;; Don't need this any more
                  predicate nil)))
         (collection-ok
          ;; Don't use ido if the collection is empty or too large.
          (and collection-ok
               collection
               (or (null ido-ubiquitous-max-items)
                   (<= (length collection) ido-ubiquitous-max-items))))
         ;; Final check for everything
         (ido-allowed (and ido-allowed collection-ok))
         (comp-read-fun
          (if ido-allowed
              'ido-ubiquitous-completing-read
            ido-ubiquitous-fallback-completing-read-function)))
    (funcall comp-read-fun
	     prompt collection predicate
	     require-match initial-input
	     hist def inherit-input-method)))

;;; Old-style default support

(defvar ido-ubiquitous-initial-item nil
  "The first item selected when ido starts.

This is initialized to the first item in the list of completions
when ido starts, and is cleared when any character is entered
into the prompt or the list is cycled. If it is non-nil and still
equal to the first item in the completion list when ido exits,
then if `ido-ubiquitous-enable-old-style-default' is
non-nil, ido returns an empty string instead of the first item on
the list.")

(defadvice ido-read-internal (before clear-initial-item activate)
  (setq ido-ubiquitous-initial-item nil))

(defadvice ido-make-choice-list (after set-initial-item activate)
  (when (and ad-return-value (listp ad-return-value))
    (setq ido-ubiquitous-initial-item (car ad-return-value))))

(defadvice ido-next-match (after clear-initial-item activate)
  (setq ido-ubiquitous-initial-item nil))

(defadvice ido-prev-match (after clear-initial-item activate)
  (setq ido-ubiquitous-initial-item nil))

(defadvice ido-exit-minibuffer (around compatibility activate)
  "Emulate a quirk of `completing-read'.

> If the input is null, `completing-read' returns DEF, or the
> first element of the list of default values, or an empty string
> if DEF is nil, regardless of the value of REQUIRE-MATCH.

See `ido-ubiquitous-enable-old-style-default', which
controls whether this advice has any effect."
  (condition-case nil
      (let* ((enable-oldstyle
              (and
               ;; Completing a list, not a buffer or file
               (eq ido-cur-item 'list)
               ;; Only enable if we are replacing `completing-read'
               ido-ubiquitous-this-call-replaces-completing-read
               ;; Default is nil
               (null ido-default-item)
               ;; Input is empty
               (string= ido-text "")
               ;; Old-style default enabled
               (if ido-ubiquitous-active-override
                   (eq ido-ubiquitous-active-override 'enable-old)
                 ido-ubiquitous-enable-old-style-default)
               ;; First item on the list hasn't changed
               (string= (car ido-cur-list)
                        ido-ubiquitous-initial-item)))
             ;; Prefix inverts oldstyle behavior
             (should-invert current-prefix-arg)
             (actually-enable-oldstyle
              (if should-invert (not enable-oldstyle) enable-oldstyle)))
        (if actually-enable-oldstyle
            (ido-select-text)
          ad-do-it))
    (error ad-do-it))
  (setq ido-ubiquitous-initial-item nil))

;;; Overrides

(defun ido-ubiquitous-restore-default-overrides (&optional save)
  "Re-add the default overrides for ido-ubiquitous.

This will ensure that the default overrides are all present and
at the head of the list in `ido-ubiquitous-command-overrides' and
`ido-ubiquitous-function-overrides'. User-added overrides will
not be removed, but they may be masked if one of the default
overrides affects the same functions.

With a prefix arg, also save the above variables' new values for
future sessions."
  (interactive "P")
  (let ((setter (if save
                    'customize-save-variable
                  'customize-set-variable)))
    (loop for (var def) in '((ido-ubiquitous-command-overrides
                                 ido-ubiquitous-default-command-overrides)
                                (ido-ubiquitous-function-overrides
                                 ido-ubiquitous-default-function-overrides))
             do (let* ((curval (eval var))
                       (defval (eval def))
                       (newval (delete-dups (append defval curval))))
                  (funcall setter var newval)))
    (message (if save
                 "ido-ubiquitous: Restored default command and function overrides and saved for future sessions."
               "ido-ubiquitous: Restored default command and function overrides for current session only."))))

(defun ido-ubiquitous-spec-match (spec symbol)
  "Returns t if SPEC matches SYMBOL (which should be a function name).

See `ido-ubiquitous-command-overrides'."
  (when (and symbol (symbolp symbol))
    (destructuring-bind (type text) spec
      (let ((matcher (cdr (assoc type ido-ubiquitous-spec-matchers)))
            (text (ido-ubiquitous--as-string text))
            (symname (ido-ubiquitous--as-string symbol)))
        (when (null matcher)
          (error "ido-ubiquitous: Unknown match spec type \"%s\". See `ido-ubiquitous-spec-matchers' for valid types." type))
        (funcall matcher text symname)))))

(defun ido-ubiquitous-get-command-override (cmd)
  "Return the override associated with the command CMD.

If there is no override set for CMD in
`ido-ubiquitous-command-overrides', return nil."
  (when (and cmd (symbolp cmd))
    (loop for (action . spec) in ido-ubiquitous-command-overrides
          when (memq action '(disable enable enable-old nil))
          when (ido-ubiquitous-spec-match spec cmd)
          return action
          finally return nil)))

;;; Workaround for https://github.com/DarwinAwardWinner/ido-ubiquitous/issues/24

;; When `call-interactively' is advised, `called-interactively-p'
;; always returns nil. So we redefine it (and `interactive-p') to test
;; the correct condition.

(defsubst ido-ubiquitous--looks-like-advised-orig (func)
  "Returns t if FUNC is a symbol starting with \"ad-Orig-\".

Such symbols are used to store the original definitions of
functions that have been advised by `defadvice' or similar."
  (and (symbolp func)
       (string-prefix-p "ad-Orig-" (symbol-name func))))

(defsubst ido-ubiquitous--looks-like-call-interactively (func)
  "Returns t if FUNC looks like the function `call-interactively'.

FUNC \"looks like\" `call-interactively' if it is the literal
symbol `call-interactively', or the value of `(symbol-function
'call-interactively)', or a symbol whose `symbol-function' is the
same as that of `call-interactively'.

This function is used to determine whether a given function was
\"called by\" `call-interactively' and therefore was called
interactively."
  (when func
    (eq (symbol-function 'call-interactively)
        (if (symbolp func)
            (symbol-function func)
          func))))

(defun ido-ubiquitous--backtrace-from (fun)
  "Return all backtrace frames, starting with the one for FUN.

FUN may be a list of functions, in which case the first one found
on the stack will be used."
  (let ((stack
         (loop for i upfrom 0
               for frame = (backtrace-frame i)
               while frame
               collect frame))
        (funcs (if (functionp fun)
                   (list fun)
                 fun)))
    (while (and stack
                (not (memq (cadar stack) funcs)))
      (setq stack (cdr stack)))
    stack))

(defun ido-ubiquitous--clean-advice-from-backtrace (stack)
  "Takes a stack trace and cleans all evidence of advice.

Specifically, for each call to a function starting with
\"ad-Orig-\", that call and all prior calls up to but not
including the advised function's original name are deleted from
the stack."
  (let ((skipping-until nil))
    (loop for frame in stack
             for func = (cadr frame)
             ;; Check if we found the frame we we're skipping to
             if (and skipping-until
                     (eq func skipping-until))
             do (setq skipping-until nil)
             ;; If we're looking at an the original form of an advised
             ;; function, skip until the real name of that function.
             if (and (not skipping-until)
                     (ido-ubiquitous--looks-like-advised-orig func))
             do (setq skipping-until
                      (intern
                       (substring (symbol-name func)
                                  (eval-when-compile (length "ad-Orig-")))))
             unless skipping-until collect frame)))

(defsubst ido-ubiquitous--interactive-internal ()
  "Eqivalent of the INTERACTIVE macro in the Emacs C source.

This is an internal function that should never be called
directly.

See the C source for the logic behind this function."
  (and (not executing-kbd-macro)
       (not noninteractive)))

(defun ido-ubiquitous--interactive-p-internal ()
  "Equivalent of C function \"interactive_p\".

This is an internal function that should never be called
directly.

See the C source for the logic behind this function."
  (let ((stack
         ;; We clean advice from the backtrace. This ensures that we
         ;; get the right answer even if `call-interactively' has been
         ;; advised.
         (ido-ubiquitous--clean-advice-from-backtrace
          (cdr
           (ido-ubiquitous--backtrace-from
            '(called-interactively-p interactive-p))))))
    ;; See comments in the C function for the logic here.
    (while (and stack
                (or (eq (cadar stack) 'bytecode)
                    (null (caar stack))))
      (setq stack (cdr stack)))
    ;; Top of stack is now the function that we want to know
    ;; about. Pop it, then check if the next function is
    ;; `call-interactively', using a more permissive test than the default.
    (ido-ubiquitous--looks-like-call-interactively (cadadr stack))))

(defadvice call-interactively (around ido-ubiquitous activate)
  "Implements the behavior specified in `ido-ubiquitous-command-overrides'."
  (ido-ubiquitous-with-override
      (ido-ubiquitous-get-command-override (ad-get-arg 0))
    ad-do-it))

;; Work around `called-interactively-p' in Emacs 24.3 and earlier,
;; which always returns nil when `call-interactively' is advised.
(when (not (and (featurep 'nadvice)
                (boundp 'called-interactively-p-functions)))

  (defadvice interactive-p (around ido-ubiquitous activate)
    "Return the correct result when `call-interactively' is advised.

This advice completely overrides the original definition."
    (condition-case nil
        (setq ad-return-value
              (and (ido-ubiquitous--interactive-internal)
                   (ido-ubiquitous--interactive-p-internal)))
      ;; In case of error in the advice, fall back to the default
      ;; implementation
      (error ad-do-it)))

  (defadvice called-interactively-p (around ido-ubiquitous activate)
    "Return the correct result when `call-interactively' is advised.

This advice completely overrides the original definition."
    (condition-case nil
        (setq ad-return-value
              (and (or (ido-ubiquitous--interactive-internal)
                       (not (eq kind 'interactive)))
                   (ido-ubiquitous--interactive-p-internal)))
      ;; In case of error in the advice, fall back to the default
      ;; implementation
      (error ad-do-it))))

;;; Other

(defun ido-ubiquitous-warn-about-ido-disabled ()
  "Warn if ido-ubiquitous is enabled without ido.

Don't warn if emacs is still initializing, since ido-ubiquitous
could be enabled first during init."
  (when (and ido-ubiquitous-mode
             after-init-time
             (not (bound-and-true-p ido-mode)))
    (warn "ido-ubiquitous-mode enabled without ido mode. ido-ubiquitous requires ido mode to be enabled.")))

(defun ido-ubiquitous-initialize ()
  "Do initial setup for ido-ubiquitous.

This only needs to be called once when the file is first loaded."
  ;; Clean up old versions of ido-ubiquitous that defined advice on
  ;; `completing-read' instead of modifying
  ;; `completing-read-function'.
  (when (ad-find-advice 'completing-read 'around 'ido-ubiquitous)
    (ad-remove-advice 'completing-read 'around 'ido-ubiquitous)
    (ad-activate 'completing-read))
  ;; Make sure the mode is turned on/off as specified by the value of
  ;; the mode variable
  (ido-ubiquitous-mode (if ido-ubiquitous-mode 1 0)))
(ido-ubiquitous-initialize)

(provide 'ido-ubiquitous)

;;; ido-ubiquitous.el ends here
