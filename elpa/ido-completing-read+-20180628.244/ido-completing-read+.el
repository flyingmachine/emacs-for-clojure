;;; ido-completing-read+.el --- A completing-read-function using ido  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2017 Ryan C. Thompson

;; Filename: ido-completing-read+.el
;; Author: Ryan Thompson
;; Created: Sat Apr  4 13:41:20 2015 (-0700)
;; Version: 4.11
;; Package-Version: 20180628.244
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (s "0.1") (memoize "1.1"))
;; URL: https://github.com/DarwinAwardWinner/ido-completing-read-plus
;; Keywords: ido, completion, convenience

;; This file is NOT part of GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; If you use the excellent `ido-mode' for efficient completion of
;; file names and buffers, you might wonder if you can get ido-style
;; completion everywhere else too. Well, that's what this package
;; does! ido-ubiquitous is here to enable ido-style completion for
;; (almost) every function that uses the standard completion function
;; `completing-read'.

;; This package implements the `ido-completing-read+' function, which
;; is a wrapper for `ido-completing-read'. Importantly, it detects
;; edge cases that ordinary ido cannot handle and either adjusts them
;; so ido *can* handle them, or else simply falls back to Emacs'
;; standard completion instead. Hence, you can safely set
;; `completing-read-function' to `ido-completing-read+' without
;; worrying about breaking completion features that are incompatible
;; with ido.

;; To use this package, call `ido-ubiquitous-mode' to enable the mode,
;; or use `M-x customize-variable ido-ubiquitous-mode' it to enable it
;; permanently. Once the mode is enabled, most functions that use
;; `completing-read' will now have ido completion. If you decide in
;; the middle of a command that you would rather not use ido, just use
;; C-f or C-b at the end/beginning of the input to fall back to
;; non-ido completion (this is the same shortcut as when using ido for
;; buffers or files).

;; Note that `completing-read-default' is a very general function with
;; many complex behaviors that ido cannot emulate. This package
;; attempts to detect some of these cases and avoid using ido when it
;; sees them. So some functions will not have ido completion even when
;; this mode is enabled. Some other functions have ido disabled in
;; them because their packages already provide support for ido via
;; other means (for example, magit). See `M-x describe-variable
;; ido-cr+-function-blacklist' for more information.

;; ido-completing-read+ version 4.0 is a major update. The formerly
;; separate package ido-ubiquitous has been subsumed into
;; ido-completing-read+, so ido-ubiquitous 4.0 is just a wrapper that
;; loads ido-completing-read+ and displays a warning about being
;; obsolete. If you have previously customized ido-ubiquitous, be sure
;; to check out `M-x customize-group ido-completing-read-plus' after
;; updating to 4.0 and make sure the new settings are to your liking.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defconst ido-completing-read+-version "4.11"
  "Currently running version of ido-completing-read+.

Note that when you update ido-completing-read+, this variable may
not be updated until you restart Emacs.")

(require 'ido)
(require 'minibuf-eldef)
(require 'cl-lib)
(require 'cus-edit)
(require 's)

;; Optional dependency, only needed for optimization
(require 'memoize nil t)

;; Silence some byte-compiler warnings
(eval-when-compile
  (require 'minibuf-eldef)
  (require 'flx-ido nil t))

;;; Debug messages

(define-minor-mode ido-cr+-debug-mode
  "If non-nil, ido-cr+ will print debug info.

Debug info is printed to the *Messages* buffer."
  nil
  :global t
  :group 'ido-completing-read-plus)

(defsubst ido-cr+--debug-message (format-string &rest args)
  (when ido-cr+-debug-mode
    (apply #'message (concat "ido-completing-read+: " format-string) args)))

;;; Ido variables

;; For unknown reasons, these variables need to be re-declared here to
;; silence byte-compiler warnings, despite already being declared in
;; ido.el.

(defmacro define-ido-internal-var (symbol &optional initvalue docstring)
  "Declare and initialize an ido internal variable.

This is used to suppress byte-compilation warnings about
reference to free variables when ido-cr+ attempts to access
internal ido variables with no initial value set. Such variables
are originally declared like `(defvar VARNAME)'.

This is a wrapper for `defvar' that supplies a default for the
INITVALUE and DOCSTRING arguments."
  `(defvar ,symbol ,initvalue
     ,(or docstring
          "Internal ido variable.

This variable was originally declared in `ido.el' without an
initial value or docstring. The documentation you're reading
comes from re-declaring it in `ido-completing-read+.el' in order
to suppress some byte-compilation warnings. Setting another
package's variable is not safe in general, but in this case it
should be, because ido always let-binds this variable before
using it, so the initial value shouldn't matter.")))

(define-ido-internal-var ido-context-switch-command)
(define-ido-internal-var ido-cur-list)
(define-ido-internal-var ido-cur-item)
(define-ido-internal-var ido-require-match)
(define-ido-internal-var ido-process-ignore-lists)

;; Vars and functions from flx-ido package
(defvar flx-ido-mode)
(declare-function flx-ido-reset "ext:flx-ido.el")

;;;###autoload
(defvar ido-cr+-minibuffer-depth -1
  "Minibuffer depth of the most recent ido-cr+ activation.

If this equals the current minibuffer depth, then the minibuffer
is currently being used by ido-cr+, and ido-cr+ feature will be
active. Otherwise, something else is using the minibuffer and
ido-cr+ features will be deactivated to avoid interfering with
the other command.

This is set to -1 by default, since `(minibuffer-depth)' should
never return this value.")

(defvar ido-cr+-assume-static-collection nil
  "If non-nil, ido-cr+ will assume that the collection is static.

This is used to avoid unnecessary work in the case where the
collection is a function, since a function collection could
potentially change the set of completion candidates
dynamically.")

(defvar ido-cr+-current-command nil
  "Command most recently invoked by `call-interactively'.

This is necessary because `command-execute' and
`call-interactively' do not set `this-command'. Instead, the C
code that calls `command-execute' sets it beforehand, so using
either of those functions directly won't set `this-command'.")

(defvar ido-cr+-dynamic-collection nil
  "Stores the collection argument if it is a function.

This allows ido-cr+ to update the set of completion candidates
dynamically.")

(defvar ido-cr+-last-dynamic-update-text nil
  "The value of `ido-text' last time a dynamic update occurred.")

(defvar ido-cr+-dynamic-update-idle-time 0.25
  "Time to wait before updating dynamic completion list.")

(defvar ido-cr+-dynamic-update-timer nil
  "Idle timer for updating dynamic completion list.")

(defvar ido-cr+-exhibit-pending nil
  "This is non-nil after calling `ido-tidy' until the next call to `ido-exhibit'.

Typically this is non-nil while any command is running and nil at all
other times, since those two functions are in `pre-command-hook'
and `post-command-hook' respectively. In particular, this will
generally be nil while running an idle timer.")

(make-obsolete-variable
 'ido-cr+-no-default-action
 " This variable no longer has any effect. Customize `ido-cr+-nil-def-alternate-behavior-list' instead."
 "4.2")

(defvar ido-cr+-orig-completing-read-args nil
  "Original arguments passed to `ido-completing-read+'.

These are used for falling back to `completing-read-default'.")

(defvar ido-cr+-all-completions-memoized 'all-completions
  "Memoized version of `all-completions'.

During completion with dynamic collection, this variable is set
to a memoized copy of `all-completions'.")

(defvar ido-cr+-all-prefix-completions-memoized 'ido-cr+-all-prefix-completions
  "Memoized version of `ido-cr+-all-prefix-completions'.

During completion with dynamic collection, this variable is set
to a memoized copy of `ido-cr+-all-prefix-completions'.")

(defvar ido-cr+-active-restrictions nil
  "List of restrictions in place from `ido-restrict-to-matches'.

Each element is a cons cell of (REMOVEP . TEXT), where REMOVEP is
the prefix argument to `ido-restrict-to-matches' and TEXT is the
pattern used to restrict.")

(defgroup ido-completing-read-plus nil
  "Extra features and compatibility for `ido-completing-read'."
  :group 'ido)

(defcustom ido-cr+-fallback-function
  ;; Initialize to the current value of `completing-read-function',
  ;; unless that is already set to the ido completer, in which case
  ;; use `completing-read-default'.
  (if (memq completing-read-function
            '(ido-completing-read+
              ido-completing-read
              ;; Current ido-ubiquitous function
              completing-read-ido-ubiquitous
              ;; Old ido-ubiquitous functions that shouldn't be used
              completing-read-ido
              ido-ubiquitous-completing-read))
      'completing-read-default
    completing-read-function)
  "Alternate completing-read function to use when ido is not wanted.

This will be used for functions that are incompatible with ido
or if ido cannot handle the completion arguments. It will also be
used when the user requests non-ido completion manually via C-f
or C-b."
  :type '(choice (const :tag "Standard emacs completion"
                        completing-read-default)
                 (function :tag "Other function"))
  :group 'ido-completing-read-plus)

(defcustom ido-cr+-max-items 30000
  "Max collection size to use ido-cr+ on.

If `ido-completing-read+' is called on a collection larger than
this, the fallback completion method will be used instead. To
disable fallback based on collection size, set this to nil."
  :type '(choice (const :tag "No limit" nil)
                 (integer
                  :tag "Limit" :value 30000
                  :validate
                  (lambda (widget)
                    (let ((v (widget-value widget)))
                      (if (and (integerp v)
                               (> v 0))
                          nil
                        (widget-put widget :error "This field should contain a positive integer")
                        widget)))))
  :group 'ido-completing-read-plus)

(defcustom ido-cr+-function-blacklist
  '(read-file-name-internal
    read-buffer
    ;; https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues/60
    todo-add-category
    ;; Gnus already supports ido on its own
    gnus-emacs-completing-read
    gnus-iswitchb-completing-read
    grep-read-files
    ;; Magit already supports ido on its own
    magit-builtin-completing-read
    ;; ESS already supports ido on its own
    ess-completing-read
    ;; https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues/39
    Info-read-node-name
    ;; https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues/44
    tmm-prompt
    ;; https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues/156
    org-tags-completion-function)
  "Functions & commands for which ido-cr+ should be disabled.

Each entry can be either a symbol or a string. A symbol means to
fall back specifically for the named function. A regular
expression means to fall back for any function whose name matches
that regular expression. When ido-cr+ is called through
`completing-read', if any function in the call stack of the
current command matches any of the blacklist entries, ido-cr+
will be disabled for that command. Additionally, if the
collection in the call to `completing-read' matches any of the
blacklist entries, ido-cr+ will be disabled.

Note that using specific function names is generally preferable
to regular expressions, because the associated function
definitions will be compared directly, so if the same function is
called by another name, it should still trigger the fallback. For
regular expressions, only name-based matching is possible."
  :group 'ido-completing-read-plus
  :type '(repeat (choice (symbol :tag "Function or command name")
                         (string :tag "Regexp"))))

(defcustom ido-cr+-function-whitelist
  nil
  "Functions & commands for which ido-cr+ should be enabled.

If this variable is nil, the whitelist will not be used, and
ido-cr+ will be allowed in all functions/commands not listed in
`ido-cr+-function-backlist'.

If this variable is non-nil, ido-cr+'s whitelisting mode will be
enabled, and ido-cr+ will be disabled for *all* functions unless
they match one of the entries. Matching is done in the same
manner as `ido-cr+-function-blacklist', and blacklisting takes
precedence over whitelisting."
  :group 'ido-completing-read-plus
  :type '(repeat (choice (symbol :tag "Function or command name")
                         (string :tag "Regexp"))))

(defcustom ido-cr+-nil-def-alternate-behavior-list
  '("\\`describe-\\(function\\|variable\\)\\'"
    "\\`wl-"
    ;; https://github.com/mrkkrp/ebal/issues/12
    "\\`ebal-"
    ;; https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues/4
    webjump
    ;; https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues/83
    where-is
     ;; https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues/51
    find-tag
    ;; https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues/89
    "\\`etags-select-"
    ;; https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues/58
    imenu--completion-buffer
    ;; https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues/116
    project--completing-read-strict
    ;; https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues/127#issuecomment-319463217
    bookmark-completing-read
    )
  "Functions & commands with alternate behavior when DEF is nil.

This variable has the same format as
`ido-cr+-function-blacklist'. When `ido-completing-read+` is
called through `completing-read' by/with any command, function,
or collection matched by entries in this list, it will behave
differently when DEF is nil. Instead of using the empty string as
the default value, it will use the first element of COLLECTION.

This is needed for optimal compatibility with commands written
under the assumption that REQUIRE-MATCH means that a match is
required."
  :group 'ido-completing-read-plus
  :type '(repeat (choice (symbol :tag "Function or command name")
                         (string :tag "Regexp"))))

(defvaralias 'ido-cr+-nil-def-wall-of-shame 'ido-cr+-nil-def-alternate-behavior-list
  "Functions and commands whose authors need to read the docstring for `completing-read'.

Many functions that call `completing-read' are written with the
assumption that the setting the REQUIRE-MATCH argument of
`completing-read' to t means it is required to return a match.
While that would make logical sense, it's wrong. the docstring
for `completing-read' describes the correct behavior.

> If the input is null, ‘completing-read’ returns DEF, or the
> first element of the list of default values, or an empty string
> if DEF is nil, regardless of the value of REQUIRE-MATCH.

This can be avoided by passing an element of COLLECTION as DEF
instead of leaving it as nil.")

;;;###autoload
(defcustom ido-cr+-replace-completely nil
  "If non-nil, replace `ido-completeing-read' completely with ido-cr+.

Enabling this may interfere with or cause errors in other
packages that use `ido-completing-read'. If you discover any such
incompatibilities, please file a bug report at
https://github.com/DarwinAwardWinner/ido-completing-read-plus/issues"
  :type 'boolean)

;; Signal used to trigger fallback
(define-error 'ido-cr+-fallback "ido-cr+-fallback")

(defsubst ido-cr+--explain-fallback (arg)
  ;; This function accepts a string, or an ido-cr+-fallback
  ;; signal.
  (when ido-cr+-debug-mode
    (when (and (listp arg)
               (eq (car arg) 'ido-cr+-fallback))
      (setq arg (cadr arg)))
    (ido-cr+--debug-message "Falling back to `%s' because %s."
                            ido-cr+-fallback-function arg)))

;;;###autoload
(defsubst ido-cr+-active ()
  "Returns non-nil if ido-cr+ is currently using the minibuffer."
  (>= ido-cr+-minibuffer-depth (minibuffer-depth)))

(defun ido-cr+--called-from-completing-read ()
  "Returns non-nil if the most recent call to ido-cr+ was from `completing-read'."
  (equal (cadr (backtrace-frame 1 'ido-completing-read+))
         'completing-read))

(defmacro ido-cr+-function-is-in-list (fun fun-list &optional list-name)
  "Return non-nil if FUN matches an entry in FUN-LIST.

This is used to check for matches to `ido-cr+-function-blacklist'
and `ido-cr+-function-whitelist'. Read those docstrings to see
how the matching is done.

This is declared as macro only in order to extract the variable
name used for the second argument so it can be used in a debug
message. It should be called as if it were a normal function."
  (when (null list-name)
    (if (symbolp fun-list)
        (setq list-name (symbol-name fun-list))
      (setq list-name "list")))
  `(cl-loop
    for entry in ,fun-list
    if (cond
        ;; Nil: Never matches anything
        ((null entry)
         nil)
        ;; Symbol: Compare names and function definitions
        ((symbolp entry)
         (or (eq entry ,fun)
             (let ((entry-def (ignore-errors (indirect-function entry)))
                   (fun-def (ignore-errors (indirect-function ,fun))))
               (and
                fun-def entry-def
                (eq
                 (indirect-function entry-def)
                 (indirect-function fun-def))))))
        ;; String: Do regexp matching against function name if it is a
        ;; symbol
        ((stringp entry)
         (and (symbolp ,fun)
              (string-match-p entry (symbol-name ,fun))))
        ;; Anything else: invalid blacklist entry
        (t
         (ido-cr+--debug-message "Ignoring invalid entry in %s: `%S'" ,list-name entry)
         nil))
    return entry
    ;; If no blacklist entry matches, return nil
    finally return nil))

(defun ido-cr+-function-is-blacklisted (fun)
  "Return non-nil if FUN is blacklisted.

See `ido-cr+-function-blacklist'."
  (ido-cr+-function-is-in-list fun ido-cr+-function-blacklist))

(defun ido-cr+-function-is-whitelisted (fun)
  "Return non-nil if FUN is whitelisted.

See `ido-cr+-function-whitelist'."
  (or (null ido-cr+-function-whitelist)
      (ido-cr+-function-is-in-list fun ido-cr+-function-whitelist)))

;;;###autoload
(defun ido-completing-read+ (prompt collection &optional predicate
                                    require-match initial-input
                                    hist def inherit-input-method)
  "ido-based method for reading from the minibuffer with completion.

See `completing-read' for the meaning of the arguments.

This function is a wrapper for `ido-completing-read' designed to
be used as the value of `completing-read-function'. Importantly,
it detects edge cases that ido cannot handle and uses normal
completion for them."
  (let* (;; Save the original arguments in case we need to do the
         ;; fallback
         (ido-cr+-orig-completing-read-args
          (list prompt collection predicate require-match
                initial-input hist def inherit-input-method))
         ;; Need to save a copy of this since activating the
         ;; minibuffer once will clear out any temporary minibuffer
         ;; hooks, which need to get restored before falling back so
         ;; that they will trigger again when the fallback function
         ;; uses the minibuffer. We make a copy in case the original
         ;; list gets modified in place.
         (orig-minibuffer-setup-hook (cl-copy-list minibuffer-setup-hook))
         ;; Need just the string part of INITIAL-INPUT
         (initial-input-string
          (cond
           ((consp initial-input)
            (car initial-input))
           ((stringp initial-input)
            initial-input)
           ((null initial-input)
            "")
           (t
            (signal 'wrong-type-argument (list 'stringp initial-input)))))
         (ido-cr+-active-restrictions nil)
         ;; If collection is a function, save it for later, unless
         ;; instructed not to
         (ido-cr+-dynamic-collection
          (when (and (not ido-cr+-assume-static-collection)
                     (functionp collection))
            collection))
         (ido-cr+-last-dynamic-update-text nil)
         ;; Only memoize if the collection is dynamic.
         (ido-cr+-all-prefix-completions-memoized
          (if (and ido-cr+-dynamic-collection (featurep 'memoize))
              (memoize (indirect-function 'ido-cr+-all-prefix-completions))
            'ido-cr+-all-prefix-completions))
         (ido-cr+-all-completions-memoized
          (if (and ido-cr+-dynamic-collection (featurep 'memoize))
              (memoize (indirect-function 'all-completions))
            'all-completions))
         ;; If the whitelist is empty, everything is whitelisted
         (whitelisted (not ido-cr+-function-whitelist))
         ;; If non-nil, we need alternate nil DEF handling
         (alt-nil-def nil))
    (condition-case sig
        (progn
          ;; Check a bunch of fallback conditions
          (when (and inherit-input-method current-input-method)
            (signal 'ido-cr+-fallback
                    '("ido cannot handle alternate input methods")))

          ;; Check for black/white-listed collection function
          (when (functionp collection)
            ;; Blacklist
            (when (ido-cr+-function-is-blacklisted collection)
              (if (symbolp collection)
                  (signal 'ido-cr+-fallback
                          (list (format "collection function `%S' is blacklisted" collection)))
                (signal 'ido-cr+-fallback
                        (list "collection function is blacklisted"))))
            ;; Whitelist
            (when (and (not whitelisted)
                       (ido-cr+-function-is-whitelisted collection))
              (ido-cr+--debug-message
               (if (symbolp collection)
                   (format "Collection function `%S' is whitelisted" collection)
                 "Collection function is whitelisted"))
              (setq whitelisted t))
            ;; nil DEF list
            (when (and
                   require-match (null def)
                   (ido-cr+-function-is-in-list
                    collection
                    ido-cr+-nil-def-alternate-behavior-list))
              (ido-cr+--debug-message
               (if (symbolp collection)
                   (format "Using alternate nil DEF handling for collection function `%S'" collection)
                 "Using alternate nil DEF handling for collection function"))
              (setq alt-nil-def t)))

          ;; Expand all currently-known completions.
          (setq collection
                (if ido-cr+-dynamic-collection
                    (funcall ido-cr+-all-prefix-completions-memoized
                             initial-input-string collection predicate)
                  (all-completions "" collection predicate)))
          ;; No point in using ido unless there's a collection
          (when (and (= (length collection) 0)
                     (not ido-cr+-dynamic-collection))
            (signal 'ido-cr+-fallback '("ido is not needed for an empty collection")))
          ;; Check for excessively large collection
          (when (and ido-cr+-max-items
                     (> (length collection) ido-cr+-max-items))
            (signal 'ido-cr+-fallback
                    (list
                     (format
                      "there are more than %i items in COLLECTION (see `ido-cr+-max-items')"
                      ido-cr+-max-items))))

          ;; If called from `completing-read', check for
          ;; black/white-listed commands/callers
          (when (ido-cr+--called-from-completing-read)
            ;; Check calling command and `ido-cr+-current-command'
            (cl-loop
             for cmd in (list this-command ido-cr+-current-command)

             if (ido-cr+-function-is-blacklisted cmd)
             do (signal 'ido-cr+-fallback
                        (list "calling command `%S' is blacklisted" cmd))

             if (and (not whitelisted)
                     (ido-cr+-function-is-whitelisted cmd))
             do (progn
                  (ido-cr+--debug-message "Command `%S' is whitelisted" cmd)
                  (setq whitelisted t))

             if (and
                 require-match (null def) (not alt-nil-def)
                 (ido-cr+-function-is-in-list
                  cmd ido-cr+-nil-def-alternate-behavior-list))
             do (progn
                  (ido-cr+--debug-message
                   "Using alternate nil DEF handling for command `%S'" cmd)
                  (setq alt-nil-def t)))

            ;; Check every function in the call stack starting after
            ;; `completing-read' until to the first
            ;; `funcall-interactively' (for a call from the function
            ;; body) or `call-interactively' (for a call from the
            ;; interactive form, in which the function hasn't actually
            ;; been called yet, so `funcall-interactively' won't be on
            ;; the stack.)
            (cl-loop for i upfrom 1
                     for caller = (cadr (backtrace-frame i 'completing-read))
                     while caller
                     while (not (memq (indirect-function caller)
                                      '(internal--funcall-interactively
                                        (indirect-function 'call-interactively))))

                     if (ido-cr+-function-is-blacklisted caller)
                     do (signal 'ido-cr+-fallback
                                (list (if (symbolp caller)
                                          (format "calling function `%S' is blacklisted" caller)
                                        "a calling function is blacklisted")))

                     if (and (not whitelisted)
                             (ido-cr+-function-is-whitelisted caller))
                     do (progn
                          (ido-cr+--debug-message
                           (if (symbolp caller)
                               (format "Calling function `%S' is whitelisted" caller)
                             "A calling function is whitelisted"))
                          (setq whitelisted t))

                     if (and require-match (null def) (not alt-nil-def)
                             (ido-cr+-function-is-in-list
                              caller ido-cr+-nil-def-alternate-behavior-list))
                     do (progn
                          (ido-cr+--debug-message
                           (if (symbolp caller)
                               (format "Using alternate nil DEF handling for calling function `%S'" caller)
                             "Using alternate nil DEF handling for a calling function"))
                          (setq alt-nil-def t))))

          (unless whitelisted
            (signal 'ido-cr+-fallback
                    (list "no functions or commands matched the whitelist for this call")))

          (when (and require-match (null def))
            ;; Replace nil with "" for DEF if match is required, unless
            ;; alternate nil DEF handling is enabled
            (if alt-nil-def
                (ido-cr+--debug-message
                 "Leaving the default at nil because alternate nil DEF handling is enabled.")
              (ido-cr+--debug-message
               "Adding \"\" as the default completion since no default was provided.")
              (setq def (list ""))))

          ;; In ido, the semantics of "default" are simply "put it at
          ;; the front of the list". Furthermore, ido can't handle a
          ;; list of defaults, nor can it handle both DEF and
          ;; INITIAL-INPUT being non-nil. So, just pre-process the
          ;; collection to put the default(s) at the front and then
          ;; set DEF to nil in the call to ido to avoid these issues.
          (unless (listp def)
            ;; Ensure DEF is a list
            (setq def (list def)))
          (when def
            ;; Ensure DEF are strings
            (setq def (mapcar (apply-partially #'format "%s") def))
            ;; Prepend DEF to COLLECTION and remove duplicates
            (setq collection (delete-dups (append def collection))
                  def nil))

          ;; Check for a specific bug
          (when (and ido-enable-dot-prefix
                     (version< emacs-version "26.1")
                     (member "" collection))
            (signal 'ido-cr+-fallback
                    '("ido cannot handle the empty string as an option when `ido-enable-dot-prefix' is non-nil; see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26997")))

          ;; Fix ido handling of cons-style INITIAL-INPUT. TODO add a
          ;; version check after this bug is fixed:
          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27807
          (when (consp initial-input)
            ;; `completing-read' uses 0-based index while
            ;; `read-from-minibuffer' uses 1-based index.
            (cl-incf (cdr initial-input)))

          ;; Finally ready to do actual ido completion
          (prog1
              (let ((ido-cr+-minibuffer-depth (1+ (minibuffer-depth)))
                    (ido-cr+-dynamic-update-timer nil)
                    (ido-cr+-exhibit-pending t)
                    ;; Reset this for recursive calls to ido-cr+
                    (ido-cr+-assume-static-collection nil))
                (unwind-protect
                    (ido-completing-read
                     prompt collection
                     predicate require-match initial-input hist def
                     inherit-input-method)
                  (when ido-cr+-dynamic-update-timer
                    (cancel-timer ido-cr+-dynamic-update-timer)
                    (setq ido-cr+-dynamic-update-timer nil))))
            ;; This detects when the user triggered fallback mode
            ;; manually.
            (when (eq ido-exit 'fallback)
              (signal 'ido-cr+-fallback '("user manually triggered fallback")))))

      ;; Handler for ido-cr+-fallback signal
      (ido-cr+-fallback
       (let (;; Reset `minibuffer-setup-hook' to original value
             (minibuffer-setup-hook orig-minibuffer-setup-hook)
             ;; Reset this for recursive calls to ido-cr+
             (ido-cr+-assume-static-collection nil))
         (ido-cr+--explain-fallback sig)
         (apply ido-cr+-fallback-function ido-cr+-orig-completing-read-args))))))

;;;###autoload
(defun ido-completing-read@ido-cr+-replace (orig-fun &rest args)
  "This advice allows ido-cr+ to completely replace `ido-completing-read'.

See the varaible `ido-cr+-replace-completely' for more information."
  (if (or (ido-cr+-active)
          (not ido-cr+-replace-completely))
      ;; ido-cr+ has either already activated or isn't going to
      ;; activate, so just run the function as normal
      (apply orig-fun args)
    ;; Otherwise, we need to activate ido-cr+.
    (apply #'ido-completing-read+ args)))
;;;###autoload
(advice-add 'ido-completing-read :around
            #'ido-completing-read@ido-cr+-replace)

;;;###autoload
(defun call-interactively@ido-cr+-record-current-command
    (orig-fun command &rest args)
  "Let-bind the command being interactively called.

See `ido-cr+-current-command' for more information."
  (let ((ido-cr+-current-command command))
    (apply orig-fun command args)))
;;;###autoload
(advice-add 'call-interactively :around
            #'call-interactively@ido-cr+-record-current-command)

;; Fallback on magic C-f and C-b
(defun ido-magic-forward-char@ido-cr+-fallback (&rest _args)
  "Allow falling back in ido-completing-read+."
  (when (ido-cr+-active)
    ;; `ido-context-switch-command' is already let-bound at this
    ;; point.
    (setq ido-context-switch-command #'ido-fallback-command)))
(advice-add 'ido-magic-forward-char :before
            #'ido-magic-forward-char@ido-cr+-fallback)

(defun ido-magic-backward-char@ido-cr+-fallback (&rest _args)
  "Allow falling back in ido-completing-read+."
  (when (ido-cr+-active)
    ;; `ido-context-switch-command' is already let-bound at this
    ;; point.
    (setq ido-context-switch-command #'ido-fallback-command)))
(advice-add 'ido-magic-backward-char :before
            #'ido-magic-backward-char@ido-cr+-fallback)

(defun ido-select-text@ido-cr+-fix-require-match (orig-fun &rest args)
  "Fix ido behavior when `require-match' is non-nil.

Standard ido will allow C-j to exit with an incomplete completion
even when `require-match' is non-nil. Ordinary completion does
not allow this. In ordinary completion, RET on an incomplete
match is equivalent to TAB, and C-j selects the first match.
Since RET in ido already selects the first match, this advice
sets up C-j to be equivalent to TAB in the same situation.

This advice only activates if the current ido completion was
called through ido-cr+."
  (if (and
       ;; Only override C-j behavior if...
       ;; We're using ico-cr+, and...
       (ido-cr+-active)
       ;; Require-match is non-nil, and...
       ido-require-match
       ;; The current input doesn't exactly match a known option, and...
       (not (member ido-text ido-cur-list))
       ;; The current input doesn't exactly match an option according
       ;; to `test-completion' (or the collection is not dynamic).
       (or (not ido-cr+-dynamic-collection)
           (test-completion ido-text ido-cr+-dynamic-collection
                            (nth 2 ido-cr+-orig-completing-read-args))))
      (progn
        (ido-cr+--debug-message
         "Overriding C-j behavior for require-match: performing completion instead of exiting with current text. (This might still exit with a match if `ido-confirm-unique-completion' is nil)")
        (ido-complete))
    (apply orig-fun args)))
(advice-add 'ido-select-text :around
            #'ido-select-text@ido-cr+-fix-require-match)

(defun ido-tidy@ido-cr+-set-exhibit-pending (&rest _args)
  (setq ido-cr+-exhibit-pending t))
(advice-add 'ido-tidy :after 'ido-tidy@ido-cr+-set-exhibit-pending)

(defun ido-exhibit@ido-cr+-clear-exhibit-pending (&rest _args)
  (setq ido-cr+-exhibit-pending nil))
(advice-add 'ido-exhibit :before 'ido-exhibit@ido-cr+-clear-exhibit-pending)

(defun ido-cr+-all-prefix-completions
    (string collection &optional predicate)
  "Run `all-completions' on every prefix of STRING.

Arguments COLLECTION and PREDICATE are as in `all-completions'.
Note that \"all prefixes\" includes both STRING itself and the
empty string. The return value is the union of all the returned
lists, with elements ordered by their first occurrence.

This function is only useful if COLLECTION is a function that
might return additional completions for certain non-empty strings
that it wouldn't return for the empty string. If COLLECTION is
not a function, this is equivalent to
`(all-completions \"\" COLELCTION PREDICATE)'."
  (cond
   ;; Dynamic collection.
   ((functionp collection)
    ;; Collect completions for all prefixes of STRING starting from
    ;; "".
    (cl-loop
     for i from 0 upto (length string)
     append (funcall
             ido-cr+-all-completions-memoized
             (s-left i string)
             collection
             predicate)
     into completion-list
     finally return (delete-dups completion-list)))
   ;; If COLLECTION is not dynamic, then just call `all-completions'
   ;; on the empty string, which will already return every possible
   ;; completion.
   (t
    (all-completions "" collection predicate))))

(defun ido-cr+-apply-restrictions (collection restrictions)
  "Filter COLLECTION through RESTRICTIONS in sequence.

COLLECTION is a list of strings. RESTRICTIONS is a list of cons
cells, with the cdr being the restriction text and the car being
nil to include matches for that text and t to exclude matches for
that text. The return value is a list of strings that satisfy all
the restrictions, in the same order as they appeared in
COLLECTION.

RESTRICTIONS are applied one by one in order, which is important
because in theory the order can make a difference to the final
result."
  (cl-loop
   with filtered-collection = collection
   with need-reverse = nil
   for (removep . text) in restrictions
   for restriction-matches =
   (let ((ido-text text)
         (ido-cur-item (or ido-cur-item 'list)))
     (ido-set-matches-1 filtered-collection t))
   do (setq filtered-collection
            (if removep
                (seq-difference filtered-collection restriction-matches)
              (setq need-reverse (not need-reverse))
              restriction-matches))
   ;; Each run of `ido-set-matches-1' reverses the order, so reverse
   ;; it one more time if it had an odd number of reverses.
   finally return
   (if need-reverse
       (nreverse filtered-collection)
     filtered-collection)))

(defun ido-cr+-cyclicp (x)
  "Returns non-nill if X is a list containing a circular reference."
  (cl-loop
   for tortoise on x
   for hare on (cdr x) by #'cddr
   thereis (eq tortoise hare)))

(defun ido-cr+-update-dynamic-collection ()
  "Update the set of completions for a dynamic collection.

This has no effect unless `ido-cr+-dynamic-collection' is non-nil."
  (when (and ido-cr+-dynamic-collection
             (ido-cr+-active))
    ;; (cl-assert (not (ido-cr+-cyclicp ido-cur-list)))
    (let ((orig-ido-cur-list ido-cur-list)
          (ido-text
           (buffer-substring-no-properties (minibuffer-prompt-end)
                                           ido-eoinput)))
      ;; If current `ido-text' is equal to or a prefix of the previous
      ;; one, a dynamic update is not needed.
      (when (or (null ido-cr+-last-dynamic-update-text)
                (not (s-prefix? ido-text ido-cr+-last-dynamic-update-text)))
        (ido-cr+--debug-message "Doing a dynamic update because `ido-text' changed from %S to %S"
                                ido-cr+-last-dynamic-update-text ido-text)
        (setq ido-cr+-last-dynamic-update-text ido-text)
        (condition-case-unless-debug err
            (let* ((predicate (nth 2 ido-cr+-orig-completing-read-args))
                   (first-match (car ido-matches))
                   (strings-to-check
                    (cond
                     ;; If no match, then we only check `ido-text'
                     ((null first-match)
                      (list ido-text))
                     ;; If `ido-text' is a prefix of `first-match', then we
                     ;; only need to check `first-match'
                     ((and first-match
                           (s-prefix? ido-text first-match))
                      (list first-match))
                     ;; Otherwise we need to check both
                     (t
                      (list ido-text first-match))))
                   (new-completions
                    (cl-loop
                     for string in strings-to-check
                     append
                     (funcall
                      ido-cr+-all-prefix-completions-memoized
                      string ido-cr+-dynamic-collection predicate)
                     into result
                     finally return result)))
              ;; (cl-assert (not (ido-cr+-cyclicp new-completions)))
              (if (equal new-completions ido-cur-list)
                  (ido-cr+--debug-message "Skipping dynamic update because the completion list did not change.")
                (when (and (bound-and-true-p flx-ido-mode)
                           (functionp 'flx-ido-reset))
                  ;; Reset flx-ido since the set of completions has changed
                  (funcall 'flx-ido-reset))
                (setq ido-cur-list (delete-dups (append ido-cur-list new-completions)))
                (when ido-cr+-active-restrictions
                  (setq ido-cur-list (ido-cr+-apply-restrictions
                                      ido-cur-list
                                      ido-cr+-active-restrictions)))
                (ido-cr+--debug-message
                 "Updated completion candidates for dynamic collection. `ido-cur-list' now has %s elements"
                 ido-text (length ido-cur-list))
                ;; Recompute matches with new completions
                (let ((ido-rescan t))
                  (ido-set-matches))
                (setq ido-rescan nil)
                ;; Put the pre-update first match (if any) back in
                ;; front
                (when (and first-match
                           (not (equal first-match (car ido-matches)))
                           (member first-match ido-matches))
                  (ido-cr+--debug-message "Restoring first match %S after dynamic update" first-match)
                  (setq ido-matches (ido-chop ido-matches first-match)))
                ;; Rebuild the completion display unless ido is already planning
                ;; to do it anyway
                (unless ido-cr+-exhibit-pending
                  (ido-tidy)
                  (let ((ido-rescan nil))
                    (ido-exhibit)))))
          (error
           (display-warning 'ido-cr+
                            (format
                             "Disabling dynamic update due to error: %S"
                             err))
           ;; Reset any variables that might have been modified during
           ;; the failed update
           (setq ido-cur-list orig-ido-cur-list)
           ;; Prevent any further attempts at dynamic updating
           (setq ido-cr+-dynamic-collection nil))))))
  ;; Always cancel an active timer when this function is called.
  (when ido-cr+-dynamic-update-timer
    (cancel-timer ido-cr+-dynamic-update-timer)
    (setq ido-cr+-dynamic-update-timer nil)))

(defun ido-cr+-schedule-dynamic-collection-update ()
  "Schedule a dynamic collection update for now or in the future."
  (when (and (ido-cr+-active)
             ido-cr+-dynamic-collection)
    ;; Cancel the previous timer
    (when ido-cr+-dynamic-update-timer
      (cancel-timer ido-cr+-dynamic-update-timer)
      (setq ido-cr+-dynamic-update-timer nil))
    (cl-assert (not (ido-cr+-cyclicp ido-cur-list)))
    (if (<= (length ido-matches) 1)
        ;; If we've narrowed it down to zero or one matches, update
        ;; immediately.
        (ido-cr+-update-dynamic-collection)
      ;; If there are still several choices, defer update until idle
      (setq ido-cr+-dynamic-update-timer
            (run-with-idle-timer (max 0.01 ido-cr+-dynamic-update-idle-time) nil
                                 #'ido-cr+-update-dynamic-collection)))))

(defun ido-cr+-minibuffer-setup ()
  "set up minibuffer `post-command-hook' for ido-cr+ "
  (when (ido-cr+-active)
    (add-hook 'post-command-hook
              'ido-cr+-schedule-dynamic-collection-update)))
(add-hook 'ido-minibuffer-setup-hook
          'ido-cr+-minibuffer-setup)

;; Also need to update dynamic collections on TAB, and do so *before*
;; deciding to exit based on `ido-confirm-unique-completion'
(defun ido-complete@ido-cr+-update-dynamic-collection (oldfun &rest args)
  "Maybe update the set of completions when pressing TAB."
  (when ido-cr+-dynamic-collection
    ;; First run with `ido-confirm-unique-completion' non-nil so it
    ;; can't exit
    (let ((ido-confirm-unique-completion t))
      (apply oldfun args))
    ;; Update `ido-eoinput'
    (setq ido-eoinput (point-max))
    ;; Clear this var to force an update
    (setq ido-cr+-last-dynamic-update-text nil)
    ;; Now do update
    (ido-cr+-update-dynamic-collection))
  ;; After maybe updating the dynamic collection, if there's still
  ;; only one completion, now it's allowed to exit
  (apply oldfun args))
(advice-add 'ido-complete :around 'ido-complete@ido-cr+-update-dynamic-collection)

;; When using `ido-restrict-to-matches', we also need to add an
;; equivalent predicate to the dynamic collection so that
;; dynamically-added completions are also properly restricted.
(defun ido-restrict-to-matches@ido-cr+-record-restriction
    (&optional removep)
  "Record the restriction criterion for ido-cr+"
  (ido-cr+--debug-message "Appending restriction %S to `ido-cr+-active-restrictions'"
                          (cons removep ido-text))
  (add-to-list 'ido-cr+-active-restrictions (cons removep ido-text) t))
(advice-add 'ido-restrict-to-matches :before
            'ido-restrict-to-matches@ido-cr+-record-restriction)

;; Interoperation with minibuffer-electric-default-mode: only show the
;; default when the input is empty and the empty string is the
;; selected choice
(defun minibuf-eldef-update-minibuffer@ido-cr+-compat (orig-fun &rest args)
  "This advice allows minibuffer-electric-default-mode to work with ido-cr+."
  (if (ido-cr+-active)
      (unless (eq minibuf-eldef-showing-default-in-prompt
                  (and (string= (car ido-cur-list) "")
                       (string= ido-text "")))
        ;; Swap state.
        (setq minibuf-eldef-showing-default-in-prompt
              (not minibuf-eldef-showing-default-in-prompt))
        (overlay-put minibuf-eldef-overlay 'invisible
                     (not minibuf-eldef-showing-default-in-prompt)))
    (apply orig-fun args)))
(advice-add 'minibuf-eldef-update-minibuffer :around
            #'minibuf-eldef-update-minibuffer@ido-cr+-compat)

;;;###autoload
(define-minor-mode ido-ubiquitous-mode
  "Use ido completion instead of standard completion almost everywhere.

If this mode causes problems for a function, you can customize
when ido completion is or is not used by customizing
`ido-cr+-function-blacklist'."
  nil
  :global t
  :group 'ido-completing-read-plus
  ;; Actually enable/disable the mode by setting
  ;; `completing-read-function'.
  (setq completing-read-function
        (if ido-ubiquitous-mode
            #'ido-completing-read+
          ido-cr+-fallback-function)))

(defcustom ido-cr+-auto-update-blacklist 'notify
  "Whether to add new overrides when updating ido-cr+.

This variable has 3 possible values, with the following meanings:

  `t': Auto-update the blacklist
  `notify': Notify you about updates but do not apply them
  `nil': Ignore all blacklist updates

Ido-cr+ comes with a default blacklist for commands that are
known to be incompatible with ido completion. New versions of
ido-cr+ may come with updates to this blacklist as more
incompatible commands are discovered. However, customizing your
own overrides would normally prevent you from receiving these
updates, since Emacs will not overwrite your customizations.

To resolve this problem, you can set this variable to `t', and
then ido-cr+ can automatically add any new built-in overrides
whenever it is updated. (Actually, the update will happen the
next time Emacs is restarted after the update.) This allows you
to add your own overrides but still receive updates to the
default set.

If you want ido-cr+ to just notify you about new default
overrides instead of adding them itself, set this variable to
`notify'. If you don't want this auto-update behavior at all, set
it to `nil'.

(Note that having this option enabled effectively prevents you
from removing any of the built-in default blacklist entries,
since they will simply be re-added the next time Emacs starts.)"
  :type '(choice :tag "When new overrides are available:"
                 (const :menu-tag "Auto-add"
                        :tag "Add them automatically"
                        t)
                 (const :menu-tag "Notify"
                        :tag "Notify me about them"
                        notify)
                 (const :menu-tag "Ignore"
                        :tag "Ignore them"
                        nil))
  :group 'ido-completing-read-plus)

(defun ido-cr+-update-blacklist (&optional save quiet)
  "Re-add any missing default blacklist entries.

This is useful after an update of ido-ubiquitous that adds new
default overrides. See `ido-cr+-auto-update-blacklist' for more
information.

If SAVE is non-nil, also save the new blacklist to the user's
Custom file (but only if it was already customized beforehand).
When called interactively, a prefix argument triggers a save.

When called from Lisp code, this function returns non-nil if the
blacklist was modified."
  (interactive "P")
  (let* ((var-state (custom-variable-state 'ido-cr+-function-blacklist
                                           ido-cr+-function-blacklist))
         (curval ido-cr+-function-blacklist)
         (defval (eval (car (get 'ido-cr+-function-blacklist 'standard-value))))
         (newval (delete-dups (append defval curval)))
         (new-entries (cl-set-difference defval curval :test #'equal))
         (modified nil)
         (saved nil)
         (message-lines ()))
    (cl-case var-state
      (standard
       ;; Var is not customized, just set the new default
       (ido-cr+--debug-message "Blacklist was not customized, so it has been updated to the new default value.")
       (setq ido-cr+-function-blacklist defval
             modified new-entries))
      ((saved set changed)
       ;; Var has been customized and saved by the user, so set the
       ;; new value and maybe save it
       (ido-cr+--debug-message "Updating user-customized blacklist with new default entries.")
       (setq ido-cr+-function-blacklist newval
             modified t)
       (when (and save (eq var-state 'saved))
         (ido-cr+--debug-message "Saving new blacklist value to Custom file.")
         (customize-save-variable 'ido-cr+-function-blacklist ido-cr+-function-blacklist)
         (setq saved t)))
      (otherwise
       (ido-cr+--debug-message "Customization status of blacklist is unknown. Not modifying it.")))
    (if (and modified (not quiet))
        (progn
          (push (format "Added the following entries to `ido-cr+-function-blacklist': %S" new-entries)
                message-lines)
          (if saved
              (push "Saved the new value of `ido-cr+-function-blacklist' to your Custom file."
                    message-lines)
            (push "However, the new value of `ido-cr+-function-blacklist' has not yet been saved for future sessions. To save it. re-run this command with a prefix argument:  `C-u M-x ido-cr+-update-blacklist'; or else manually inspect and save the value using `M-x customize-variable ido-cr+-function-blacklist'."
                  message-lines)))
      (push "No updates were required to `ido-cr+-function-blacklist'." message-lines))
    (unless quiet
      (message (mapconcat #'identity (nreverse message-lines) "\n")))
    modified))

(defun ido-cr+-maybe-update-blacklist ()
  "Maybe call `ico-cr+-update-blacklist.

 See `ido-cr+-auto-update-blacklist' for more information."
  (if ido-cr+-auto-update-blacklist
      (let* ((curval ido-cr+-function-blacklist)
             (defval (eval (car (get 'ido-cr+-function-blacklist 'standard-value))))
             (new-entries (cl-set-difference defval curval :test #'equal)))
        (if new-entries
            (if (eq ido-cr+-auto-update-blacklist 'notify)
                (display-warning 'ido-completing-read+ "There are %s new blacklist entries available. Use `M-x ido-cr+-update-blacklist' to install them. (See `ido-cr+-auto-update-blacklist' for more information.)")
              (ido-cr+--debug-message "Initiating blacklist update.")
              (ido-cr+-update-blacklist t))
          (ido-cr+--debug-message "No blacklist updates available.")))
    (ido-cr+--debug-message "Skipping blacklist update by user request.")))

(ido-cr+-maybe-update-blacklist)

(provide 'ido-completing-read+)

;;; ido-completing-read+.el ends here
