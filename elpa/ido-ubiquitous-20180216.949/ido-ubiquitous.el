;;; ido-ubiquitous.el --- Use ido (nearly) everywhere. -*- lexical-binding: t -*-

;; Copyright (C) 2011-2017 Ryan C. Thompson

;; Author: Ryan C. Thompson
;; URL: https://github.com/DarwinAwardWinner/ido-ubiquitous
;; Package-Version: 20180216.949
;; Version: 4.8
;; Created: 2011-09-01
;; Keywords: convenience, completion, ido
;; EmacsWiki: InteractivelyDoThings
;; Package-Requires: ((ido-completing-read+ "4.8") (cl-lib "0.5"))
;; Filename: ido-ubiquitous.el

;; This file is NOT part of GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; Previously a separate package, ido-ubiquitous has now been subsumed
;; into ido-completing-read+. You should update your config to install that instead.

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

(defconst ido-ubiquitous-version "4.8"
  "Currently running version of ido-ubiquitous.

Note that when you update ido-ubiquitous, this variable may not
be updated until you restart Emacs.")

(require 'ido-completing-read+)

(display-warning 'ido-ubiquitous "The ido-ubiquitous package is now redundant. All functionality, including ido-ubiquitous-mode, has been merged into the ido-completing-read+ package. You should replace ido-ubiquitous with ido-completing-read+ in your Emacs config. For more information, see:
https://github.com/DarwinAwardWinner/ido-ubiquitous#version-40-changes")

(define-obsolete-function-alias 'completing-read-ido-ubiquitous 'ido-completing-read+
  "ido-completing-read+ 4.0")
(define-obsolete-function-alias 'ido-ubiquitous-update-overrides 'ido-cr+-update-blacklist
  "ido-completing-read+ 4.0")
(define-obsolete-function-alias 'ido-ubiquitous--maybe-update-overrides 'ido-cr+-maybe-update-blacklist
  "ido-completing-read+ 4.0")
(define-obsolete-variable-alias 'ido-ubiquitous-auto-update-overrides 'ido-cr+-auto-update-blacklist
  "ido-completing-read+ 4.0")

(make-obsolete-variable
 'ido-ubiquitous-default-state
 "For the new variables to control which commands have ido completion, see `ido-cr+-function-blacklist' and `ido-cr+-function-whitelist'. For information on what happened to \"old-style\" default selection, See the FAQ."
 "ido-completing-read+ 4.0")
(make-obsolete-variable
 'ido-ubiquitous-command-overrides
 "For the new variables to control which commands have ido completion, see `ido-cr+-function-blacklist' and `ido-cr+-function-whitelist'. For information on what happened to \"old-style\" default selection, See the FAQ."
 "ido-completing-read+ 4.0")
(make-obsolete-variable
 'ido-ubiquitous-function-overrides
 "For the new variables to control which commands have ido completion, see `ido-cr+-function-blacklist' and `ido-cr+-function-whitelist'. For information on what happened to \"old-style\" default selection, See the FAQ."
 "ido-completing-read+ 4.0")
(make-obsolete-variable
 'ido-ubiquitous-allow-on-functional-collection
 "Ido-cr+ now works with most dynamic completion tables (i.e. \"functional collections\"), so this variable is no longer necessary. If a specific command uses a dynamic completion table that conflicts with ido-cr+, add it to `ido-cr+-function-blacklist' instead."
 "ido-completing-read+ 4.0")

(provide 'ido-ubiquitous)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ido-ubiquitous.el ends here
