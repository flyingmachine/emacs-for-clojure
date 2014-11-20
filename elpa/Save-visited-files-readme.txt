;;; Commentary:

;; save-visited-files is a lightweight version of Desktop.el that
;; only save the files you have open(currently).  This was created because I
;; couldn't ever get Desktop to work and wanted to persist open files across
;; sessions.  This file is the result.

;;; Installation:

;; To install, put this file somewhere in your load-path and add the following
;; to your .emacs file:
;;
;; (require 'save-visited-files)
;; (turn-on-save-visited-files-mode)
;;
;; This will load the set of saved files on startup, as well as updating this
;; list whenever the auto-save-timer is run.  This does not wait to save on
;; closing emacs because I wanted it to be useful even if emacs crashed.  To
;; save the visited files at any time, you can call M-x save-visited-files-save.
;; M-x save-visited-files-restore will open all files saved this way.  To turn
;; off the saving of files, you need to run (turn-off-save-visited-files-mode)

;; Changelog:
;; 1.2
;;  * Changed default value of save-visited-files-location to ~/.emacs.d/emacs-visisted-files
;;  * Improvements/rewriting by Jonathan Kotta
;;  ** Checks save-visited-files-location is writable, and gives a message if not
;;  ** Changed to use define-minor-mode
;;  ** Moved (setq save-visited-files-already-restored t) to the end of
;;  ** save-visited-files-restore from save-visited-files-mode.
;;  ** Doesn't print a message in the echo area every time it saves the file list.
;; 1.1
;;  * Improvements/rewriting by Ryan Thomson
;;  ** Use auto-save-hook instead of a periodic timer
;;  ** More consistent naming conventions
;;  ** Customization ability via M-x customize-group save-visited-files
;;  ** Better handling of the temp buffer
;; 1.0
;;  * Initial Release

