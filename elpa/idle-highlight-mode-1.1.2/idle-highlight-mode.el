;;; idle-highlight-mode.el --- highlight the word the point is on

;; Copyright (C) 2008-2011 Phil Hagelberg, Cornelius Mika

;; Author: Phil Hagelberg, Cornelius Mika
;; URL: http://www.emacswiki.org/cgi-bin/wiki/IdleHighlight
;; Version: 1.1.2
;; Created: 2008-05-13
;; Keywords: convenience
;; EmacsWiki: IdleHighlight

;; This file is NOT part of GNU Emacs.

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

;;; Commentary:

;; Based on some snippets by fledermaus from the #emacs channel.

;; M-x idle-highlight-mode sets an idle timer that highlights all
;; occurences in the buffer of the word under the point.

;; Enabling it in a hook is recommended. But you don't want it enabled
;; for all buffers, just programming ones.
;;
;; Example:
;;
;; (defun my-coding-hook ()
;;   (make-local-variable 'column-number-mode)
;;   (column-number-mode t)
;;   (if window-system (hl-line-mode t))
;;   (idle-highlight t))
;;
;; (add-hook 'emacs-lisp-mode-hook 'my-coding-hook)
;; (add-hook 'ruby-mode-hook 'my-coding-hook)
;; (add-hook 'js2-mode-hook 'my-coding-hook)

;;; Code:

(require 'thingatpt)


(defgroup idle-highlight nil
 "Highlight other occurrences of the word at point."
 :group 'faces)

(defface idle-highlight
 '((t (:inherit region)))
 "Face used to highlight other occurrences of the word at point."
 :group 'idle-highlight)

(defvar idle-highlight-regexp nil
 "Buffer-local regexp to be idle-highlighted.")

(defvar idle-highlight-global-timer nil
 "Timer to trigger highlighting.")

(defun idle-highlight-word-at-point ()
  "Highlight the word under the point."
  (if idle-highlight-mode
      (let* ((target-symbol (symbol-at-point))
             (target (symbol-name target-symbol)))
        (if (and target-symbol
                 (not (in-string-p))
                 (looking-at-p "\\s_\\|\\sw") ;; Symbol characters
                 ;; TODO: no need to highlight keywords like if
                 (not (equal target "end"))) 
            (progn (idle-highlight-unhighlight)
                   (setq idle-highlight-regexp (concat "\\<" (regexp-quote target) "\\>"))
                   (highlight-regexp idle-highlight-regexp 'idle-highlight))
          (idle-highlight-unhighlight)
          (setq idle-highlight-regexp nil)))))

(defsubst idle-highlight-unhighlight ()
  (if idle-highlight-regexp (unhighlight-regexp idle-highlight-regexp)))

;;;###autoload
(define-minor-mode idle-highlight-mode
  "Idle-Highlight Minor Mode"
  :group 'idle-highlight
  (if idle-highlight-mode
      (progn (unless idle-highlight-global-timer
               (setq idle-highlight-global-timer
                     (run-with-idle-timer 0.5 :repeat 'idle-highlight-word-at-point)))
             (set (make-local-variable 'idle-highlight-regexp) nil))
    (idle-highlight-unhighlight)))

(provide 'idle-highlight-mode)
;;; idle-highlight-mode.el ends here
