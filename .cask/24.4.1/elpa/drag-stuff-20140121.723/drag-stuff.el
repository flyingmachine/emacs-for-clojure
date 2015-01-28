;;; drag-stuff.el --- Drag stuff (lines, words, region, etc...) around

;; Copyright (C) 2010-2012 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.1.0
;; Keywords: speed, convenience
;; URL: http://github.com/rejeep/drag-stuff

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; drag-stuff is a minor mode for dragging stuff around in Emacs. You
;; can drag lines, words and region.

;; To use drag-stuff, make sure that this file is in Emacs load-path
;; (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require drag-stuff
;; (require 'drag-stuff)

;; To start drag-stuff
;; (drag-stuff-mode t) or M-x drag-stuff-mode
;;
;; drag-stuff is buffer local, so hook it up
;; (add-hook 'ruby-mode-hook 'drag-stuff-mode)
;;
;; Or use the global mode to activate it in all buffers.
;; (drag-stuff-global-mode t)

;; Drag Stuff stores a list (`drag-stuff-except-modes') of modes in
;; which `drag-stuff-mode' should not be activated in (note, only if
;; you use the global mode) because of conflicting use.
;;
;; You can add new except modes:
;;   (add-to-list 'drag-stuff-except-modes 'conflicting-mode)

;; Default modifier key is the meta-key. This can be changed and is
;; controlled by the variable `drag-stuff-modifier'.
;;
;; Control key as modifier:
;;   (setq drag-stuff-modifier 'control)
;;
;; Meta and Shift keys as modifier:
;;   (setq drag-stuff-modifier '(meta shift))

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar drag-stuff-except-modes ()
  "A list of modes in which `drag-stuff-mode' should not be activated.")

(defvar drag-stuff-modifier 'meta
  "Modifier key(s) for bindings in `drag-stuff-mode-map'.")

(defvar drag-stuff-mode-map (make-sparse-keymap)
  "Keymap for `drag-stuff-mode'.")

(defvar drag-stuff-before-drag-hook nil
  "Called before dragging occurs.")

(defvar drag-stuff-after-drag-hook nil
  "Called after dragging occurs.")

(defun drag-stuff--kbd (key)
  "Key binding helper."
  (let ((mod (if (listp drag-stuff-modifier)
                 drag-stuff-modifier
               (list drag-stuff-modifier))))
    (vector (append mod (list key)))))

(defmacro drag-stuff--execute (&rest body)
  "Execute BODY without conflicting modes."
  `(let ((auto-fill-function nil)
         (electric-indent-mode nil)
         (longlines-mode-active
          (and (boundp 'longlines-mode) longlines-mode)))
     (when longlines-mode-active
       (longlines-mode -1))
     (run-hooks 'drag-stuff-before-drag-hook)
     ,@body
     (run-hooks 'drag-stuff-after-drag-hook)
     (when longlines-mode-active
       (longlines-mode 1))))

(defun drag-stuff-up (arg)
  "Drag stuff ARG lines up."
  (interactive "p")
  (drag-stuff--execute
   (if mark-active
       (drag-stuff-lines-up (- arg))
     (drag-stuff-line-up (- arg)))))

(defun drag-stuff-down (arg)
  "Drag stuff ARG lines down."
  (interactive "p")
  (drag-stuff--execute
   (if mark-active
       (drag-stuff-lines-down arg)
     (drag-stuff-line-down arg))))

(defun drag-stuff-right (arg)
  "Drag stuff ARG lines to the right."
  (interactive "p")
  (if mark-active
      (drag-stuff-region-right arg)
    (drag-stuff-word-right arg)))

(defun drag-stuff-left (arg)
  "Drag stuff ARG lines to the left."
  (interactive "p")
  (if mark-active
      (drag-stuff-region-left arg)
    (drag-stuff-word-left arg)))

(defun drag-stuff-line-up (arg)
  "Drag current line ARG lines up."
  (if (> (line-number-at-pos) (abs arg))
      (drag-stuff-line-vertically
       (lambda (beg end column)
         (drag-stuff-drag-region-up beg end arg)
         (move-to-column column)))
    (message "Can not move line further up")))

(defun drag-stuff-line-down (arg)
  "Drag current line ARG lines down."
  (if (<= (+ (line-number-at-pos) arg) (count-lines (point-min) (point-max)))
      (drag-stuff-line-vertically
       (lambda (beg end column)
         (drag-stuff-drag-region-down beg end arg)
         (move-to-column column)))
    (message "Can not move line further down")))

(defun drag-stuff-line-vertically (fn)
  "Yields variables used to drag line vertically."
  (let ((column (current-column))
        (beg (line-beginning-position))
        (end (line-end-position)))
    (funcall fn beg end column)))

(defun drag-stuff-lines-up (arg)
  "Move all lines in the selected region ARG lines up."
  (if (> (line-number-at-pos (min (point) (mark))) (abs arg))
      (drag-stuff-lines-vertically
       (lambda (beg end)
         (drag-stuff-drag-region-up beg end arg)))
    (message "Can not move lines further up")))

(defun drag-stuff-lines-down (arg)
  "Move all lines in the selected region ARG lines up."
  (if (<= (+ (line-number-at-pos (max (point) (mark))) arg) (count-lines (point-min) (point-max)))
      (drag-stuff-lines-vertically
       (lambda (beg end)
         (drag-stuff-drag-region-down beg end arg)))
    (message "Can not move lines further down")))

(defun drag-stuff-lines-vertically (fn)
  "Yields variables used to drag lines vertically."
  (let* ((deactivate-mark nil)
         (mark-line (line-number-at-pos (mark)))
         (point-line (line-number-at-pos (point)))
         (mark-col (save-excursion (exchange-point-and-mark) (current-column)))
         (point-col (current-column))
         (bounds (drag-stuff-whole-lines-region))
         (beg (car bounds))
         (end (car (cdr bounds))))
    (funcall fn beg end)
    ;; Restore region
    (goto-line mark-line)
    (forward-line arg)
    (move-to-column mark-col)
    (exchange-point-and-mark)
    (goto-line point-line)
    (forward-line arg)
    (move-to-column point-col)))

(defun drag-stuff-drag-region-up (beg end arg)
  "Drags region between BEG and END ARG lines up."
  (let ((region (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (backward-delete-char 1)
    (forward-line (+ arg 1))
    (goto-char (line-beginning-position))
    (insert region)
    (newline)
    (forward-line -1)))

(defun drag-stuff-drag-region-down (beg end arg)
  "Drags region between BEG and END ARG lines down."
  (let ((region (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (delete-char 1)
    (forward-line (- arg 1))
    (goto-char (line-end-position))
    (newline)
    (insert region)))

(defun drag-stuff-whole-lines-region ()
  "Return the positions of the region with whole lines included."
  (let (beg end)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (list beg end)))

(defun drag-stuff-region-left (arg)
  "Drags region left ARG times."
  (if (> (min (point) (mark)) (point-min))
      (drag-stuff-region-horizontally (- arg))
    (message "Can not move region further to the left")))

(defun drag-stuff-region-right (arg)
  "Drags region right ARG times."
  (if (< (max (point) (mark)) (point-max))
      (drag-stuff-region-horizontally arg)
    (message "Can not move region further to the right")))

(defun drag-stuff-region-horizontally (arg)
  "Drags region horizontally ARG times."
  (let* ((beg (mark))
         (end (point))
         (region (buffer-substring-no-properties beg end))
         (deactivate-mark nil))
    (delete-region beg end)
    (forward-char arg)
    (insert region)
    (set-mark (+ beg arg))
    (goto-char (+ end arg))))

(defun drag-stuff-word-left (arg)
  "Drags word left ARG times."
  (drag-stuff-word-horizontally (- arg)))

(defun drag-stuff-word-right (arg)
  "Drags word right ARG times."
  (drag-stuff-word-horizontally arg))

(defun drag-stuff-word-horizontally (arg)
  "Drags word horizontally ARG times."
  (let ((old-point (point))
        (offset (- (save-excursion (forward-word) (point)) (point))))
    (condition-case err
        (progn
          (transpose-words arg)
          (backward-char offset))
      (error
       (message
        (if (> arg 0)
            "Can not move word further to the right"
          "Can not move word further to the left"))
       (goto-char old-point)))))

(defun drag-stuff-define-keys ()
  "Defines keys for `drag-stuff-mode'."
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'up) 'drag-stuff-up)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'down) 'drag-stuff-down)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'right) 'drag-stuff-right)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'left) 'drag-stuff-left))

;;;###autoload
(define-minor-mode drag-stuff-mode
  "Drag stuff around."
  :init-value nil
  :lighter " drag"
  :keymap drag-stuff-mode-map
  (when drag-stuff-mode
    (drag-stuff-define-keys)))

;;;###autoload
(defun turn-on-drag-stuff-mode ()
  "Turn on `drag-stuff-mode'."
  (interactive)
  (unless (member major-mode drag-stuff-except-modes)
    (drag-stuff-mode +1)))

;;;###autoload
(defun turn-off-drag-stuff-mode ()
  "Turn off `drag-stuff-mode'."
  (interactive)
  (drag-stuff-mode -1))

;;;###autoload
(define-globalized-minor-mode drag-stuff-global-mode
  drag-stuff-mode
  turn-on-drag-stuff-mode)


(provide 'drag-stuff)

;;; drag-stuff.el ends here
