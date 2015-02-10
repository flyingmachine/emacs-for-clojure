;;; cider-browse-ns.el --- CIDER namespace browser

;; Copyright © 2014 John Andrews

;; Author: John Andrews <john.m.andrews@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; (cider-browse-ns)
;; Display a list of all vars in a namespace.
;; Pressing <enter> will take you to the cider-doc buffer for that var.
;; Pressing ^ will take you to a list of all namespaces (akin to dired mode)

;; (cider-browse-ns-all)
;; Explore clojure namespaces by browsing a list of all namespaces.
;; Pressing enter expands into a list of that namespace's vars as if by
;; executing the command (cider-browse-ns "my.ns")

;;; Code:

(require 'cider-repl)
(require 'cider-client)
(require 'cider-interaction)

(defvar cider-browse-ns-buffer "*Browse NS*")
(defvar-local cider-browse-ns-current-ns nil)

;; Utility Functions

(defun cider-browse-ns-properties (text)
  "Decorate TEXT with a clickable keymap and function face."
  (propertize text
              'font-lock-face 'font-lock-function-name-face
              'mouse-face 'highlight
              'keymap cider-browse-ns-mouse-map))


;; Mode Definition

(defvar cider-browse-ns-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cider-popup-buffer-mode-map)
    (define-key map [return] 'cider-browse-ns-operate-on-point)
    (define-key map "^" 'cider-browse-ns-all)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    map))

(define-derived-mode cider-browse-ns-mode special-mode "browse-ns"
  "Major mode for browsing Clojure namespaces.

\\{cider-browse-ns-mode-map}"
  (set-syntax-table clojure-mode-syntax-table)
  (setq buffer-read-only t)
  (setq-local electric-indent-chars nil)
  (setq-local truncate-lines t)
  (setq-local cider-browse-ns-current-ns nil))

(defun cider-browse-ns-list (buffer title items)
  "Reset contents of BUFFER.  Then display TITLE at the top and ITEMS are indented underneath."
  (with-current-buffer buffer
    (cider-browse-ns-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize title 'font-lock-face 'cider-doc-strong-face))
      (newline)
      (dolist (item items)
        (insert "  " item)
        (newline))
      (goto-char (point-min)))))

(defvar cider-browse-ns-mouse-map (make-sparse-keymap))
(define-key cider-browse-ns-mouse-map [mouse-1] 'cider-browse-ns-handle-mouse)


;; Interactive Functions

;;;###autoload
(defun cider-browse-ns (namespace)
  "List all NAMESPACE's vars in BUFFER."
  (interactive (list (completing-read "Browse namespace: " (cider-sync-request:ns-list))))
  (with-current-buffer (cider-popup-buffer cider-browse-ns-buffer t)
    (let ((vars (cider-sync-request:ns-vars namespace)))
      (cider-browse-ns-list (current-buffer)
                            namespace
                            (mapcar (lambda (var)
                                      (format "/%s"
                                              (cider-browse-ns-properties var)))
                                    vars))
      (setq-local cider-browse-ns-current-ns namespace))))

;;;###autoload
(defun cider-browse-ns-all ()
  "List all loaded namespaces in BUFFER."
  (interactive)
  (with-current-buffer (cider-popup-buffer cider-browse-ns-buffer t)
    (let ((names (cider-sync-request:ns-list)))
      (cider-browse-ns-list (current-buffer)
                            "All loaded namespaces"
                            (mapcar (lambda (name)
                                      (cider-browse-ns-properties name))
                                    names))
      (setq-local cider-browse-ns-current-ns nil))))

(defun cider-browse-ns-operate-on-point ()
  "Expand browser according to thing at current point."
  (interactive)
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (cond
     ((= 1 (line-number-at-pos))
      'nothing-to-do)
     ((string-match " +/\\(.+\\)" line)
      (cider-doc-lookup (format "%s/%s" cider-browse-ns-current-ns (match-string 1 line))))
     ('else
      (cider-browse-ns (replace-regexp-in-string " " "" line))))))

(defun cider-browse-ns-handle-mouse (event)
  "Handle mouse click EVENT."
  (interactive "e")
  (cider-browse-ns-operate-on-point))

(provide 'cider-browse-ns)

;;; cider-browse-ns.el ends here
