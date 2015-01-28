;;; flycheck-cask.el --- Cask support in Flycheck -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/flycheck/flycheck-cask
;; Keywords: tools, convenience
;; Version: 20141217.537
;; X-Original-Version: 0.3-cvs
;; Package-Requires: ((emacs "24.1") (flycheck "0.14") (dash "2.4.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add Cask support for Flycheck.

;; Configure Flycheck to initialize packages from Cask in Cask projects.

;;;; Setup

;; (add-hook 'flycheck-mode-hook #'flycheck-cask-setup)

;;; Code:

(require 'flycheck)
(require 'dash)

(defgroup flycheck-cask nil
  "Cask support for Flycheck."
  :prefix "flycheck-cask-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck-cask"))

(defcustom flycheck-cask-add-root-directory t
  "When non-nil, add the root directory to the load path.

If this variable is non nil, add the root directory of a Cask
project to `flycheck-emacs-lisp-load-path'."
  :group 'flycheck-cask
  :type 'boolean)

(defun flycheck-cask-package-dir (root-dir)
  "Get the package directory for ROOT-DIR."
  (expand-file-name (format ".cask/%s/elpa" emacs-version) root-dir))

;;;###autoload
(defun flycheck-cask-setup ()
  "Setup Cask integration for Flycheck.

If the current file is part of a Cask project, as denoted by the
existence of a Cask file in the file's directory or any ancestor
thereof, configure Flycheck to initialze Cask packages while
syntax checking.

Set `flycheck-emacs-lisp-initialize-packages' and
`flycheck-emacs-lisp-package-user-dir' accordingly."
  (when (buffer-file-name)
    (-when-let (root-dir (locate-dominating-file (buffer-file-name) "Cask"))
      (setq-local flycheck-emacs-lisp-initialize-packages t)
      (setq-local flycheck-emacs-lisp-package-user-dir
                  (flycheck-cask-package-dir root-dir))
      (when (eq flycheck-emacs-lisp-load-path 'inherit)
        ;; Disable `load-path' inheritance if enabled.
        (setq-local flycheck-emacs-lisp-load-path nil))
      (when flycheck-cask-add-root-directory
        (setq-local flycheck-emacs-lisp-load-path
                    (cons root-dir flycheck-emacs-lisp-load-path))))))

(provide 'flycheck-cask)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; flycheck-cask.el ends here
