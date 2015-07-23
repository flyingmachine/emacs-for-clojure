;; Guarantee all packages are installed on start
(defvar packages-list
  '(paredit)
  "List of packages needs to be installed at launch")

(require 'cl-lib)
(defun my/install-packages ()
  (interactive)
  (let ((missing-packages
	 (cl-remove-if #'package-installed-p packages-list)))
    (when missing-packages
      (pacakge-refresh-contents)
      (mapc #'package-install missing-packages))))

  (my/install-packages)

