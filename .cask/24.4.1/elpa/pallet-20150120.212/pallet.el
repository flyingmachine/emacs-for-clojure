;;; pallet.el --- Manage your packages with Cask.

;; Copyright (C) 2015 Robert Dallas Gray

;; Author: Robert Dallas Gray
;; URL: https://github.com/rdallasgray/pallet
;; Version: 0.9.0
;; Created: 2013-02-24
;; Keywords: elpa, package

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
;;
;; [![Melpa Status](http://melpa.milkbox.net/packages/pallet-badge.svg)](http://melpa.milkbox.net/#/pallet)
;; [![Melpa Stable Status](http://melpa-stable.milkbox.net/packages/pallet-badge.svg)](http://melpa-stable.milkbox.net/#/pallet)
;; 
;; 
;; #Pallet
;; Pallet is a package management helper for Emacs.
;; 
;; It uses @rejeep's excellent
;; [Cask](https://github.com/cask/cask) as a platform to keep
;; track of your installed packages.
;; 
;; ##News
;; Version 0.9 introduces support for Cask version and VC references,
;; which will now be retained if specified in your Cask file:
;; ```
;; (depends on "graphene" "20141030.219")
;; (depends on "pallet" :git "https://github.com/rdallasgray/pallet" :ref "master")
;; ```
;; Many thanks to [Sam Brightman](https://github.com/sambrightman) for
;; implementing this feature.
;; 
;; Version 0.8 introduces the `;;;pallet-ignore` comment, which allows
;; you to tell Pallet to ignore (and retain) text following the comment.
;; 
;; Version 0.7 introduces a significant breaking change: it is now
;; necessary to start `pallet-mode` for pallet to track your package
;; installs and deletes. See the instructions below.
;; 
;; Version 0.7 introduces a new integration test harness using
;; [Servant](https://github.com/cask/servant). This is intended to allow
;; safer and quicker addition of new features going forward.
;; 
;; ##Target platform
;; Pallet is currently tested with Emacs versions 24.3 through 24.4.
;; 
;; ##Use
;; Pallet has a very simple interface:
;; - `M-x pallet-init` creates a Cask file using information about
;;   installed packages from the package.el system
;; - `M-x pallet-install` installs packages listed in your Cask file
;; - `M-x pallet-update` updates installed packages
;; 
;; Pallet's main job, though, is to add and delete package references
;; from your Cask file as you install and delete them using the built-in
;; Emacs package management system. Turn this on by adding `(pallet-mode
;; t)` to your Emacs init file, or by calling `pallet-mode` interactively (`M-x
;; pallet-mode`).
;; 
;; ##Installation
;; To install pallet, you should first install Cask, following the
;; instructions [here](http://cask.readthedocs.org/en/latest/). **At present,
;; just install Cask -- don't add anything to your .emacs or init.el file**.
;; 
;; After installing Cask, there are two ways you can go, depending on
;; your situation:
;; 
;; 1. **I have a working Emacs install, with packages already installed,
;;    and can access [Melpa](http://melpa.org).**
;; 
;;    In this case run `M-x list-packages`, and install pallet.  Then,
;;    run `M-x pallet-init`. Now you have a Cask file in your emacs.d
;;    directory which contains listings for all files you've previously
;;    installed via `package-install`. Run `M-x pallet-install`, and your
;;    .emacs.d/elpa directory will be replicated under .emacs.d/.cask/.
;; 
;;    You can if you wish now delete your .emacs.d/elpa directory, and
;;    remove any lines from your init.el adding archives to
;;    `package-archive`, or running `package-initialize`.
;; 
;; 2. **I have a newly installed Emacs and/or am not set up to access
;;    Melpa.**
;; 
;;    In this case, create a file called `Cask` in your emacs.d
;;    directory. Add the following lines to it:
;; 
;;    ```lisp
;;    (source melpa)
;; 
;;    (depends-on "pallet")
;;    ```
;; 
;;    Then, in terminal and in your emacs.d directory, run
;; 
;;    ```
;;    cask install
;;    ```
;; 
;;    This will create a .cask directory inside your .emacs.d directory,
;;    initialize a package directory under .emacs.d/.cask/, and install
;;    pallet to it.
;; 
;; **Finally, make sure the following lines are in your init.el, before any
;;   packages are required:**
;; 
;; ```lisp
;; (require 'cask "<path-to-cask>/cask.el")
;; (cask-initialize)
;; (require 'pallet)
;; (pallet-mode t)
;; ```
;; 
;; `<path-to-cask>` will vary depending on how you installed Cask: if you
;; installed via the `curl` method, it is likely to be `~/.cask`; if you
;; installed via Homebrew, it is likely to be
;; `/usr/local/Cellar/cask/<version>`.
;; 
;; If you want pallet to maintain your Cask file automatically as you
;; install and delete packages using Emacs' built-in package-management,
;; enable `pallet-mode` by calling `(pallet-mode t)`. You can enable or
;; disable `pallet-mode` at any time by interactively calling
;; `pallet-mode` (`M-x pallet-mode`).
;; 
;; ##Ignoring a section of your Cask file
;; If you prefer to have Pallet ignore part of your Cask file (e.g. so
;; you can use Cask's
;; [VC dependencies](http://cask.readthedocs.org/en/latest/guide/dsl.html#dependencies)),
;; use the `;;;pallet-ignore` comment. Pallet will ignore any text after
;; this comment.
;; ```lisp
;; (source melpa)
;; (depends-on "s")
;; ;;;pallet-ignore
;; (depends-on "newlisp" :git
;; "https://github.com/coldnew/newlisp-mode.git")
;; ```
;; 
;; ##Contributing
;; Contributions to pallet are very welcome.
;; 
;; Fork and clone the repo, then run `git
;; submodule update --init`, which will install
;; [el.mk](http://github.com/rdallasgray/el.mk).
;; 
;; ###Simple testing
;; Install [Cask](http://cask.readthedocs.org/en/latest).
;; 
;; Then run `cask install` to install development dependencies. You
;; should now be able to run the tests: `make test`.
;; 
;; ###Complete testing
;; The pallet dev setup includes a Vagrantfile, which allows pallet to be
;; tested against a selection of recent Emacs releases.
;; 
;; Having installed [Vagrant](https://vagrantup.com), add the necessary
;; box by running:
;; ```bash
;; vagrant box add trusty-server \
;; https://cloud-images.ubuntu.com/vagrant/trusty/current/trusty-server-cloudimg-i386-vagrant-disk1.box
;; ```
;; 
;; Then run `vagrant up`. This may take a while, as several versions of
;; Emacs may be downloaded and installed from source.
;; 
;; Shell into the vm by running `vagrant ssh`, and run the tests using
;; `./test_all.sh`. This will run the complete test suite against all
;; installed Emacs versions.
;; 
;; ###Pull requests
;; Any new feature or fix should be covered by tests -- see the files
;; in /test for guidance on how to write your own. When you've
;; created your feature or fix, make a pull request against master in this repo.
;;
;;; Code:

;; We need to get a copy of the package-archives alist
;; before requiring Cask, as doing so will empty the list.
(package-initialize)

(defvar pallet--package-archives-copy
  (copy-alist package-archives))

(require 'cask)
(require 'f)

;; interactive/api functions

(defun pallet-init ()
  "Bootstrap a Cask setup from package.el information."
  (interactive)
  (pallet--repack t))

(defun pallet-install ()
  "Install packages from the Cask file."
  (interactive)
  (pallet--cask-up
   (lambda (bundle) (cask-install bundle))))

(defun pallet-update ()
  "Update installed packages."
  (interactive)
  (pallet--cask-up
   (lambda (bundle) (cask-update bundle))))

;;; private

(defvar pallet--ignored-text-comment ";;;pallet-ignore")

(defun pallet--on ()
  "Add and remove entries from your Cask file on `package-install' and `package-delete'."
  (ad-enable-advice 'package-install 'after 'pallet--after-install)
  (ad-enable-advice 'package-delete 'after 'pallet--after-delete)
  (ad-activate 'package-install)
  (ad-activate 'package-delete))

(defun pallet--off ()
  "Stop reacting to `package-install' and `package-delete'."
  (ad-disable-advice 'package-install 'after 'pallet--after-install)
  (ad-disable-advice 'package-delete 'after 'pallet--after-delete)
  (ad-activate 'package-install)
  (ad-activate 'package-delete))

(defun pallet--repack (&optional use-copy)
  "Recreate the Cask file from package.el information;
use `pallet--package-archives-copy' if USE-COPY is true."
  (let ((archive-alist
         (if use-copy pallet--package-archives-copy package-archives)))
    (pallet--ship archive-alist (pallet--pick-packages))))

(defun pallet--cask-up (&optional body)
  "Attempt to initialize Cask, optionally running BODY if initialisation succeeds."
  (if (file-exists-p (pallet--cask-file))
      (let ((bundle (cask-initialize)))
        (when body (funcall body bundle)))
    (message "No Cask file found. Run `pallet-init' to create one.")))

(defun pallet--cask-file ()
  "Location of the Cask file."
  (expand-file-name "Cask" user-emacs-directory))

(defun pallet--package-name (package-name-or-desc)
  "Return a package name from a string or package-desc struct in PACKAGE-NAME-OR-DESC."
  (cond ((symbolp package-name-or-desc) package-name-or-desc)
        ((stringp package-name-or-desc) (intern package-name-or-desc))
        ((fboundp 'package-desc-name) (package-desc-name package-name-or-desc))))

(defun pallet--pick-packages ()
  "Get a simple list of installed packages."
  (if package-alist
      (let ((picked '()))
        (dolist (package package-alist)
          (push (make-cask-dependency :name (car package)) picked))
        (reverse picked))
    nil))

(defun pallet--pick-cask (bundle)
  "Get a list of dependencies from the Cask BUNDLE."
  (pallet--pick-cask-except bundle nil))

(defun pallet--pick-cask-except (bundle excluded-package-name)
  "Get a list of dependencies from the Cask BUNDLE, excluding EXCLUDED-PACKAGE-NAME."
  (let ((picked '()))
    (dolist (package (cask-runtime-dependencies bundle))
      (when (not (equal (cask-dependency-name package)
                      excluded-package-name))
          (push package picked)))
    (delete-dups picked)))

(defun pallet--pack (archives packages)
  "Construct a Caskfile from ARCHIVES and PACKAGES."
  (format "%s\n\n%s\n"
          (pallet--write-sources archives)
          (pallet--write-depends packages)))

(defun pallet--pack-one (package-name)
  "Add PACKAGE-NAME to the Caskfile."
  (pallet--cask-up
   (lambda (bundle)
     (cask-add-dependency bundle package-name :scope 'runtime)
     (pallet--ship package-archives (pallet--pick-cask bundle)))))

(defun pallet--unpack-one (package-name)
  "Remove a PACKAGE-NAME from the Caskfile."
  (pallet--cask-up
   (lambda (bundle)
     (pallet--ship package-archives
                     (pallet--pick-cask-except bundle package-name)))))

(defun pallet--ship (archives packages)
  "Create and save a Caskfile based on installed ARCHIVES and PACKAGES."
  (let ((ignored-text (when (f-exists? (pallet--cask-file))
                        (pallet--ignored-text
                         (f-read-text (pallet--cask-file))))))
    (pallet--write-file (pallet--cask-file)
                        (pallet--with-ignored-text
                         ignored-text
                         (pallet--pack archives packages)))))

(defun pallet--with-ignored-text (ignored-text text)
  "Maybe insert IGNORED-TEXT below a comment, after TEXT."
  (if ignored-text
      (concat text pallet--ignored-text-comment ignored-text)
    text))

(defun pallet--ignored-text (text)
  "Find TEXT after `pallet--ignored-text-comment'."
  (nth 1 (s-split pallet--ignored-text-comment text)))

(defun pallet--write-sources (archive-list)
  "Create a Caskfile source set from ARCHIVE-LIST."
  (let ((source-list '()))
    (dolist (source archive-list)
      (push (pallet--format-source source) source-list))
    (mapconcat 'identity (sort source-list #'string<) "\n")))

(defun pallet--format-source (source)
  "Return a string correctly formatting an archive SOURCE."
  (let ((cask-source `(,(intern (car source)) . ,(cdr source))))
    (if (member cask-source cask-source-mapping)
        (format "(source %s)" (car source))
      (format "(source \"%s\" \"%s\")" (car source) (cdr source)))))

(defun pallet--write-depends (package-list)
  "Create a Caskfile dependency set from PACKAGE-LIST."
  (let ((depends-list '()))
    (dolist (package package-list)
      (push (pallet--format-dependency package) depends-list))
    (let ((depends-list (sort depends-list #'string<)))
      (mapconcat 'identity depends-list "\n"))))

(defun pallet--format-dependency (package)
  "Return a string correctly formatting a dependency PACKAGE."
  (let ((depend-args (list (symbol-name (cask-dependency-name package)))))
    (-when-let (version (cask-dependency-version package))
      (nconc depend-args (list version)))
    (-when-let (fetcher (cask-dependency-fetcher package))
      (nconc depend-args (list fetcher))
      (let ((url (cask-dependency-url package)))
        (nconc depend-args (list url)))
      (-when-let (ref (cask-dependency-ref package))
        (nconc depend-args (list :ref ref)))
      (-when-let (branch (cask-dependency-branch package))
        (nconc depend-args (list :branch branch)))
      (-when-let (files (cask-dependency-files package))
        (nconc depend-args (list :files files))))
    (format "(depends-on %s)" (mapconcat
                               (lambda (x) (format "%S" x))
                               depend-args " "))))

(defun pallet--write-file (file contents)
  "Write to FILE the given (string) CONTENTS."
  (f-write contents 'utf-8 file))

(defun pallet--installed-p (package-name)
  "Return t if (string) PACKAGE-NAME is installed, or nil otherwise."
  ;; Ensure we have up-to-date information -- package-delete doesn't
  ;; recreate package-alist automatically.
  (epl-initialize t)
  (epl-package-installed-p package-name))


;; advise package.el functions

(defadvice package-install
    (after pallet--after-install (package-name-or-desc))
  "Add a dependency to the Cask file after `package-install'."
  (let ((package-name (pallet--package-name package-name-or-desc)))
    (message "Pallet: packing %s" package-name)
    (pallet--pack-one package-name)))

(defadvice package-delete
  (after pallet--after-delete (package-name-or-desc &optional version))
  "Remove a dependency from the Cask file after `package-delete'."
  ;; NB check if package is still installed; updates trigger deletes
  (let ((package-name (pallet--package-name package-name-or-desc)))
    (when (not (pallet--installed-p package-name))
      (message "Pallet: unpacking %s" package-name)
      (pallet--unpack-one package-name))))

;;;###autoload
(define-minor-mode pallet-mode
  "Maintain entries in your Cask file automatically."
  :init-value nil
  :global t
  :group 'pallet
  (if pallet-mode
      (pallet--on)
    (pallet--off)))

(provide 'pallet)

;;; pallet.el ends here
