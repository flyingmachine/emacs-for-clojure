;; magit is a full-fledged interface for git
;; https://magit.vc/manual/magit/
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)
(setup (:package magit)
  (:global "C-M-;" magit-status))
