;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(setup 
  (when (memq window-system '(mac ns))
    (:package exec-path-from-shell)
    (exec-path-from-shell-initialize)))
