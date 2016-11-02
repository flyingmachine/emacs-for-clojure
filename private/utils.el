(defun int-to-binary-string (i)
  "Display an integer in binary string representation."
  (let ((s ""))
    (while (not (= i 0))
      (setq s (concat (if (= 1 (logand i 1)) "1" "0") s))
      (setq i (lsh i -1)))
    (concat "#b" (if (string= s "") (setq s "0") s))))
