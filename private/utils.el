;;;;
;; Utils
;;;;


(safe-do
 cl-labels
 (defun take (n seq)
   "Returns a sequence of the first n itmes in seq, or all items if
   there are fewer than n."
   (cl-labels ((iter (n seq acc)
                     (if (or (<= n 0) (null seq))
                         acc
                       (iter (1- n) (cdr seq) (cons (car seq) acc)))))
     (nreverse (iter n seq nil)))))

(safe-do
 number-sequence
 (fset 'range 'number-sequence))

(safe-do
 cl-prettyexpand
 (fset 'pprint 'cl-prettyexpand))


(defun int-to-binary-string (i)
  "Display an integer in binary string representation."
  (let ((s ""))
    (while (not (= i 0))
      (setq s (concat (if (= 1 (logand i 1)) "1" "0") s))
      (setq i (lsh i -1)))
    (concat "#b" (if (string= s "") (setq s "0") s))))
