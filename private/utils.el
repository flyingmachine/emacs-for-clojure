;;;;
;; Utils
;;;;

;; take, in Elisp use `number-sequence' instead range
(safe-do
 cl-labels
 (defun take (n seq)
   "Take n elements from seq."
   (cl-labels ((iter (n seq acc)
                     (if (or (<= n 0) (null seq))
                         acc
                       (iter (1- n) (cdr seq) (cons (car seq) acc)))))
     (nreverse (iter n seq nil)))))


(defun int-to-binary-string (i)
  "Display an integer in binary string representation."
  (let ((s ""))
    (while (not (= i 0))
      (setq s (concat (if (= 1 (logand i 1)) "1" "0") s))
      (setq i (lsh i -1)))
    (concat "#b" (if (string= s "") (setq s "0") s))))
