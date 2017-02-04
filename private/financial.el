;; Financial Calculations

(defmacro /. (DIVIDEND &rest DIVISORS)
  "Returns first float-point DIVIDEND divided by all the remaining DIVISORS"
  (declare (indent 1))
  `(/ (+ ,DIVIDEND 0.0) ,@DIVISORS))

(defmacro rate. (R &optional periods)
  "Returns the rate of R on the spedified periods."
  (let ((rt (make-symbol "rt")))
    `(let ((,rt (if ,periods ,periods 1)))
       (/. ,R ,rt))))

(defun interest (P R T &optional periods)
  "Return the earned simple interest, P is the principal, R is the 
  interest rate, and T is the times"
  (let ((R1 (rate. R periods)))
    (* P R1 T)))

(defun future-value (P R T &optional periods)
  "Returns the future value using simple interest. P is the principal, R is the
  interest rate, and T is the times"
  (let ((R1 (rate. R periods)))
    (* P (+ 1 (* R1 T)))))

(defun future-value+ (P R T &optional periods)
  "Returns the future value using compound interest. P is the principal, R is the
  interest rate, and T is the times"
  (let ((R1 (rate. R periods)))
    (* P (expt (+ 1 R1) T))))

(defun interest+ (P R T &optional periods)
  "Return the earned compound interest, P is the pricipal, R is the
  interest rate, and T is times"
  (let ((R1 (rate. R periods)))
    (- (future-value+ P R1 T) P)))

(defun present-value (F R T &optional periods)
  "Returns the present value using simple interest. F is the future value, R is 
  the interest rate, and T is the times"
  (let ((R1 (rate. R periods)))
    (/. F (+ 1 (* R1 T)))))

(defun present-value+ (F R T &optional periods)
  "Returns the present value using compound interest. F is the future value, R is
  the interest rate, and T is the times"
  (let ((R1 (rate. R periods)))
    (/. F (expt (+ 1 R1) T))))

(defun times (F P R &optional periods)
  "Returns the times using simple interest. F is the future value, P is the 
  principal, and R is the interest rate"
  (let ((R1 (rate. R periods)))
    (/. (- (/. F P) 1) R1)))

(defun times+ (F P R &optional periods)
  "Returns the times using compound interest. F is the future value, P is the
  principal, and R is the interest rate"
  (let ((R1 (rate. R periods)))
    (/. (log (/. F P)) (log (+ 1 R1)))))

(defun rate (F P T &optional periods)
  "Returns the simple interest rate. F is the future value, P is the principal,
  and T is times"
  (let ((R (/. (- (/. F P) 1) T))
        (rt (if periods periods 1)))
    (* R rt)))

(defun rate+ (F P T &optional periods)
  "Returns the compound interest rate. F is the future value, P is the principal,
  and T is times"
  (let ((R  (- (expt (/. F P) (/. 1 T)) 1))
        (rt (if periods periods 1)))
    (* R rt)))
