;; Financial Calculations

(defun /. (DIVIDEND &rest DIVISORS)
  "Returns first float-point DIVIDEND divided by all the remaining DIVISORS"
  (cl-reduce '/ (cons (+ DIVIDEND 0.0) DIVISORS)))

(defun rate/365 (R &optional annual?)
  "Returns daily rate of R when the &optional annual? is true, others returns R"
  (if annual? (/. R 365) R))

(defun rate*365 (R &optional annual?)
  "Returns annual rate of R when the &optional annual? is ture, others returns R"
  (if annual? (* R 365) R))

(defun interest (P R T &optional annual?)
  "Return the earned simple interest, P is the pricipal, R is the 
  interest rate, and T is times"
  (let ((R1 (rate/365 R annual?)))
    (* P R1 T)))

(defun future-value (P R T &optional annual?)
  "Returns the future value using simple interest. P is the principal, R is the
  interest rate, and T is the times"
  (let ((R1 (rate/365 R annual?)))
    (* P (+ 1 (* R1 T)))))

(defun future-value+ (P R T &optional annual?)
  "Returns the future value using compound interest. P is the principal, R is the
  interest rate, and T is the times"
  (let ((R1 (rate/365 R annual?)))
    (* P (expt (+ 1 R1) T))))

(defun interest+ (P R T &optional annual?)
  "Return the earned compound interest, P is the pricipal, R is the
  interest rate, and T is times"
  (let ((R1 (rate/365 R annual?)))
    (- (future-value+ P R1 T) P)))

(defun present-value (F R T &optional annual?)
  "Returns the present value using simple interest. F is the future value, R is 
  the interest rate, and T is the times"
  (let ((R1 (rate/365 R annual?)))
    (/. F (+ 1 (* R1 T)))))

(defun present-value+ (F R T &optional annual?)
  "Returns the present value using compound interest. F is the future value, R is
  the interest rate, and T is the times"
  (let ((R1 (rate/365 R annual?)))
    (/. F (expt (+ 1 R1) T))))

(defun times (F P R &optional annual?)
  "Returns the times using simple interest. F is the future value, P is the 
  principal, and R is the interest rate"
  (let ((R1 (rate/365 R annual?)))
    (/. (- (/. F P) 1) R1)))

(defun times+ (F P R &optional annual?)
  "Returns the times using compound interest. F is the future value, P is the
  principal, and R is the interest rate"
  (let ((R1 (rate/365 R annual?)))
    (/. (log (/. F P)) (log (+ 1 R1)))))

(defun rate (F P T &optional annual?)
  "Returns the simple interest rate. F is the future value, P is the principal,
  and T is times"
  (let ((R (/. (- (/. F P) 1) T)))
    (rate*365 R annual?)))

(defun rate+ (F P T &optional annual?)
  "Returns the compound interest rate. F is the future value, P is the principal,
  and T is times"
  (let ((R  (- (expt (/. F P) (/. 1 T)) 1)))
    (rate*365 R annual?)))
