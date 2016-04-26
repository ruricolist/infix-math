(defpackage :infix-math/contractions
  (:use :cl)
  (:export
   :sq :ln :lg :lb
   :log1+ :log1-
   :log1+/x
   :exp-1
   :exp-1/x
   :expt-1
   :log1-exp
   :log1+exp
   :log2-exp
   :hypot
   :exptm))

(in-package :infix-math/contractions)

;; Seemingly useless functions

;; References:

;; <http://www.plunk.org/~hatch/rightway.php> for log1+ and exp-1,
;; both ultimately from a document of Kahan's no longer online.

(declaim (inline sq ln lb lg log1+ log1-))

(defun sq (x)
  (* x x))

(defun ln (n)
  "Natural logarithm.
This is almost, but not quite, the same as (log x): we try to ensure
that the result is a double float."
  (log n (exp 1d0)))

(defun lb (n)
  "Binary logarithm."
  (log n 2.0d0))

(defun lg (n)
  "Decimal logarithm."
  (log n 10.0d0))

(defun log1+ (x)
  "Compute (log (+ 1 x)) stably even when X is near zero."
  (let ((u (+ 1 x)))
    (if (= u 1)
        x
        (/ (* x (ln u))
           (- u 1)))))

(defun log1+/x (x)
  "Compute (/ (log (+ 1 x)) x) stably even when X is near zero."
  (let ((u (+ x 1)))
    (if (= u 1)
        x
        (/ (log u)
           (- u 1)))))

(defun log1- (x)
  "Compute (log (- 1 x)) stably even when X is near zero."
  (log1+ (- x)))

(defun exp-1 (x)
  "Compute (- (exp x) 1) stably even when X is near zero."
  (let ((u (exp x)))
    (if (= u 1)
        x
        (let ((v (- u 1)))
          (if (= v -1)
              -1
              (/ (* v x)
                 (ln u)))))))

(defun exp-1/x (x)
  "Compute (/ (- (exp x) 1) x) stably even when X is near zero."
  (let ((u (exp x)))
    (if (= u 1)
        x
        (let ((v (- u 1)))
          (if (= v -1)
              (/ -1 x)
              (/ v (log u)))))))

;; XXX Boost.
(defun expt-1 (a z)
  "Compute (base^pow)-1.
Aka powm1."
  (or (and (or (< (abs a) 1)
               (< (abs z) 1))
           (let ((p (* (log a) z)))
             (and (< (abs p) 2)
                  (exp-1 p))))
      (- (expt a z) 1)))

(defun log1-exp (a)
  "Compute log(1-exp(x)) stably even when A is near zero.

This is sometimes known as the E_3, the third Einstein function.

See Mächler 2008 for notes on accurate calculation.

https://cran.r-project.org/web/packages/Rmpfr/vignettes/log1mexp-note.pdf"
  (cond ((or (complexp a) (minusp a))
         ;; XXX
         (log (- 1 (exp a))))
        ((<= a 0)                       ;XXX
         #+ () (log1- (- (exp (- a))))
         (log1+ (- (exp a)))
         #+ () (log (- 1 (exp a))))
        ((<= a #.(log 2d0))
         (log (- (exp-1 a))))
        (t
         ;; The paper has -a, but that's wrong.
         (log1+ (- (exp a))))))

(defun log1+exp (a)
  "Accurately compute log(1+exp(x)) even when A is near zero."
  (if (realp a)
      (let ((x (coerce a 'double-float)))
        (cond ((<= x -37)
               (exp x))
              ((<= x 18)
               (log1+ (exp x)))
              ((<= x 33.3)
               (+ x (exp (- x))))
              (t x)))
      (log (+ 1 (exp a)))))

(defun log2-exp (x)
  (log1+ (- (exp-1 x))))



(defun hypot (x y)
  "Compute the hypotenuse of X and Y without danger of floating-point
overflow or underflow."
  (setf x (abs x)
        y (abs y))
  (when (< x y)
    (rotatef x y))
  (* x (sqrt (+ 1 (sq (/ y x))))))

;;; http://www.cliki.net/EXPT-MOD
(defun expt-mod (n exponent modulus)
  "As (mod (expt n exponent) modulus), but more efficient."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (loop with result = 1
        for i of-type fixnum from 0 below (integer-length exponent)
        for sqr = n then (mod (* sqr sqr) modulus)
        when (logbitp i exponent) do
          (setf result (mod (* result sqr) modulus))
        finally (return result)))
