(defpackage :infix-math/unstable
  (:use :cl :floating-point-contractions :optima)
  (:import-from :serapeum :~>> :expand-macro)
  (:import-from :infix-math/symbols :^ :over)
  (:import-from :fare-quasiquote)
  (:import-from :named-readtables :in-readtable)
  (:shadow :=)
  (:export :rewrite-unstable-expressions))

(in-package :infix-math/unstable)

(in-readtable :fare-quasiquote)

(declaim (inline =))

(defun = (&rest xs)
  (apply #'cl:= xs))

(defpattern = (x)
  (let ((it (gensym (string 'it))))
    `(guard ,it (and (numberp ,it)
                     (cl:= ,it ,x)))))

;;; http://www.cliki.net/EXPT-MOD
(defun expt-mod (n exponent modulus)
  "As (mod (expt n exponent) modulus), but more efficient."
  (loop with result = 1
        for i of-type fixnum from 0 below (integer-length exponent)
        for sqr = n then (mod (* sqr sqr) modulus)
        when (logbitp i exponent) do
          (setf result (mod (* result sqr) modulus))
        finally (return result)))

(defun rewrite (form &aux (orig form))
  "If FORM is recognized as a numerically unstable expression, rewrite
it."
  (setf form (expand-macro form))
  (match form
    ((and x (type (not list))) x)

    ;; Hypotenuse.
    (`(sqrt (expt ,a ,(= 2)) (expt ,b ,(= 2)))
      `(hypot ,a ,b))

    ;; Modular exponentiation.
    (`(mod (expt ,a ,b) ,m)
      `(expt-mod ,a ,b ,m))

    ;; Log of exp.
    (`(log (- ,(= 1) (exp ,x)))
      `(log1-exp ,x))
    (`(log (+ ,(= 1) (exp ,x)))
      `(log1+exp ,x))

    (`(log (+ (- (- (exp ,x) ,(= 1)))
              ,(= 1)))
      `(log2-exp ,x))

    ;; log1p, log1m
    (`(log (+ ,(= 1) ,x))
      `(log1+ ,x))
    (`(log (- ,(= 1) ,x))
      `(log1- ,x))


    (`(/ (log (+ ,(= 1) ,x1)) ,x2)
      (unless (cl:= x1 x2)
        (fail))
      `(log1+/x ,x1))

    ;; expm1
    (`(- (exp ,x) ,(= 1))
      `(exp-1 ,x))
    (`(/ (- (exp ,x1) ,(= 1)) ,x2)
      (unless (= x1 x2)
        (fail))
      `(exp-1/x ,x1))

    (`(- (expt ,a ,b) ,(= 1))
      `(expt-1 ,a ,b))

    ;; Some Lisps return log as a single float by default.
    (`(log ,x) `(ln ,x))
    ;; TODO More stable?
    (`(expt ,x ,(= 2)) `(* ,x ,x))

    ;; More from http://www.plunk.org/~hatch/rightway.php.

    ;; Do you need an infix-math library to write an infix-math
    ;; library?

    ;; sqrt(1+x)-1
    (`(- (sqrt (+ ,(= 1)
                  ,x))
         ,(= 1))
      `(/ ,x (+ (sqrt (+ 1 ,x)) 1)))
    ;; 1-sqrt(1-x)
    (`(- ,(= 1)
         (sqrt (- ,(= 1)
                  ,x)))
      `(/ ,x (sqrt (+ (- 1 ,x) 1))))
    ;; (1+x)^2-1
    (`(- (expt (+ ,(= 1) ,x)
               ,(= 2))
         ,(= 1))
      `(* ,x (+ 2 ,x)))
    ;; 1-(1-x)^2
    (`(- ,(= 1)
         (expt (- ,(= 1) ,x)
               ,(= 2)))
      `(* ,x (- 2 ,x)))
    ;; Return the original form for readability's sake.
    (otherwise orig)))

(defun rewrite-unstable-expressions (form)
  (let ((rewrite (rewrite form)))
    (if (eql rewrite form)
        (if (listp form)
            (mapcar #'rewrite-unstable-expressions form)
            form)
        (mapcar #'rewrite-unstable-expressions rewrite))))
