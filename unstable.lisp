(defpackage :infix-math/unstable
  (:use :cl :infix-math/contractions :optima)
  (:import-from :serapeum :~>>)
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
    `(guard ,it (cl:= ,it ,x))))

(defun rewrite (form)
  "If FORM is recognized as a numerically unstable expression, rewrite
it."
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

    (otherwise form)))

(defun replace-aliases (form)
  (~>> form
       copy-tree
       (nsubst 'expt '^)
       (nsubst '/ 'over)))

(defun rewrite-unstable-expressions (form)
  (labels ((rec (form)
             (let ((rewrite (rewrite form)))
               (if (eql rewrite form)
                   (if (listp form)
                       (mapcar #'rec form)
                       form)
                   (mapcar #'rec rewrite)))))
    (rec (replace-aliases form))))
