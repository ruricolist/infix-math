(defpackage :infix-math/symbols
  (:use :cl)
  #+sbcl :lock #+sbcl t
  (:export
   :^
   :! :√
   :× :% :÷
   :<< :>>
   :&
   :over
   :π))

(in-package :infix-math/symbols)

(define-symbol-macro π pi)

(define-symbol-macro e (exp 1d0))

(define-symbol-macro i (sqrt -1))

(defmacro unary-operator (new old)
  `(progn
     (declaim (inline ,new))
     (defun ,new (a)
       (,old a))
     (define-compiler-macro ,new (a)
       (list ',old a))))

(defmacro binary-operator (new old)
  `(progn
     (declaim (inline ,new))
     (defun ,new (a b)
       (,old a b))
     (define-compiler-macro ,new (a b)
       (list ',old a b))))

(defmacro unary-operators (&body body)
  `(progn
     ,@(loop for (new old . nil) on body by #'cddr
             collect `(unary-operator ,new ,old))))

(defmacro binary-operators (&body body)
  `(progn
     ,@(loop for (new old . nil) on body by #'cddr
             collect `(binary-operator ,new ,old))))



(declaim (inline ash-))
(defun ash- (i c)
  (ash i (- c)))



(unary-operators
  ;; ! factorial
  √ sqrt)

(binary-operators
  ^ expt
  × *
  ÷ rem
  % rem
  << ash
  >> ash-
  & logand
  over /)
