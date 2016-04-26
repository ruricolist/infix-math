(defpackage :infix-math/symbols
  (:use :cl)
  (:export
   :^
   :! :√
   :× :% :÷
   :<< :>>
   :&
   :over))

(in-package :infix-math-symbols)

(defmacro unary-operator (new old)
  `(progn
     (declaim (inline ,new))
     (defun ,new (a)
       (,old a))))

(defmacro binary-operator (new old)
  `(progn
     (declaim (inline ,new))
     (defun ,new (a b)
       (,old a b))))

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
