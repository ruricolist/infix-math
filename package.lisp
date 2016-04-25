;;;; package.lisp

(defpackage :infix-math-symbols
  (:use :cl)
  (:export
   :^
   :! :√
   :× :% :÷
   :<< :>>
   :&))

(defpackage #:infix-math
  (:use #:cl :infix-math-symbols :alexandria :serapeum)
  (:export :$ :declare-operator))
