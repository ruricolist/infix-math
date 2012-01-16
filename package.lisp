;;;; package.lisp

(defpackage #:infix-math
  (:use #:cl)
  (:export #:use-infix-math
           #:set-precedence-from-operator
           #:declare-right-associative
           #:declare-associative
           #:declare-chaining
           #:declare-alias))
