;;;; math.asd

(asdf:defsystem "infix-math"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on ("infix-math/infix-math")
  :description "An extensible infix syntax for math in Common Lisp."
  :in-order-to ((test-op (test-op "infix-math/test")))
  :license "MIT"
  :perform (test-op (o c) (symbol-call :infix-math/test :run-tests)))
