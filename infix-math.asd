;;;; math.asd

(asdf:defsystem :infix-math
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :description "An extensible infix syntax for math in Common Lisp."
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:infix-math/infix-math))
