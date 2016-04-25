;;;; math.asd

(asdf:defsystem #:infix-math
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :description "An extensible infix syntax for math in Common Lisp."
  :license "MIT"
  :serial t
  :depends-on (:serapeum)
  :components ((:file "package")
               (:file "symbols")
               (:file "infix-math")))
