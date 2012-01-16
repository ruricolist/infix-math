;;;; math.asd

(asdf:defsystem #:infix-math
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :description "An extensible infix syntax for math in Common Lisp."
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "infix-math")))
