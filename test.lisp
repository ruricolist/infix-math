(defpackage :infix-math/test
  (:use :cl :infix-math :infix-math/symbols :fiveam))
(in-package :infix-math/test)

(def-suite infix-math)
(in-suite infix-math)

(defun run-tests ()
  (run! 'infix-math))
