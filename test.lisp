(defpackage :infix-math/test
  (:use :cl :infix-math :infix-math/symbols :fiveam :serapeum)
  (:shadow :<*> :choose :√ :? :1/))
(in-package :infix-math/test)

(def-suite infix-math)
(in-suite infix-math)

(define-symbol-macro x 5)

(defun run-tests ()
  (run! 'infix-math))

(test test-simple ()
  (is (= ($ 2 + 2) 4))
  (is (= ($ 1 + 2 * 3)) 7))

(test test-minus
  (is (= ($ 7 - 3) 4))
  (is (= ($ - 3 - 1) -4)))

(test test-paren-descent ()
  "Parentheses should be descended into during parsing."
  (let ((p 1))
    (is (= ($ (tan pi * (p - 1/2)))
           (tan (* pi (- p 1/2)))))))

(test test-left-to-right-association ()
  "Operators at the same level of precedence should associate left to right."
  (is (+ 0.1d0 (+ 0.2d0 0.3d0)) 0.6d0)
  (is (+ (+ 0.1d0 0.2d0) 0.3d0) 0.6000000000000001D0)
  (is ($ 0.1d0 + 0.2d0 + 0.3d0) 0.6000000000000001D0))

(test test-grouping ()
  "Parentheses should be parsed as groupings."
  (is (= ($ 0.1d0 + (0.2d0 + 0.3d0)) 0.6d0)))

(test test-integer-coefficient ()
  "Literal integer coefficients should be parsed."
  (is (= ($ 2x) 10))
  (is (= ($ -2x) -10)))

(test test-coefficient-priority ()
  "Literal coefficients should have very high priority."
  (is (= ($ 2 ^ 2 * x) (* (expt 2 2) x) 20))
  (is (= ($ 2 ^ 2x) (expt 2 (* 2 x)) 1024)))

(test test-coefficient-of-one ()
  "A coefficient of one should be optional."
  (is (= ($ -x)
         ($ -1x)
         (* -1 x))))

(test test-decimal-coefficient ()
  "Coefficients with dots should be parsed as decimals."
  (is (= ($ 1.5x)
         (* 3/2 x))))

(test test-fractional-coefficient ()
  "Fractional coefficients should be parsed."
  (is (= ($ 1/3x) (* 1/3 x))))

(test test-over-priority ()
  "The `over' operator should have lower priority than /."
  (is (= ($ x * 2 / x * 3) (* (/ (* x 2) x) 3) 6))
  (is (= ($ (x * 2) / (x * 3)) (/ (* x 2) (* x 3)) 2/3))
  (is (= ($ x * 2 over x * 3) (/ (* x 2) (* x 3)) 2/3)))

(test test-over-with-dashes ()
  "A run of dashes should be interpreted as `over'."
  (is (= ($ x * 2
            -----
            x * 3)
         2/3)))

(defun <*> (x y)
  "Matrix multiplication, maybe."
  (declare (ignore x y)))

(test test-operator-characters ()
  "A symbol that entirely composed of operator characters should be
interpreted as an infix operator with the highest non-unary priority."
  (is (equal (macroexpand '($ x * y <*> z)) '(* x (<*> y z)))))

(defun choose (n k)
  "Binomial coefficient, maybe."
  (declare (ignore n k)))

(test test-operator-with-dots ()
  "Any function should be usable as an infix operator by surrounding its
name with dots."
  (is (equal (macroexpand '($ n .choose. k)) '(choose n k))))

(declare-unary-operator √)

(test test-chained-unary-operators ()
  "Regression: chained unary operators should parse correctly."
  (is (equal (macroexpand '($ (- √ 5))) '(- (√ 5)))))

(test test-parenthesized-unary-operator-precedence ()
  "Regression: priority should be preserved for parenthesized unary operators."
  (is (equal* (macroexpand '($ √ 5 + 1))
              (macroexpand '($ (√ 5 + 1)))
              '(+ (√ 5) 1))))

(declare-binary-operator <_> :from *)

(declare-binary-operator ?
  :from *
  :right-associative t)

(defun 1/ (x)
  (/ x))

(declare-unary-operator 1/)

(test test-allow-1/-as-unary-operator ()
  "1/ as a unary operator should be parseable."
  (is (= (/ 5) ($ 1/ 5))))

(test test-syntax-error-0
  (signals error (macroexpand '($ foo x))))

(test test-syntax-error-1
  (signals error (macroexpand '($ 1 + foo x))))

(test test-syntax-error-2
  (signals error (macroexpand '($ 1 √ 2))))
