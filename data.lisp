(defpackage :infix-math/data
  (:use
   :cl
   :alexandria
   :serapeum
   :infix-math/symbols)
  (:export
   :unary-negation
   :operator?
   :trim-dotted-operator
   :precedence
   :associative?
   :unary?
   :variadic?
   :right-associative?
   :declare-operator))

(in-package :infix-math/data)

(deftype operator ()
  'symbol)

(deftype precedence ()
  '(or (real 0 *) null))

(defmacro unary-negation (x y)
  "Dummy macro for compiling unary negation."
  (declare (ignore x))
  `(- ,y))

(defparameter *order-of-operations*
  '((unary-negation)
    (expt ^ log)
    (* × / % ÷ rem mod
     floor ffloor
     ceiling fceiling
     truncate ftruncate
     round fround
     scale-float
     gcd lcm atan)
    (+ -)
    (ash << >>)
    (logand & logandc1 logandc2 lognand)
    (logxor logeqv)
    (logior logorc1 logorc2 lognor)
    (min max)
    (over))
  "Basic C-style operator precedence, with some differences.

   The use of MIN, MAX, GCD and LCM as infix operators is after
   Dijkstra (see EWD 1300). Perl 6 is also supposed to use them this
   way, and I have adopted its precedence levels.")

(defparameter *variadic*
  '(+ * × / gcd lcm max min logand logxor logeqv logior min max)
  "Built-in functions that take variable-length argument lists.

   N.B. (+ x y z) ≢ (reduce #'+ x y z). Reduce always works
   left-to-right, but in (+ x y z) the order of evaluation is not
   guaranteed: the implementation may use any of several strategies,
   with unpredictable results when the operands are of varying
   precision (see 12.1.1.1.1). Preserving this behavior falls under
   least surprise.")

(defparameter *associative*
  '(+ * × gcd lcm max min logand logxor logeqv logior)
  "Associative operators.")

(defparameter *right-associative*
  '(expt ^ $$))

(defparameter *precedence*
  (alexandria:alist-hash-table
   (loop for i from 0
         for level in *order-of-operations*
         append (loop for op in level
                      collect (cons op i))))
  "Table of operator precedence.")

(defun operator-char? (c)
  (not (or (alpha-char-p c)
           (find c "-_"))))

(defun dotted-operator? (sym)
  (let ((s (string sym)))
    (and (> (length s) 2)
         (let ((first-char (aref s 0))
               (last-char (aref s (1- (length s)))))
           (and (eql #\. first-char)
                (eql #\. last-char))))))

(defun looks-like-operator? (sym)
  "Does SYM start and end with an operator char?"
  (let ((s (string sym)))
    (or (and (> (length s) 0)
             (every #'operator-char? s))
        (dotted-operator? sym))))

(defun operator? (operator)
  (and (symbolp operator)
       (precedence operator)))

(defun trim-dotted-operator (operator)
  (unless (operator? operator)
    (error "Not an operator: ~a." operator))
  (if (dotted-operator? operator)
      (intern (slice (string operator) 1 -1)
              (symbol-package operator))
      operator))

(defun precedence (operator)
  (or (gethash (assure operator operator) *precedence*)
      (and (looks-like-operator? operator)
           ;; highest non-unary precedence
           1)))

(defun (setf precedence) (value operator)
  (setf (gethash (assure operator operator) *precedence*)
        (assure precedence value)))

(defun save-operator (&key name precedence associative right-associative)
  (setf (precedence name) precedence
        (associative? name) associative
        (right-associative? name) right-associative)
  name)

(defun copy-operator (from to)
  (save-operator :name to
                 :precedence (precedence from)
                 :right-associative (right-associative? from)
                 :associative (associative? from)))

(defmacro declare-operator (new &key from precedence associative right-associative)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(if from
           `(copy-operator ',from ',new)
           `(save-operator
             :name ',new
             :precedence ,precedence
             :associative ,associative
             :right-associative ,right-associative))))

(defun variadic? (operator)
  (member operator *variadic*))

(defun (setf variadic?) (value operator)
  (if value
      (pushnew (assure operator operator) *variadic*)
      (removef *variadic* operator)))

(defun associative? (operator)
  (member operator *associative*))

(defun (setf associative?) (value operator)
  (if value
      (pushnew (assure operator operator) *associative*)
      (removef *associative* operator)))

(defun right-associative? (operator)
  (member operator *right-associative*))

(defun (setf right-associative?) (value operator)
  (if value
      (pushnew (assure operator operator) *right-associative*)
      (removef *right-associative* operator)))

(defun unary? (operator)
  (eql operator '-))
