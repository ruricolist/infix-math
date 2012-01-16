;;;; infix-math.lisp

(in-package #:infix-math)

(defparameter *basic-order-of-operations*
  '((expt)
    (* / rem mod
     floor ffloor
     ceiling fceiling
     truncate ftruncate
     round fround
     scale-float
     gcd lcm atan)
    (+ -)
    (ash)
    (logand logandc1 logandc2 lognand)
    (logxor logeqv)
    (logior logorc1 logorc2 lognor)
    (min max)
    (bit-and bit-andc1 bit-andc2 bit-nand)
    (bit-xor bit-eqv)
    (bit-ior bit-orc1 bit-orc2 bit-nor)
    (intersection set-difference)
    (set-exclusive-or)
    (union)
    (< <= > >= = =/ subsetp)
    (and)
    (or)
    (incf decf)
    ;; Equivalent to C's , operator.
    (progn))
  "Basic C-style operator precedence, with some differences.

   The use of MIN, MAX, GCD and LCM as infix operators is after
   Dijkstra (see EWD 1300). Perl 6 is also supposed to use them this
   way, and I have adopted its precedence levels.

   Bitwise operators have a higher precedence than predicates. See
   <http://cm.bell-labs.com/cm/cs/who/dmr/chist.html>, where Dennis
   Ritchie considers C's ordering infelicitous.

   All predicates share the same level of precedence (as in Python) to
   facilitate chaining.")

(loop for i from 0
      for level in *basic-order-of-operations*
      do (dolist (op level)
           (setf (get op 'level) i)))

(defun alias (operator)
  (declare (type symbol operator))
  (or (get operator 'alias) operator))

(defun (setf alias) (value operator)
  (declare (type symbol value operator))
  (setf (get operator 'alias)
        ;; Prevent recursive aliases.
        (alias value)))

(defmacro declare-alias (from to)
  "Declare FROM as an alias for TO. Replace FROM with TO whenever it
occurs in an infix expression.

Note that declaring an alias is not sufficient to make TO a valid
operator; only SET-PRECEDENCE-FROM-OPERATOR can do that."
  `(setf (alias ',from) ',to))

(defun operator? (operator)
  (when (symbolp operator)
    (get (alias operator) 'level)))

(defun built-in? (operator)
  (declare (type symbol operator))
  (get (alias operator) 'built-in))

(defun precedence (operator)
  (declare (type symbol operator))
  (get (alias operator) 'level))

(defun (setf precedence) (precedence operator)
  (declare (type symbol operator)
           (type integer precedence))
  (setf (get (alias operator) 'level) precedence))

(defun set-precedence-from-operator (to-operator from-operator)
  "Make TO-OPERATOR an operator at the same level of precedence as
FROM-OPERATOR."
  (declare (type symbol to-operator from-operator))
  (setf (precedence to-operator) (precedence from-operator))
  to-operator)

(defun precedence< (op1 op2)
  (declare (type symbol op1 op2))
  (if (right-associative? op1)
      (> (precedence op1) (precedence op2))
      (>= (precedence op1) (precedence op2))))

(defun precedence= (op1 op2)
  (declare (type symbol op1 op2))
  (= (precedence op1) (precedence op2)))

(defun chaining? (operator)
  (get (alias operator) 'chaining))

(defun (setf chaining?) (value operator)
  (declare (type boolean value)
           (type symbol operator))
  (setf (get (alias operator) 'chaining) value))

(defmacro declare-chaining (&rest operators)
  "Declare OPERATORS to be chaining predicates, like < and =.

Each operator should be a predicate of two arguments. The order of the
arguments is important: if the predicate returns non-nil, it is the
the second argument that is passed to the next link in the chain.

It is not required but probably wise to give all predicates the same
precedence, since operators only chain within levels."
  (let ((op (gensym)))
    `(dolist (,op ',operators)
       (setf (chaining? ,op) t))))

(declare-chaining > >= < <= = /= subsetp
                  eq eql equal equalp)

(defparameter *variadic*
  '(+ * gcd lcm max min logand logxor logeqv logior and or progn)
  "Built-in functions that take variable-length argument lists.

   N.B. (+ x y z) does not imply (reduce #'+ x y z). Reduce always
   works left-to-right, but in (+ x y z) the order of evaluation is
   not guaranteed: the implementation may use any of several
   strategies, with unpredictable results when the operands are of
   varying precision (see 12.1.1.1.1). Preserving this behavior falls
   under least surprise.")

(defun variadic? (operator)
  (declare (type symbol operator))
  (member (alias operator) *variadic*))

(defun associative? (operator)
  (declare (type symbol operator))
  (or (get (alias operator) 'associative)
      ;; Chaining operators are trivially associative.
      (chaining? operator)))

(defun (setf associative?) (value operator)
  (declare (type boolean value)
           (type symbol operator))
  (setf (get (alias operator) 'associative) value))

(defmacro declare-associative (&rest operators)
  "Declare OPERATORS to have the associative property (like addition
and multiplication). Not to be confused with right-associatvity and
DECLARE-RIGHT-ASSOCIATIVE."
  (let ((op (gensym)))
    `(dolist (,op ',operators)
       (setf (associative? ,op) t))))

(declare-associative + * gcd lcm max min
                     logand logxor logeqv logior
                     bit-and bit-xor bit-eqv bit-ior
                     intersection set-exclusive-or union
                     and or progn)

(defun right-associative? (operator)
  (get (alias operator) 'right-associative))

(defun (setf right-associative?) (value operator)
  (declare (type boolean value)
           (type symbol operator))
  (setf (get (alias operator) 'right-associative) value))

(defmacro declare-right-associative (&rest operators)
  "Declare OPERATORS to be right associative (like exponentiation and
assignment). Not to be confused with DECLARE-ASSOCIATIVE and the
associative property."
  (let ((op (gensym)))
    `(dolist (,op ',operators)
       (setf (right-associative? ,op) t))))

(declare-right-associative expt incf decf setf rotatef)

(defun valid? (expression)
  ;; Test in ascending order of expense.
  (and (consp expression)
       (cdr expression)
       (oddp (length expression))
       (loop for i from 0
             for elt in expression
             always (if (oddp i)
                        (operator? elt)
                        (not (operator? elt))))))

(defmacro delay (exp)
  (let ((value (gensym)))
    `(let (,value)
       (lambda ()
         (or ,value (setf ,value ,exp))))))

(defun force (thunk)
  (funcall thunk))

(defmacro link (op &rest args)
  `(loop with list = (list ,@(loop for arg in args
                                   collect `(delay ,arg)))
         for x in list
         for y in (cdr list)
         always (and (force x)
                     (force y)
                     (funcall ,op (force x) (force y)))
         finally (return (force y))))

(defmacro chain (op &rest args)
  `(not (null (link ,op ,@args))))

;;; It's tempting to do additional transformations here, like
;;; short-circuiting multplication by 0, but that's best left to the
;;; compiler.

(defun transform (tree &key chain)
  (if (atom tree)
      tree
      (destructuring-bind (op . args) tree
        (or (when (eql 'chain op)
              ;; The tree has already been transformed. At the moment,
              ;; [x < [y < z]] gives an error; would treating it the
              ;; same as [x < y < z] be better?
              tree)
            (or (when (and (operator? op) (associative? op))
                  (or (when (chaining? op)
                        (list* (if chain 'link 'chain)
                               `(function ,op)
                               (transform args :chain t)))
                      ;; If an operator is associative, but not known
                      ;; to be variadic, use REDUCE. It is not
                      ;; strictly necessary, but it makes the
                      ;; expansion more readable.
                      (when (and (not (variadic? op))
                                 ;; More than two args.
                                 (cddr args))
                        (list* 'reduce
                               `(function ,op)
                               (transform args :chain chain)
                               (when (right-associative? op)
                                 '(:from-end t))))))
                (cons (transform (car tree) :chain chain)
                      (transform (cdr tree) :chain chain)))))))

(defun flatten-associative-ops (node)
  (destructuring-bind (op . children) node
    (if (associative? op)
        (cons op (loop for child in children
                       if (and (consp child)
                               (eql op (car child)))
                         append (cdr child)
                       else collect child))
        node)))

(defun make-node (tree operator)
  (destructuring-bind (x y . rest) tree
    (cons (flatten-associative-ops
           (list (alias operator) y x))
          rest)))

(define-modify-macro nodef (operator) make-node)

(defun shunting-yard (expression &aux tree stack)
  (assert (valid? expression)
          (expression)
          "~S is not a valid infix expression"
          expression)
  (dolist (token expression)
    (if (operator? token)
        (loop while (and stack (precedence< token (car stack)))
              do (nodef tree (pop stack))
              finally (push token stack))
        (push token tree)))
  (when stack
    (dolist (op stack)
      (nodef tree op)))
  (flatten-associative-ops (car tree)))

(defun parse-expression (expression)
  (when expression
    (transform (shunting-yard expression))))

(defun make-bracket-reader (close)
  (lambda (stream char)
    (declare (ignore char))
    (parse-expression (read-delimited-list close stream t))))

(defun extend-readtable (readtable open close)
  (set-macro-character open (make-bracket-reader close) nil readtable)
  (set-macro-character close (get-macro-character #\)) nil readtable)
  readtable)

(defun use-infix-math (&optional open close)
  "Extend the current readtable to use OPEN and CLOSE as delimiters
for infix expressions. Default to square brackets."
  (extend-readtable *readtable* (or open #\[) (or close #\])))
