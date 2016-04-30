;;;; infix-math.lisp

(defpackage #:infix-math/infix-math
  (:nicknames :infix-math)
  (:use #:cl
   :alexandria
   :serapeum
   :infix-math/symbols
   :infix-math/data)
  (:export :$ :declare-operator :over :^))

(in-package #:infix-math)

(defun precedence< (op1 op2)
  (if (right-associative? op1)
      (> (precedence op1) (precedence op2))
      (>= (precedence op1) (precedence op2))))

(defun precedence= (op1 op2)
  (= (precedence op1) (precedence op2)))

(defun make-node (tree operator)
  (let ((operator (trim-dotted-operator operator)))
    (destructuring-bind (x y . rest) tree
      (cons (list operator y x) rest))))

(define-modify-macro nodef (operator) make-node)

(defun shunting-yard (expression &aux tree stack)
  (let ((last-token :start))
    (dolist (token expression)
      (if (operator? token)
          (progn
            (when (and (unary? token)
                       (or (eq last-token :start)
                           (operator? last-token)))
              (push token tree)
              (setf token 'unary))
            (loop while (and stack (precedence< token (car stack)))
                  do (nodef tree (pop stack))
                  finally (push token stack)))
          (push token tree))
      (setf last-token token)))
  (when stack
    (dolist (op stack)
      (nodef tree op)))
  (car tree))

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

(defun parse-expression (expression)
  (when expression
    (shunting-yard expression)))

(defun eliminate-common-subexpressions (form &optional env)
  (declare (ignore env))
  (local
    (def exprs (dict))

    ;; TODO Expand symbol macros, but without shadowing functions
    ;; which are also the names of symbol macros. (Maybe use
    ;; macroexpand-dammit?).
    (defun rec (tree)
      (when (listp tree)
        (let ((count (incf (gethash tree exprs 0))))
          ;; No subexps of subexps.
          (when (and (listp tree) (= count 1))
            (mapcar #'rec tree)))))

    (rec form)

    (def repeats
      (mapcar #'car
              (filter (op (> (cdr _) 1))
                      (hash-table-alist exprs))))

    (def gensyms
      (make-gensym-list (length repeats) (string 'subexp)))

    (if repeats
        `(let ,(mapcar #'list gensyms repeats)
           ,(sublis (mapcar #'cons repeats gensyms) form
                    :test #'equal))
        form)))

(defun expand-expression (exprs)
  (mapcar
   (lambda (expr)
     (cond ((atom expr)
            expr)
           ((operator? (second expr))
            (parse-expression (expand-expression expr)))
           ;; E.g. (- x * y), (gamma x - y)
           ((operator? (third expr))
            (cons (first expr)
                  (~> expr
                      rest
                      expand-expression
                      parse-expression
                      list)))
           (t (expand-expression expr))))
   exprs))

(defun expand-fancy-symbols (form)
  "Expand -x into (- x) and 2x into (* 2 x).
Only integers are allowed as literal coefficients.

Literal coefficients have the same precedence as unary operators.

Literal coefficients are assumed to be in base 10."
  (labels ((expand-symbol (sym)
             (let ((package (symbol-package sym))
                   (str (string sym)))
               (cond ((< (length str) 2) sym)
                     ;; Replace a series of dashes or underscores with
                     ;; `over'.
                     ((or (every (op (eql #\- _)) str)
                          (every (op (eql #\_ _)) str))
                      'over)
                     (t (multiple-value-bind (leading-int end)
                            (parse-integer str :junk-allowed t :radix 10)
                          (cond (leading-int
                                 (let* ((name (subseq str end))
                                        (sym2 (intern name package)))
                                   `(* ,leading-int ,sym2)))
                                ((string^= "-" str)
                                 (let* ((name (subseq str 1))
                                        (sym2 (intern name package)))
                                   `(- ,sym2)))
                                ((string^= "!" str)
                                 (let* ((name (subseq str 1))
                                        (sym2 (intern name package)))
                                   `(! ,sym2)))
                                ((string^= "√" str)
                                 (let* ((name (subseq str 1))
                                        (sym2 (intern name package)))
                                   `(sqrt ,sym2)))
                                (t sym)))))))
           (rec (form)
             (if (atom form)
                 (if (symbolp form)
                     (expand-symbol form)
                     form)
                 (cons (rec (car form))
                       (rec (cdr form))))))
    (rec form)))

(defmacro $ (&rest form &environment env)
  "Compile an infix math expression, recursively.

One notable feature: handles expressions like (exp 2 - 1), so
it (almost) never needs more parentheses that would be required in an
infix language. (The exception is a leading unary negation: that still
requires parentheses.)

Another notable feature: you can negate a variable by writing it with a leading dash:

     ($ -x) => (- x)

You can also write a variable with a literal coefficient:

     ($ 2x) => (* 2 x)

Note that literal coefficients have higher priority than binary operations:

     (let ((x 3)) ($ 2 ^ 2x)) => 64"
  (~> form
      expand-fancy-symbols
      expand-expression
      parse-expression
      (eliminate-common-subexpressions env)))
