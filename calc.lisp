(cl:defpackage :infix-math/calc
  (:use :cl :infix-math :alexandria :serapeum)
  (:export :calc))

(defpackage :infix-math/calc-user
  (:use :cl :infix-math/symbols)
  (:export :<-))

(cl:in-package :infix-math/calc)

(defconst eof "eof")

(defmacro infix-math/calc-user:<- (var expr)
  `(setq ,var ,expr))

(declare-binary-operator infix-math/calc-user:<-
  ;; TODO Should be lower.
  :from over
  :right-associative t)

(defconst user-pkg :infix-math/calc-user)

(defconst var-names
  (set-difference
   (append
    (loop for code from (char-code #\a) to (char-code #\z)
          collect (code-char code))
    (loop for code from (char-code #\α) to (char-code #\ω)
          collect (code-char code)))
   '(#\e #\t #\π #\ε #\ς)))

(defconst vars
  (mapcar (op (intern (string-upcase _) user-pkg))
          var-names))

(defconst vars/earmuffs
  (mapcar (op (intern (string+ "*" (string-upcase _) "*")
                      user-pkg))
          var-names))

(defmacro define-dynamic-vars ()
  `(progn
     ,@(loop for var in vars
             for var/earmuffs in vars/earmuffs
             collect `(progn
                        (defvar ,var/earmuffs)
                        (define-symbol-macro ,var ,var/earmuffs)))))

(define-dynamic-vars)

(defun eval-quiet (expr)
  (handler-bind ((warning #'muffle-warning))
    (eval expr)))

(defun calc ()
  (let ((*package* (find-package user-pkg)))
    (progv vars/earmuffs ()
      (with-simple-restart (abort "Return to Lisp")
        (loop
           (with-simple-restart (abort "Return to calculator")
             (local
               (format t "~&$> ")
               (def string (read-line))

               (def forms
                 (with-input-from-string (s string)
                   (loop for form = (read s nil eof)
                         until (eq form eof)
                         collect form)))

               (def expr
                 (cond ((null forms) nil)
                       ((single forms)
                        (let ((form (first forms)))
                          (cond
                            ((eql form :q)
                             (return-from calc *))
                            ((eql form :v)
                             (list 'quote vars))
                            (t form))))
                       (t `($ ,@forms))))

               (shiftf +++ ++ + expr)

               (def value
                 (handler-case
                     (eval-quiet expr)
                   (error (e)
                     (format t "~a" e))))

               (shiftf *** ** * value)

               (when value
                 (format t "~&~s~%" value)))))))))
