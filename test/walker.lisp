;;;; walker.lisp
;;;;
;;;; Copyright 2021 Alexander Gutev <alex.gutev@mail.bg>
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

;;;; Unit tests for BLOCK form types

(defpackage :cl-form-types/test.walker
  (:use :cl-environments-cl
	:cl-form-types.walker
	:alexandria

	:fiveam
	:cl-form-types/test))

(in-package :cl-form-types/test.walker)


;;; Test Suite Definition

(def-suite walker
    :description "Test code-walker."
    :in cl-form-types)

(in-suite walker)


;;; Utilities

(defmacro is-walk ((form &optional (result form)) &body expected)
  (with-gensyms (forms fn form-var env-var)
    `(let ((,forms))
       (flet ((,fn (,form-var ,env-var)
                (declare (ignore ,env-var))

                (push ,form-var ,forms)
                ,form-var))
         (is (equal ',result (walk-form #',fn ',form nil)))

         (is (equal ',expected (nreverse ,forms)))))))


;;; Definitions used by tests

(defmacro pass-form (form)
  form)

(define-symbol-macro a-symbol-macro some-symbol)


;;; Test Basic Forms

(test walk-atom
  "Test walking ATOM forms"

  (is-walk (x) x)
  (is-walk (1) 1)
  (is-walk ("hello") "hello")

  (is-walk (a-symbol-macro some-symbol)
    a-symbol-macro
    some-symbol))

(test walk-function-call
  "Test walking function call expression"

  (is-walk ((f x (g y 1)))
    (f x (g y 1))
    x
    (g y 1)
    y
    1))

(test walk-macro-forms
  "Test walking function call expression"

  (is-walk ((pass-form (f x (pass-form y)))
            (f x y))
    (pass-form (f x (pass-form y)))
    (f x (pass-form y))
    x
    (pass-form y)
    y))


;;; Test Simple Special-forms

(test walk-quote
  "Test walking QUOTE forms"

  (is-walk ('a-symbol-macro)
    'a-symbol-macro)

  (is-walk ('(pass-form (f x)))
    '(pass-form (f x))))

(test walk-go
  "Test walking GO forms"

  (is-walk ((go a-symbol-macro))
    (go a-symbol-macro)))

(test walk-throw
  "Test walking THROW forms"

  (is-walk ((throw a-symbol-macro) (throw some-symbol))
    (throw a-symbol-macro)
    a-symbol-macro
    some-symbol))

(test walk-return-from
  "Test walking RETURN-FROM forms"

  (is-walk ((return-from a-symbol-macro (pass-form (f x)))
            (return-from a-symbol-macro (f x)))

    (return-from a-symbol-macro (pass-form (f x)))
    (pass-form (f x))
    (f x)
    x))

(test walk-the
  "Test walking THE forms"

  (is-walk ((the type (pass-form (f x)))
            (the type (f x)))

    (the type (pass-form (f x)))
    (pass-form (f x))
    (f x)
    x))

(test walk-if
  "Test walking IF forms"

  (is-walk ((if (f a-symbol-macro) (pass-form (g x)))
            (if (f some-symbol) (g x)))

    (if (f a-symbol-macro) (pass-form (g x)))

    (f a-symbol-macro)
    a-symbol-macro
    some-symbol

    (pass-form (g x))
    (g x)
    x)

  (is-walk ((if (f a-symbol-macro) (pass-form (g x)) (n y))
            (if (f some-symbol) (g x) (n y)))

    (if (f a-symbol-macro) (pass-form (g x)) (n y))

    (f a-symbol-macro)
    a-symbol-macro
    some-symbol

    (pass-form (g x))
    (g x)
    x

    (n y)
    y))

(test walk-multiple-value-call
  "Test walking MULTIPLE-VALUE-CALL forms"

  (is-walk ((multiple-value-call fn a-symbol-macro (f (pass-form a)))
            (multiple-value-call fn some-symbol (f a)))

    (multiple-value-call fn a-symbol-macro (f (pass-form a)))
    fn

    a-symbol-macro
    some-symbol

    (f (pass-form a))
    (pass-form a)
    a))


;;; Test Grouping Forms

(test walk-block
  "Test walking BLOCK forms"

  (is-walk
      ((block name
         (f (g x) a-symbol-macro)
         (pass-form (n (m (pass-form a)))))

       (block name
         (f (g x) some-symbol)
         (n (m a))))

    (block name
      (f (g x) a-symbol-macro)
      (pass-form (n (m (pass-form a)))))

    (f (g x) a-symbol-macro)
    (g x)
    x
    a-symbol-macro
    some-symbol

    (pass-form (n (m (pass-form a))))
    (n (m (pass-form a)))
    (m (pass-form a))
    (pass-form a)
    a))

(test walk-catch
  "Test walking CATCH forms"

  (is-walk
      ((catch 'tag
         (f (g x) a-symbol-macro)
         (pass-form (n (m (pass-form a)))))

       (catch 'tag
         (f (g x) some-symbol)
         (n (m a))))

    (catch 'tag
      (f (g x) a-symbol-macro)
      (pass-form (n (m (pass-form a)))))

    'tag
    (f (g x) a-symbol-macro)
    (g x)
    x
    a-symbol-macro
    some-symbol

    (pass-form (n (m (pass-form a))))
    (n (m (pass-form a)))
    (m (pass-form a))
    (pass-form a)
    a))

(test walk-eval-when
  "Test walking EVAL-WHEN forms"

  (is-walk
      ((eval-when (:compile-toplevel)
         (f (g x) a-symbol-macro)
         (pass-form (n (m (pass-form a)))))

       (eval-when (:compile-toplevel)
         (f (g x) some-symbol)
         (n (m a))))

    (eval-when (:compile-toplevel)
         (f (g x) a-symbol-macro)
         (pass-form (n (m (pass-form a)))))

    (f (g x) a-symbol-macro)
    (g x)
    x
    a-symbol-macro
    some-symbol

    (pass-form (n (m (pass-form a))))
    (n (m (pass-form a)))
    (m (pass-form a))
    (pass-form a)
    a))

(test walk-progn
  "Test walking PROGN forms"

  (is-walk
      ((progn
         (f (g x) a-symbol-macro)
         (pass-form (n (m (pass-form a)))))

       (progn
         (f (g x) some-symbol)
         (n (m a))))

    (progn
      (f (g x) a-symbol-macro)
      (pass-form (n (m (pass-form a)))))

    (f (g x) a-symbol-macro)
    (g x)
    x
    a-symbol-macro
    some-symbol

    (pass-form (n (m (pass-form a))))
    (n (m (pass-form a)))
    (m (pass-form a))
    (pass-form a)
    a))

(test walk-multiple-value-prog1
  "Test walking MULTIPLE-VALUE-PROG1 forms"

  (is-walk
      ((multiple-value-prog1 (pass-form (get var))
         (f (g x) a-symbol-macro)
         (pass-form (n (m (pass-form a)))))

       (multiple-value-prog1 (get var)
         (f (g x) some-symbol)
         (n (m a))))

    (multiple-value-prog1 (pass-form (get var))
      (f (g x) a-symbol-macro)
      (pass-form (n (m (pass-form a)))))

    (pass-form (get var))
    (get var)
    var

    (f (g x) a-symbol-macro)
    (g x)
    x
    a-symbol-macro
    some-symbol

    (pass-form (n (m (pass-form a))))
    (n (m (pass-form a)))
    (m (pass-form a))
    (pass-form a)
    a))

(test walk-progv
  "Test walking PROGV forms"

  (is-walk
      ((progv '(x y z) (list 1 2 3)
         (f (g x) a-symbol-macro)
         (pass-form (n (m (pass-form a)))))

       (progv '(x y z) (list 1 2 3)
         (f (g x) some-symbol)
         (n (m a))))

    (progv '(x y z) (list 1 2 3)
      (f (g x) a-symbol-macro)
      (pass-form (n (m (pass-form a)))))

    '(x y z)
    (list 1 2 3)
    1
    2
    3

    (f (g x) a-symbol-macro)
    (g x)
    x
    a-symbol-macro
    some-symbol

    (pass-form (n (m (pass-form a))))
    (n (m (pass-form a)))
    (m (pass-form a))
    (pass-form a)
    a))

(test walk-tagbody
  "Test walking TAGBODY forms"

  (is-walk
      ((tagbody start
          (f (g x) a-symbol-macro)
          (pass-form tag2)
        a-symbol-macro
          (pass-form (n (m (pass-form a))))
        end)

       (tagbody start
          (f (g x) some-symbol)
        tag2
        a-symbol-macro
          (n (m a))
        end))

    (tagbody start
       (f (g x) a-symbol-macro)
       (pass-form tag2)
     a-symbol-macro
       (pass-form (n (m (pass-form a))))
     end)

    (f (g x) a-symbol-macro)
    (g x)
    x
    a-symbol-macro
    some-symbol

    (pass-form tag2)
    tag2

    (pass-form (n (m (pass-form a))))
    (n (m (pass-form a)))
    (m (pass-form a))
    (pass-form a)
    a))

(test walk-unwind-protect
  "Test walking UNWIND-PROTECT forms"

  (is-walk
      ((unwind-protect (pprint a-symbol-macro)
         (f (g x) a-symbol-macro)
         (pass-form (n (m (pass-form a)))))

       (unwind-protect (pprint some-symbol)
         (f (g x) some-symbol)
         (n (m a))))

    (unwind-protect (pprint a-symbol-macro)
      (f (g x) a-symbol-macro)
      (pass-form (n (m (pass-form a)))))

    (pprint a-symbol-macro)
    a-symbol-macro
    some-symbol

    (f (g x) a-symbol-macro)
    (g x)
    x
    a-symbol-macro
    some-symbol

    (pass-form (n (m (pass-form a))))
    (n (m (pass-form a)))
    (m (pass-form a))
    (pass-form a)
    a))


;;; Test FUNCTION Forms

(test walk-function
  "Test walking FUNCTION forms"

  (is-walk (#'a-symbol-macro)
    #'a-symbol-macro)

  (is-walk (#'(setf something))
    #'(setf something)))

(test walk-function-lambda
  "Test walking FUNCTION form with LAMBDA expression"

  (is-walk
      (#'(cl:lambda (a b c a-symbol-macro)
           (f (pass-form a) b)
           (g c 1 a-symbol-macro))

         #'(cl:lambda (a b c a-symbol-macro)
             (f a b)
             (g c 1 a-symbol-macro)))

    #'(cl:lambda (a b c a-symbol-macro)
        (f (pass-form a) b)
        (g c 1 a-symbol-macro))

    (f (pass-form a) b)
    (pass-form a)
    a
    b

    (g c 1 a-symbol-macro)
    c
    1
    a-symbol-macro))

(test walk-function-lambda-optional-key
  "Test walking FUNCTION form with LAMBDA expression with optional and keyword arguments"

  (is-walk
      (#'(cl:lambda (a b &optional c (d (1+ a-symbol-macro) d-sp) &key (e 2) ((:keyf f)))
           (f (pass-form a) b)
           (g c d e f))

         #'(cl:lambda (a b &optional (c nil) (d (1+ some-symbol) d-sp) &key ((:e e) 2) ((:keyf f) nil))
             (f a b)
             (g c d e f)))

    #'(cl:lambda (a b &optional c (d (1+ a-symbol-macro) d-sp) &key (e 2) ((:keyf f)))
        (f (pass-form a) b)
        (g c d e f))

    nil
    (1+ a-symbol-macro)
    a-symbol-macro
    some-symbol

    2
    nil

    (f (pass-form a) b)
    (pass-form a)
    a
    b

    (g c d e f)
    c
    d
    e
    f))

(test walk-function-lambda-rest-aux
  "Test walking FUNCTION form with LAMBDA expression with rest and auxiliary parameters"

  (is-walk
      (#'(cl:lambda (a b &rest rst &key a-symbol-macro &allow-other-keys &aux (x 1) (y (1+ a-symbol-macro)))
           (f (pass-form a) b)
           (pprint rst)
           (g (+ x y) a-symbol-macro))

         #'(cl:lambda (a b &rest rst &key ((:a-symbol-macro a-symbol-macro) nil) &allow-other-keys &aux (x 1) (y (1+ a-symbol-macro)))
             (f a b)
             (pprint rst)
             (g (+ x y) a-symbol-macro)))

    #'(cl:lambda (a b &rest rst &key a-symbol-macro &allow-other-keys &aux (x 1) (y (1+ a-symbol-macro)))
        (f (pass-form a) b)
        (pprint rst)
        (g (+ x y) a-symbol-macro))

    nil
    1
    (1+ a-symbol-macro)
    a-symbol-macro

    (f (pass-form a) b)
    (pass-form a)
    a
    b

    (pprint rst)
    rst

    (g (+ x y) a-symbol-macro)
    (+ x y)
    x y
    a-symbol-macro))

(test walk-lambda
  "Test walking expression with LAMBDA operator"

  (is-walk
      (((cl:lambda (a b c a-symbol-macro)
          (f (pass-form a) b)
          (g c 1 a-symbol-macro))

        1
        a-symbol-macro
        2
        3)

       ((cl:lambda (a b c a-symbol-macro)
          (f a b)
          (g c 1 a-symbol-macro))

        1
        some-symbol
        2
        3))

    ((cl:lambda (a b c a-symbol-macro)
       (f (pass-form a) b)
       (g c 1 a-symbol-macro))

     1
     a-symbol-macro
     2
     3)

    (f (pass-form a) b)
    (pass-form a)
    a
    b

    (g c 1 a-symbol-macro)
    c
    1
    a-symbol-macro

    1
    a-symbol-macro some-symbol
    2 3))

(test walk-function-lambda-optional-key
  "Test walking expression with LAMBDA operator with optional and keyword arguments"

  (is-walk
      (((cl:lambda (a b &optional c (d (1+ a-symbol-macro) d-sp) &key (e 2) ((:keyf f)))
          (f (pass-form a) b)
          (g c d e f))

        1 2 a-symbol-macro)

       ((cl:lambda (a b &optional (c nil) (d (1+ some-symbol) d-sp) &key ((:e e) 2) ((:keyf f) nil))
          (f a b)
          (g c d e f))

        1 2 some-symbol))

    ((cl:lambda (a b &optional c (d (1+ a-symbol-macro) d-sp) &key (e 2) ((:keyf f)))
       (f (pass-form a) b)
       (g c d e f))

     1 2 a-symbol-macro)

    nil
    (1+ a-symbol-macro)
    a-symbol-macro
    some-symbol

    2
    nil

    (f (pass-form a) b)
    (pass-form a)
    a
    b

    (g c d e f)
    c d e f

    1 2
    a-symbol-macro
    some-symbol))

(test walk-function-lambda-rest-aux
  "Test walking FUNCTION form with LAMBDA expression with rest and auxiliary parameters"

  (is-walk
      (((cl:lambda (a b &rest rst &key a-symbol-macro &allow-other-keys &aux (x 1) (y (1+ a-symbol-macro)))
          (f (pass-form a) b)
          (pprint rst)
          (g (+ x y) a-symbol-macro))

        a a-symbol-macro)

       ((cl:lambda (a b &rest rst &key ((:a-symbol-macro a-symbol-macro) nil) &allow-other-keys &aux (x 1) (y (1+ a-symbol-macro)))
          (f a b)
          (pprint rst)
          (g (+ x y) a-symbol-macro))

        a some-symbol))

    ((cl:lambda (a b &rest rst &key a-symbol-macro &allow-other-keys &aux (x 1) (y (1+ a-symbol-macro)))
       (f (pass-form a) b)
       (pprint rst)
       (g (+ x y) a-symbol-macro))

     a a-symbol-macro)

    nil
    1
    (1+ a-symbol-macro)
    a-symbol-macro

    (f (pass-form a) b)
    (pass-form a)
    a
    b

    (pprint rst)
    rst

    (g (+ x y) a-symbol-macro)
    (+ x y)
    x y
    a-symbol-macro

    a
    a-symbol-macro
    some-symbol))


;;; Test Lexical Function Definitions

(test walk-flet
  "Test walking FLET forms"

  (is-walk
      ((cl:flet ((f (a b &optional (c 1))
                (pprint a)
                (pass-form (pprint b))
                (+ a b c))

              (g (x) x)

              (pass-form (form) (pprint form)))
         (declare (ftype (function (integer integer &optional integer) integer) f))

         (pass-form (f 1 2))
         (g (pass-form a-symbol-macro)))

       (cl:flet ((f (a b &optional (c 1))
                (pprint a)
                (pprint b)
                (+ a b c))

              (g (x) x)

              (pass-form (form) (pprint form)))
         (declare (ftype (function (integer integer &optional integer) integer) f))

         (pass-form (f 1 2))
         (g (pass-form some-symbol))))

    (cl:flet ((f (a b &optional (c 1))
             (pprint a)
             (pass-form (pprint b))
             (+ a b c))

           (g (x) x)

           (pass-form (form) (pprint form)))
      (declare (ftype (function (integer integer &optional integer) integer) f))

      (pass-form (f 1 2))
      (g (pass-form a-symbol-macro)))

    1

    (pprint a)
    a

    (pass-form (pprint b))
    (pprint b)
    b

    (+ a b c)
    a b c

    x

    (pprint form)
    form

    (pass-form (f 1 2))
    (f 1 2)
    1 2

    (g (pass-form a-symbol-macro))
    (pass-form a-symbol-macro)
    a-symbol-macro
    some-symbol))

(test walk-labels
  "Test walking LABELS forms"

  (is-walk
      ((cl:labels ((f (a b &optional (c 1))
                  (pprint a)
                  (pass-form (pprint b))
                  (+ a b c))

                (g (x) x)

                (pass-form (form) (pprint form)))
         (declare (ftype (function (integer integer &optional integer) integer) f))
         (declare (optimize (speed 3)))

         (pass-form (f 1 2))
         (g (pass-form a-symbol-macro)))

       (cl:labels ((f (a b &optional (c 1))
                  (pprint a)
                  (pass-form (pprint b))
                  (+ a b c))

                (g (x) x)

                (pass-form (form) (pprint form)))
         (declare (ftype (function (integer integer &optional integer) integer) f))
         (declare (optimize (speed 3)))

         (pass-form (f 1 2))
         (g (pass-form some-symbol))))

    (cl:labels ((f (a b &optional (c 1))
               (pprint a)
               (pass-form (pprint b))
               (+ a b c))

             (g (x) x)

             (pass-form (form) (pprint form)))
      (declare (ftype (function (integer integer &optional integer) integer) f))
      (declare (optimize (speed 3)))

      (pass-form (f 1 2))
      (g (pass-form a-symbol-macro)))

    1

    (pprint a)
    a

    (pass-form (pprint b))
    (pprint b)
    b

    (+ a b c)
    a b c

    x

    (pprint form)
    form

    (pass-form (f 1 2))
    (f 1 2)
    1 2

    (g (pass-form a-symbol-macro))
    (pass-form a-symbol-macro)
    a-symbol-macro
    some-symbol))


;;; Test Lexical Variable Bindings

(test walk-let
  "Test walking LET forms"

  (is-walk ((let ((x 1)
                  (a-symbol-macro (1+ x))
                  y
                  (z a-symbol-macro))
              (declare (type integer x a-symbol-macro))

              (pass-form (pprint x))
              a-symbol-macro)

            (let ((x 1)
                  (a-symbol-macro (1+ x))
                  (y)
                  (z some-symbol))
              (declare (type integer x a-symbol-macro))

              (pprint x)
              a-symbol-macro))

    (let ((x 1)
          (a-symbol-macro (1+ x))
          y
          (z a-symbol-macro))
      (declare (type integer x a-symbol-macro))

      (pass-form (pprint x))
      a-symbol-macro)

    1
    (1+ x) x
    a-symbol-macro some-symbol

    (pass-form (pprint x))
    (pprint x)
    x

    a-symbol-macro))

(test walk-let*
  "Test walking LET* forms"

  (is-walk ((let* ((x 1)
                   (a-symbol-macro (1+ x))
                   y
                   (z a-symbol-macro))
              (declare (type integer x a-symbol-macro)
                       (optimize (speed 3)))

              (pass-form (pprint x))
              a-symbol-macro)

            (let* ((x 1)
                   (a-symbol-macro (1+ x))
                   (y)
                   (z a-symbol-macro))
              (declare (type integer x a-symbol-macro)
                       (optimize (speed 3)))

              (pprint x)
              a-symbol-macro))

    (let* ((x 1)
           (a-symbol-macro (1+ x))
           y
           (z a-symbol-macro))
      (declare (type integer x a-symbol-macro)
               (optimize (speed 3)))

      (pass-form (pprint x))
      a-symbol-macro)

    1
    (1+ x) x
    a-symbol-macro

    (pass-form (pprint x))
    (pprint x)
    x

    a-symbol-macro))

(test walk-locally
  "Test walking LOCALLY forms"

  (is-walk ((cl:locally (declare (optimize (speed 3)))
              (pass-form (pprint x))
              a-symbol-macro)

            (cl:locally (declare (optimize (speed 3)))
              (pprint x)
              some-symbol))

    (cl:locally (declare (optimize (speed 3)))
      (pass-form (pprint x))
      a-symbol-macro)

    (pass-form (pprint x))
    (pprint x)
    x

    a-symbol-macro
    some-symbol))


;;; Test Lexical Macro Definitions

(test walk-macrolet
  "Test walking MACROLET forms"

  (is-walk
      ((cl:macrolet ((pass-form (form)
                       (list 'quote form))

                     (square (x y)
                       (list '* (list '+ x y) (list '- x y))))

         (pass-form (pprint x))
         a-symbol-macro
         (square a b))

       (cl:macrolet ((pass-form (form)
                       (list 'quote form))

                     (square (x y)
                       (list '* (list '+ x y) (list '- x y))))

         '(pprint x)
         some-symbol
         (* (+ a b) (- a b))))

    (cl:macrolet ((pass-form (form)
                    (list 'quote form))

                  (square (x y)
                    (list '* (list '+ x y) (list '- x y))))

      (pass-form (pprint x))
      a-symbol-macro
      (square a b))

    (pass-form (pprint x))
    '(pprint x)

    a-symbol-macro
    some-symbol

    (square a b)
    (* (+ a b) (- a b))
    (+ a b)
    a b
    (- a b)
    a b))

(test walk-symbol-macrolet
  "Test walking MACROLET forms"

  (is-walk
      ((cl:symbol-macrolet ((a-symbol-macro "hello")
                            (local-sym :local))

         (pprint a-symbol-macro)
         (pass-form local-sym))

       (cl:symbol-macrolet ((a-symbol-macro "hello")
                            (local-sym :local))

         (pprint "hello")
         :local))

    (cl:symbol-macrolet ((a-symbol-macro "hello")
                            (local-sym :local))

         (pprint a-symbol-macro)
         (pass-form local-sym))

    (pprint a-symbol-macro)
    a-symbol-macro
    "hello"

    (pass-form local-sym)
    local-sym
    :local))

(test (walk-load-time-value :depends-on walk-symbol-macrolet)
  "Test walking LOAD-TIME-VALUE forms"

  (is-walk
      ((cl:symbol-macrolet ((x 1))
         (load-time-value (pass-form (1+ x))))

       (cl:symbol-macrolet ((x 1))
         (load-time-value (1+ 1) nil)))

    (cl:symbol-macrolet ((x 1))
      (load-time-value (pass-form (1+ x))))

    (load-time-value (pass-form (1+ x)))
    (pass-form (1+ x))
    (1+ x)
    x
    1))


;;; Test SETQ

(test walk-setq
  "Test walking SETQ forms"

  (is-walk ((setq x (f 1)
                  y a-symbol-macro)

            (setq x (f 1)
                  y some-symbol))

    (setq x (f 1)
          y a-symbol-macro)

    (f 1)
    1

    a-symbol-macro
    some-symbol))

(test walk-setq-macro
  "Test walking SETQ forms with symbol-macros"

  (let ((forms))
    (flet ((walk (form env)
             (push form forms)
             (if (eq (ensure-car form) 'cl:setf)
                 (values form t)
                 form)))

      (is
       (equal
        '(cl:setf x 1 y 2 a-symbol-macro 3)
        (walk-form #'walk '(setq x 1 y 2 a-symbol-macro 3) nil)))

      (is
       (equal
        '((setq x 1 y 2 a-symbol-macro 3)
          (cl:setf x 1 y 2 a-symbol-macro 3))

        (nreverse forms))))))

(test walk-setq-malformed
  "Test walking malformed SETQ forms"

  (flet ((f (x e)
           (declare (ignore e))
           x))

    (signals
        cl-form-types:malformed-form-error

      (walk-form #'f '(setq x 1 y) nil))

    (signals
        cl-form-types:malformed-form-error

      (walk-form #'f '(setq (place x) 1) nil))

    (signals
        cl-form-types:malformed-form-error

      (walk-form #'f '(setq 1 2) nil))))
