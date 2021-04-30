;;;; special-forms.lisp
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

;;;; Unit tests for special form types

(defpackage :cl-form-types/test.special-forms
  (:use :cl-environments-cl
	:cl-form-types
	:alexandria

	:fiveam
	:cl-form-types/test))

(in-package :cl-form-types/test.special-forms)


;;; Test Suite Definition

(def-suite special-forms
    :description "Test determining type of special forms."
    :in cl-form-types)

(in-suite special-forms)


;;; Definitions used by tests

(defmacro pass-form (form)
  form)


;;; THE Form Tests

(test the-forms
  "Test FORM-TYPE on THE forms"

  (is-form-type number (cl:the number some-nonsense))
  (is-form-type (simple-array integer) (the (simple-array integer) (range 1 10)))
  (is-form-type (array fixnum 2) (cl:the (array fixnum 2) (identity-matrix 3))))

(test macro-the-forms
  "Test FORM-TYPE on macros which expand to THE forms"

  (macrolet ((local-pass (form)
	       `(pass-form ,form)))

    (symbol-macrolet ((the-number-10 (the number 10)))

      (is-form-type string (pass-form (the string (get-greeting "Bob"))))
      (is-form-type number the-number-10)
      (is-form-type number (local-pass the-number-10)))))


;;; QUOTE Form Tests

(test quote-forms
  "Test FORM-TYPE on QUOTE forms"

  (is-form-type (eql x) 'x)
  (is-form-type list '(1 2 3))
  (is-form-type string '"hello"))

(test macro-quote-forms
  "Test FORM-TYPE on macros which expand to QUOTE forms"

  (macrolet ((local-pass (form)
	       `(pass-form ,form))

	     (my-quote (thing)
	       `',thing))

    (symbol-macrolet ((the-quoted-symbol 'symbol))

      (is-form-type (eql x) (pass-form 'x))
      (is-form-type (eql hello) (local-pass 'hello))
      (is-form-type (eql symbol) the-quoted-symbol)
      (is-form-type list (my-quote '(a b c d))))))


;;; FUNCTION FORM Tests

(test function-forms
  "Test FORM-TYPE on FUNCTION forms"

  (flet ((mul (x y) (* x y)))
    (declare (ftype (function (integer integer) integer) mul))

    (is-form-type (function (integer integer) integer) #'mul)
    (is-form-type function #'unknown-function :strict t)))

(test macro-function-forms
  "Test FORM-TYPE on macros which expand to FUNCTION forms"

  (flet ((flip (x) (/ x)))
    (declare (ftype (function (integer) number) flip))

    (macrolet ((func (name)
		 `(pass-form #',name)))

      (symbol-macrolet ((func-flip #'flip))

	(is-form-type (function (integer) number) (pass-form #'flip))
	(is-form-type (function (integer) number) (func flip))
	(is-form-type (function (integer) number) func-flip)
	(is-form-type function (func unknown-func) :strict t)))))


;;; PROGN Form Tests

(test progn-forms
  "Test FORM-TYPE on PROGN forms"

  (is-form-type string
    (progn
      (pprint hello)
      (pprint bye)
      (cl:the string (concatenate hello bye))))

  (is-form-type number
    (progn
      (pass-form (the number (+ a b))))))

(test progn-nested-forms
  "Test FORM-TYPE on nested PROGN forms"

  (is-form-type string
    (progn
      (pprint hello)
      (progn
	(pprint bye)
	(cl:the string (concatenate hello bye))))))

(test progn-empty-forms
  "Test FORM-TYPE on empty PROGN forms"

  (is-form-type (eql nil) (progn))
  (is-form-type (eql nil) (progn (progn))))

(test progn-variable-forms
  "Test FORM-TYPE on PROGN forms which return value of variable"

  (let ((greeting "hello world"))
    (declare (type string greeting)
	     (ignorable greeting))

    (symbol-macrolet ((the-number-5 5))

      (is-form-type string
	(progn
	  (pprint greeting)
	  greeting))

      (is-form-type (eql 5)
	(progn the-number-5)))))

(test progn-list-forms
  "test FORM-TYPES on PROGN forms which return function call expression"

  (labels ((inc (a) (1+ a))
	   (add (x y) (+ x y)))
    (declare (ftype (function (integer) integer) inc)
	     (ftype (function (number number) number) add))

    (is-form-type integer
      (progn
	(add a b)
	(inc c)))

    (is-form-type number
      (progn
	(inc c)
	(add a b)))))

(test macro-progn-forms
  "Test FORM-TYPE on macros which expand to PROGN forms"

  (macrolet ((local-pass (form)
	       `(pass-form ,form))

	     (swap-forms (form1 form2)
	       `(progn ,form2 ,form1)))

    (symbol-macrolet ((progn-integer
		       (progn
			 (pprint a)
			 (pprint b)
			 (the integer (+ a b)))))

      (is-form-type string
	(pass-form
	 (progn
	   (pprint str1)
	   (the string (concatenate str1 str2)))))

      (is-form-type integer progn-integer)
      (is-form-type integer (local-pass progn-integer))

      (is-form-type (eql 100) (swap-forms 100 (pprint "one hundred"))))))


;;; EVAL-WHEN Form Tests

(test eval-when-forms
  "Test FORM-TYPE on EVAL-WHEN forms"

  (is-form-type string
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (pprint hello)
      (pprint bye)
      (cl:the string (concatenate helloe bye))))

  (is-form-type number
    (eval-when (:execute)
      (pass-form (the number (+ a b))))))

(test eval-when-no-execute-forms
  "Test FORM-TYPE on EVAL-WHEN forms without :EXECUTE situation"

  (is-form-type null
    (eval-when (:compile-toplevel)
      "hello world")))

(test eval-when-nested-forms
  "Test FORM-TYPE on nested EVAL-WHEN forms"

  (is-form-type string
    (eval-when (:load-toplevel :execute :compile-toplevel)
      (pprint hello)
      (eval-when (cl:eval)
	(pprint bye)
	(cl:the string (concatenate hello bye))))))

(test eval-when-empty-forms
  "Test FORM-TYPE on empty EVAL-WHEN forms"

  (is-form-type null (cl:eval-when (:load-toplevel :execute :compile-toplevel))))

(test eval-when-variable-forms
  "Test FORM-TYPE on EVAL-WHEN forms which return value of variable"

  (let ((greeting "hello world"))
    (declare (type string greeting)
	     (ignorable greeting))

    (symbol-macrolet ((the-number-5 5))

      (is-form-type string
	(eval-when (:execute)
	  (pprint greeting)
	  greeting))

      (is-form-type (eql 5)
	(eval-when (compile load eval) the-number-5)))))

(test eval-when-list-forms
  "Test FORM-TYPES on EVAL-WHEN forms which return function call expression"

  (labels ((inc (a) (1+ a))
	   (add (x y) (+ x y)))

    (declare (ftype (function (integer) integer) inc)
	     (ftype (function (number number) number) add))

    (is-form-type integer
      (eval-when (:execute)
	(add a b)
	(inc c)))

    (is-form-type number
      (eval-when (:execute)
	(inc c)
	(add a b)))))

(test macro-eval-when-forms
  "Test FORM-TYPE on macros which expand to EVAL-WHEN forms"

  (macrolet ((group (&body forms)
	       `(pass-form
		 (eval-when (:execute)
		   ,@forms))))

    (symbol-macrolet ((eval-when-integer
		       (eval-when (:execute)
			 (pprint a)
			 (pprint b)
			 (the integer (+ a b)))))

      (is-form-type string
	(pass-form
	 (eval-when (:execute)
	   (pprint str1)
	   (the string (concatenate str1 str2)))))

      (is-form-type integer eval-when-integer)
      (is-form-type integer (pass-form eval-when-integer))

      (is-form-type (eql 100) (group (pprint "one hundred") 100)))))


;;; SETQ Form Tests

(test setq-forms
  "Test FORM-TYPE on SETQ forms"

  (is-form-type (eql 1) (setq x 1))
  (is-form-type (eql a)
    (setq y 2
	  z 'a))

  (is-form-type number
    (setq a 1
	  b x
	  c (pass-form (the number (+ a b)))))

  (is-form-type null (setq)))

(test setq-malformed-forms
  "Test FORM-TYPE on malformed SETQ forms"

  (is-form-type t (setq a) :strict t)
  (is-form-type t (setq a 1 2) :strict t))

(test setq-variable-forms
  (let ((x 1))
    (declare (type integer x)
	     (ignorable x))

    (symbol-macrolet ((greeting "hello world"))

      (is-form-type integer (setq var x))
      (is-form-type string (setq str greeting)))))

(test setq-function-forms
  (flet ((join (str1 str2)
	   (concatenate str1 str2))

	 (symb (&rest things)
	   (apply #'symbolicate things)))

    (declare (ftype (function (string string) string) join)
	     (ftype (function (* &rest *) symbol) symb))

    (is-form-type string
      (setq x (symb 'hello 1)
	    y (join "Hello" "World")))

    (is-form-type symbol
      (setq x (join "Hello" "World")
	    y (symb 'hello 1)))))

(test macro-setq-forms
  "Test FORM-TYPE on macros which expand to SETQ forms"

  (macrolet ((my-setq (var form)
	       `(pass-form (setq ,var ,form))))

    (symbol-macrolet ((setq-x-1 (setq x 1)))

      (is-form-type string
	(pass-form
	 (setq var1 "hello world")))

      (is-form-type symbol
	(my-setq var-type 'string))

      (is-form-type (eql 1) setq-x-1))))


;;; LOCALLY Form Tests

(test locally-forms
  "Test FORM-TYPE on simple (without type declarations) LOCALLY forms"

  (is-form-type string
    (cl:locally (declare (optimize speed))
      (pprint hello)
      (pprint bye)
      (cl:the string (concatenate hello bye))))

  (is-form-type number
    (cl:locally (pass-form (the number (+ a b))))))

(test locally-nested-forms
  "Test FORM-TYPE on nested LOCALLY forms"

  (is-form-type string
    (cl:locally
	(pprint hello)
      (locally (declare (optimize space))
	(pprint bye)
	(cl:the string (concatenate hello bye))))))

(test locally-empty-forms
  "Test FORM-TYPE on empty LOCALLY forms"

  (is-form-type (eql nil) (locally))
  (is-form-type null (locally (declare (optimize debug)))))

(test locally-variable-forms
  "Test FORM-TYPE on LOCALLY forms with TYPE declarations, returning variables"

  (let ((x 0.5) (y 2) (z 3/4))
    (declare (type integer y)
	     (type number z))

    (symbol-macrolet ((greeting "hello world"))

      (is-form-type integer
      	(cl:locally (declare (type float x))
      	  (pprint x)
      	  (pprint (+ x y z))
      	  y))

      (is-form-type number
      	(cl:locally
      	    (pprint (+ x y z))
      	  z))

      (is-form-type float
      	(cl:locally (declare (type float x))
      	  (pprint x)
      	  (pprint y)
      	  (pprint z)
      	  x))

      (is-form-type string
	(cl:locally (declare (optimize speed))
	  (pprint greeting)
	  greeting)))))

(test locally-list-forms
  "Test FORM-TYPE on LOCALLY forms with FTYPE, returning function call expressions"

  (labels ((neg (a) (- a))
	   (sub (a b) (- a b)))

    (is-form-type t (locally (neg x)) :strict t)

    (is-form-type integer
      (cl:locally (declare (ftype (function (integer) integer) neg))
	(sub x y)
	(neg z)))

    (is-form-type number
      (cl:locally (declare (ftype (function (number number) number) sub))
	(sub (neg var) var)))))

(test macro-locally-forms
  "Test FORM-TYPE on macros which expand to LOCALLY forms"

  (macrolet ((local (form)
	       `(pass-form ,form)))

    (symbol-macrolet ((number-one (locally 1)))

      (is-form-type string
	(pass-form
	 (cl:locally (declare (optimize speed))
	   (pprint str1)
	   (the string (concatenate str1 str2)))))

      (is-form-type (eql 1) number-one)
      (is-form-type integer (local number-one)))))
