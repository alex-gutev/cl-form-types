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


;;; IF Form Tests

(test if-forms
  "Test FORM-TYPE on IF forms"

  (is-form-type (or (eql 100) string)
    (if (test something)
	100
	"hello world")))

(test if-no-else-forms
  "Test FORM-TYPE on IF forms with else branch"

  (is-form-type (or integer null)
    (if (plusp x) (the integer x))))

(test if-variable-forms
  "Test FORM-TYPE on IF forms which return value of variable"

  (let ((varx 35))
    (declare (type integer varx)
	     (ignorable varx))

    (symbol-macrolet ((greeting "hello"))

      (is-form-type (or integer null)
	(if (test x) varx))

      (is-form-type (or integer string)
	(if (test x) varx greeting)))))

(test if-list-forms
  "Test FORM-TYPE on IF forms which return function call expression"

  (flet ((flip (x) (reverse x))
	 (mul (y z) (* y z)))
    (declare (ftype (function (string) string) flip)
	     (ftype (function (number number) number) mul))

    (is-form-type (or string number)
      (if (evenp a) (flip name) (mul a b)))

    (is-form-type (or number null)
      (if (evenp a) (mul a b)))))


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


;;; PROGV Form Tests

(test progv-forms
  "Test FORM-TYPE on PROGV forms"

  (is-form-type string
    (progv '(x y z) '(1 2 3)
      (pprint hello)
      (pprint bye)
      (cl:the string (concatenate hello bye))))

  (is-form-type number
    (progv nil nil
      (pass-form (the number (+ a b))))))

(test progv-nested-forms
  "Test FORM-TYPE on nested PROGV forms"

  (is-form-type string
    (progv vars values
      (pprint hello)
      (progv nil nil
	(pprint bye)
	(cl:the string (concatenate hello bye))))))

(test progv-empty-forms
  "Test FORM-TYPE on empty PROGV forms"

  (is-form-type (eql nil) (progv '(x y z) '(1 2 3)))
  (is-form-type (eql nil) (progv nil nil (progv nil nil))))

(test progv-variable-forms
  "Test FORM-TYPE on PROGV forms which return value of variable"

  (let ((greeting "hello world"))
    (declare (type string greeting)
	     (ignorable greeting))

    (symbol-macrolet ((the-number-5 5))

      (is-form-type string
	(progv nil nil
	  (pprint greeting)
	  greeting))

      (is-form-type (eql 5)
	(progv nil nil the-number-5)))))

(test progv-list-forms
  "test FORM-TYPES on PROGV forms which return function call expression"

  (labels ((inc (a) (1+ a))
	   (add (x y) (+ x y)))
    (declare (ftype (function (integer) integer) inc)
	     (ftype (function (number number) number) add))

    (is-form-type integer
      (progv nil nil
	(add a b)
	(inc c)))

    (is-form-type number
      (progv nil nil
	(inc c)
	(add a b)))))

(test macro-progv-forms
  "Test FORM-TYPE on macros which expand to PROGv forms"

  (macrolet ((local-pass (form)
	       `(pass-form ,form))

	     (swap-forms (form1 form2)
	       `(progv nil nil ,form2 ,form1)))

    (symbol-macrolet ((progv-integer
		       (progv nil nil
			 (pprint a)
			 (pprint b)
			 (the integer (+ a b)))))

      (is-form-type string
	(pass-form
	 (progv nil nil
	   (pprint str1)
	   (the string (concatenate str1 str2)))))

      (is-form-type integer progv-integer)
      (is-form-type integer (local-pass progv-integer))

      (is-form-type (eql 100) (swap-forms 100 (pprint "one hundred"))))))


;;; MULTIPLE-VALUE-PROG1 Form Tests

(test multiple-value-prog1-forms
  "Test FORM-TYPE on MULTIPLE-VALUE-PROG1 forms"

  (is-form-type string
    (multiple-value-prog1 (cl:the string (concatenate hello bye))
      (pprint hello)
      (pprint bye)))

  (is-form-type number
    (multiple-value-prog1
	(pass-form (the number (+ a b)))
      "hello world")))

(test multiple-value-prog1-nested-forms
  "Test FORM-TYPE on nested MULTIPLE-VALUE-PROG1 forms"

  (is-form-type string
    (multiple-value-prog1
	(multiple-value-prog1 (cl:the string (concatenate hello bye))
	  (pprint bye))
      (pprint hello))))

(test multiple-value-prog1-empty-forms
  "Test FORM-TYPE on empty MULTIPLE-VALUE-PROG1 forms"

  (is-form-type (eql nil) (multiple-value-prog1 nil))
  (is-form-type (eql nil) (multiple-value-prog1 (multiple-value-prog1 nil))))

(test multiple-value-prog1-variable-forms
  "Test FORM-TYPE on MULTIPLE-VALUE-PROG1 forms which return value of variable"

  (let ((greeting "hello world"))
    (declare (type string greeting)
	     (ignorable greeting))

    (symbol-macrolet ((the-number-5 5))

      (is-form-type string
	(multiple-value-prog1 greeting
	  (pprint greeting)))

      (is-form-type (eql 5)
	(multiple-value-prog1 the-number-5
	  "A string")))))

(test multiple-value-prog1-list-forms
  "test FORM-TYPES on MULTIPLE-VALUE-PROG1 forms which return function call expression"

  (labels ((inc (a) (1+ a))
	   (add (x y) (+ x y)))
    (declare (ftype (function (integer) integer) inc)
	     (ftype (function (number number) number) add))

    (is-form-type integer
      (multiple-value-prog1 (inc c)
	(add a b)))

    (is-form-type number
      (multiple-value-prog1 (add a b)
	(inc c)))))

(test macro-multiple-value-prog1-forms
  "Test FORM-TYPE on macros which expand to Multiple-Value-Prog1 forms"

  (macrolet ((local-pass (form)
	       `(pass-form ,form))

	     (swap-forms (form1 form2)
	       `(multiple-value-prog1 ,form1 ,form2)))

    (symbol-macrolet ((multiple-value-prog1-integer
		       (multiple-value-prog1
			   (the integer (+ a b))
			 (pprint a)
			 (pprint b))))

      (is-form-type string
	(pass-form
	 (multiple-value-prog1
	     (the string (concatenate str1 str2))
	   (pprint str1))))

      (is-form-type integer multiple-value-prog1-integer)
      (is-form-type integer (local-pass multiple-value-prog1-integer))

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


;;; UNWIND-PROTECT Form Tests

(test unwind-protect-forms
  "Test FORM-TYPE on UNWIND-PROTECT forms"

  (is-form-type string
    (unwind-protect (cl:the string (concatenate helloe bye))
      (pprint hello)
      (pprint bye)))

  (is-form-type number
    (unwind-protect (pass-form (the number (+ a b))))))

(test unwind-protect-no-cleanup-forms
  "Test FORM-TYPE on UNWIND-PROTECT forms without cleanup forms"

  (is-form-type string
    (unwind-protect "hello world")))

(test unwind-protect-nested-forms
  "Test FORM-TYPE on nested UNWIND-PROTECT forms"

  (is-form-type string
    (unwind-protect
	 (unwind-protect
	      (cl:the string (concatenate hello bye))
	   (pprint bye))
      (pprint hello))))

(test unwind-protect-variable-forms
  "Test FORM-TYPE on UNWIND-PROTECT forms which return value of variable"

  (let ((greeting "hello world"))
    (declare (type string greeting)
	     (ignorable greeting))

    (symbol-macrolet ((the-number-5 5))

      (is-form-type string
	(unwind-protect greeting
	  (pprint greeting)))

      (is-form-type (eql 5)
	(unwind-protect the-number-5)))))

(test unwind-protect-list-forms
  "Test FORM-TYPES on UNWIND-PROTECT forms which return function call expression"

  (labels ((inc (a) (1+ a))
	   (add (x y) (+ x y)))

    (declare (ftype (function (integer) integer) inc)
	     (ftype (function (number number) number) add))

    (is-form-type integer
      (unwind-protect (inc c)
	(add a b)))

    (is-form-type number
      (unwind-protect (add a b)
	(inc c)))))


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


;;; Control Transfer Form Tests

(test go-forms
  "Test FORM-TYPE on GO forms"

  ;; GO forms do not return, instead they execute a jump

  (is-form-type nil
    (go tag-1)
    :strict t)

  ;; Malformed but test to make sure the tag is not being interpreted
  ;; as a number.
  (is-form-type nil
    (go 2)
    :strict t))

(test return-from
  "Test FORM-TYPE on RETURN-FROM forms"

  ;; RETURN-FROM forms do not return, instead they execute a jump

  (is-form-type nil
    (return-from block-name 102))

  (is-form-type nil
    (return-from nil "abc")))

(test throw-forms
  "Test FORM-TYPE on THROW forms"

  ;; THROW forms do not return, instead they execute a jump

  (is-form-type nil
    (throw 'tag-name 15)
    :strict t))

(test tagbody-forms
  "Test FORM-TYPE on TAGBODY forms"

  ;; TAGBODY forms return NIL by definition

  (is-form-type null
    (tagbody
     tag1
       (pprint "In TAG1")
       (go tag3)

     tag2
       (pprint "IN TAG2")
       (go tag1)

     tag3
       (the number (get-x)))))


;;; Local Macro Definition Forms

(test macrolet-forms
  "Test FORM-TYPE on MACROLET forms"

  (is-form-type string
    (cl:macrolet ((local-mac (operator &rest operands)
		 `(pass-form (,operator ,@operands)))
	       (wrap-number (form)
		 `(the number ,form)))

      (pprint "Hello World")
      (* 1 2)
      (local-mac the string name)))

  (is-form-type number
    (cl:macrolet ((local-mac (operator &rest operands)
		 `(pass-form (,operator ,@operands)))
	       (wrap-number (form)
		 `(the number ,form)))

      (format t "X: ~a~%Y: ~a~%" x y)
      (wrap-number (+ x y)))))

(test macrolet-variable-forms
  "Test FORM-TYPE on MACROLET forms which return variable"

  (let ((x 1050))
    (declare (type number x))

    (symbol-macrolet ((person-name "Joe"))

      (is-form-type number
	(cl:macrolet ((local-pass (form)
		     `(pass-form ,form)))

	  (pprint "In MACROLET")
	  (local-pass x)))

      (is-form-type string
	(cl:macrolet ((local-mac (form)
		     `(pass-form ,form)))

	  (pprint "In MACROLET")
	  (local-mac person-name))))))

(test macrolet-function-forms
  "Test FORM-TYPE on MACROLET forms which return function"

  (flet ((func (x) (- x))
	 (thing (str) (reverse str)))

    (declare (ftype (function (integer) integer) func)
	     (ftype (function (sequence) sequence) thing))

    (is-form-type integer
      (cl:macrolet ((thing (form)
		   `(pass-form ,form)))

	(pprint "In MACROLET")
	(thing (func x))))))

(test symbol-macrolet-forms
  "Test FORM-TYPE on SYMBOL-MACROLET forms"

  (is-form-type string
    (cl:symbol-macrolet ((greeting "Welcome!"))
      (pprint greeting)
      (* 1 2)
      greeting))

  (is-form-type number
    (cl:symbol-macrolet ((the-number-5 5))
      (format t "Number: ~a~%" the-number-5)
      (pass-form the-number-5))))

(test symbol-macrolet-variable-forms
  "Test FORM-TYPE on SYMBOL-MACROLET forms which return variable"

  (let ((x 1050))
    (declare (type number x))

    (symbol-macrolet ((person-name "Joe"))

      (is-form-type number
	(cl:symbol-macrolet ((greeting "Welcome"))
	  (pprint "In SYMBOL-MACROLET")
	  x))

      (is-form-type string
	(cl:symbol-macrolet ((greeting "Welcome"))
	  (pprint "In SYMBOL-MACROLET")
	  (pass-form person-name)))

      (is-form-type symbol
	(cl:symbol-macrolet ((x 'a-symbol))
	  (pprint "In SYMBOL-MACROLET")
	  x)))))

(test symbol-macrolet-function-forms
  "Test FORM-TYPE on SYMBOL-MACROLET forms which return function expression"

  (flet ((thing (seq) (reverse seq)))
    (declare (ftype (function (sequence) sequence) thing))

    (is-form-type sequence
      (cl:symbol-macrolet ()
	(pprint "In SYMBOL-MACROLET")
	(thing seq)))))


;;; Local Variable Binding Form Types

(test let-forms
  "Test FORM-TYPE on LET forms"

  (is-form-type (eql 10)
    (cl:let ()
      (pprint "Hello")
      10))

  (is-form-type string
    (cl:let ((name "Bob"))
      (declare (type string name))
      (format t "Hello: ~a~%" name)
      name)))

(test let-variable-forms
  "Test FORM-TYPE on LET forms which return variable"

  (let ((x 1050))
    (declare (type number x)
	     (ignorable x))

    (symbol-macrolet ((greeting "Welcome"))

      (is-form-type number
	(cl:let ((y 10))
	  (pprint (+ x y))
	  (pass-form x)))

      (is-form-type string
	(cl:let ((x "x"))
	  (declare (type string x))
	  (format t "X: ~a" x)
	  x))

      (is-form-type string
	(cl:let ((x 'x))
	  (format t "X: ~a" x)
	  greeting)))))

(test let-function-forms
  "Test FORM-TYPE on LET forms which return function expression"

  (flet ((thing (seq) (reverse seq)))
    (declare (ftype (function (sequence) sequence) thing))

    (is-form-type sequence
      (cl:let ((x 1) (y 2))
	(declare (type integer x y))
	(pprint x)
	(pprint y)
	(thing (list x y))))))
