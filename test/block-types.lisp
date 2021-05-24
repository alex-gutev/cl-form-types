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

;;;; Unit tests for BLOCK form types

(defpackage :cl-form-types/test.block-forms
  (:use :cl-environments-cl
	:cl-form-types
	:alexandria

	:fiveam
	:cl-form-types/test))

(in-package :cl-form-types/test.block-forms)


;;; Test Suite Definition

(def-suite block-forms
    :description "Test determining type of BLOCK forms."
    :in cl-form-types)

(in-suite block-forms)


;;; Definitions used by tests

(defmacro pass-form (form)
  form)


;;; Simple Tests

(test (simple-block-forms :compile-at :run-time)
  "Test FORM-TYPE on BLOCK forms without RETURN-FROM"

  (is-form-type (eql 10)
    (block my-block
      (pprint (+ x y))
      (pprint (* a b))
      (progn
	(pass-form 10)))

    :test type=))

(test (with-return-from-other-block :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with RETURN-FROM other block"

  (is-form-type (eql 2)
    (block my-block
      (pprint v1)
      (princ v2)

      (when (< v1 v2)
	(return-from other-block 1))

      2)

    :test type=))


;;; With RETURN-FROM

(test (with-return-from :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with RETURN-FROM same block"

  (is-form-type (or (eql ret1) (eql ret2))
    (block my-block
      (pprint v1)
      (princ v2)

      (when (test v1 v2)
	(return-from my-block 'ret1))

      'ret2)

    :test type=))

(test (with-return-from-nil :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with NIL name with RETURN-FROM"

  (is-form-type (or (eql ret1) (eql ret2))
    (block nil
      (pprint v1)
      (princ v2)

      (when (test v1 v2)
	(return-from nil 'ret1))

      'ret2)

    :test type=))

(test (with-return-from-nested :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with RETURN-FROM in nested block"

  (is-form-type (or (eql x) (eql z))
    (block blk1
      (something x)
      (something y)

      (block blk2
	(f x (return-from blk1 'x))
	(return-from blk2 'y))

      'z)

    :test type=))

(test (with-return-from-nested-same-name :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with RETURN-FROM in nested block with same name"

  (is-form-type (eql r3)
    (block blk1
      (something x)
      (something y)

      (block blk1
	(f x (return-from blk1 'r1))
	(return-from blk2 'r2))

      'r3)

    :test type=))

(test (with-nested-return-from :compile-at :run-time :depends-on with-if)
  "Test FORM-TYPE on BLOCK with RETURN-FROM nested in RETURN-FROM"

  (is-form-type (member t1 t2 t3)
    (block nil
      (if (something)
	  (return
	    (if (something-else)
		(return 't1)
		't2))
	  't3))

    :test type=))


;;; Test with FLET forms

(test (with-flet :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with FLET form"

  (is-form-type (or (eql from-block) (eql from-g))
    (block blk
      (flet ((f (x)
	       (something x)
	       (return-from blk 'from-f))

	     (g (y)
	       (f y)
	       (return-from blk 'from-g)))

	(pprint (get-x))
	(g (get-x))

	'from-block))

    :test type=))

(test (with-nested-flet :compile-at :run-time :depends-on (and with-flet with-if))
  "Test FORM-TYPE on BLOCK with nested FLET form"

  (is-form-type (or (eql from-block) (eql from-g) (eql from-g1))
    (block blk
      (flet ((f (x)
	       (something x)
	       (return-from blk 'from-f))

	     (g (y)
	       (flet ((f1 (arg1 arg2)
			(return-from blk 'from-f1))

		      (f2 (var)
			(if var
			    (return-from blk 'from-g1)
			    (return-from blk2 'from-g2))))

		 (f2 (f y)))

	       (return-from blk 'from-g)))

	(pprint (get-x))
	(g (get-x))

	'from-block))

    :test type=))

(test (with-flet-return-from-in-lambda-list :compile-at :run-time :depends-on with-flet)
  "Test FORM-TYPE on BLOCK with FLET form with RETURN-FROM in lambda list"

  (is-form-type (or (eql l1) (eql l2) (eql l3) (eql l4) (eql l5) (eql default))
    (block nil
      (flet ((f1 (a &optional b
		    (c (return 'l1))
		    (d (return 'l2) d-sp)
		    &rest rest
		    &key e (f (return 'l3))
		    ((:key g) (return 'l4) g-sp)
		    &allow-other-keys
		    &aux (h (return 'l5)))

	       a)

	     (f2 (a &optional b
		    (c (return 'l6))
		    (d (return 'l7) d-sp)
		    &rest rest
		    &key e (f (return 'l8))
		    ((:key g) (return 'l9) g-sp)
		    &allow-other-keys
		    &aux (h (return 'l10)))

	       a))

	(f1 x)
	'default))

    :test type=))

(test (with-flet-nested-block :compile-at :run-time :depends-on with-flet)
  "Test FORM-TYPE on BLOCK with FLET and BLOCK nested in it"

  (is-form-type (or (eql from-f1) (eql default))
    (block nil
      (flet ((f1 (x)
	       (pprint x)
	       (return 'from-f1))

	     (f2 (y)
	       (pprint y)
	       (return 'from-f2)))

	(block nil
	  (f1 1)
	  (return 'from-inner-block))

	'default))

    :test type=))


;;; Test with LABELS forms

(test (with-labels :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with LABELS form"

  (is-form-type (or (eql from-block) (eql from-f) (eql from-g))
    (block blk
      (labels ((g (y)
		 (f y)
		 (return-from blk 'from-g))

	       (f (x)
		 (something x)
		 (return-from blk 'from-f))

	       (h (a b &optional c)
		 (return-from blk 'from-h)))

	(pprint (get-x))
	(g (get-x))

	'from-block))

    :test type=))

(test (with-nested-labels :compile-at :run-time :depends-on (and with-labels with-if))
  "Test FORM-TYPE on BLOCK with nested LABELS forms"

  (is-form-type (or (eql from-block) (eql from-f) (eql from-g) (eql from-f1) (eql from-g1))
    (block blk
      (labels ((f (x)
	       (something x)
	       (return-from blk 'from-f))

	     (g (y)
	       (labels ((f1 (arg1 arg2)
			  (return-from blk 'from-f1))

			(f2 (var)
			  (cond
			    ((evenp var)
			     (return-from blk 'from-g1))

			    ((zerop var)
			     (return-from blk2 'from-g2))

			    (t
			     (f1 var)))))

		 (f2 (f y)))

	       (return-from blk 'from-g)))

	(pprint (get-x))
	(g (get-x))

	'from-block))

    :test type=))

(test (with-labels-return-from-in-lambda-list :compile-at :run-time :depends-on with-labels)
  "Test FORM-TYPE on BLOCK with LABELS form with RETURN-FROM in lambda list"

  (is-form-type (or (eql l1) (eql l2) (eql l3) (eql l4) (eql l5) (eql default))
    (block nil
      (labels ((f1 (a &optional b
		      (c (return 'l1))
		      (d (return 'l2) d-sp)
		      &rest rest
		      &key e (f (return 'l3))
		      ((:key g) (return 'l4) g-sp)
		      &allow-other-keys
		      &aux (h (return 'l5)))

		 a)

	       (f2 (a &optional b
		      (c (return 'l6))
		      (d (return 'l7) d-sp)
		      &rest rest
		      &key e (f (return 'l8))
		      ((:key g) (return 'l9) g-sp)
		      &allow-other-keys
		      &aux (h (return 'l10)))

		 a))

	(f1 x)
	'default))

    :test type=))

(test (with-labels-nested-block :compile-at :run-time :depends-on with-labels)
  "Test FORM-TYPE on BLOCK with LABELS and BLOCK nested in it"

  (is-form-type (or (eql from-f1) (eql default))
    (block nil
      (labels ((f1 (x)
	       (pprint x)
	       (return 'from-f1))

	     (f2 (y)
	       (pprint y)
	       (return 'from-f2)))

	(block nil
	  (f1 1)
	  (return 'from-inner-block))

	'default))

    :test type=))


;;; Test with FUNCTION forms

(test (with-function :compile-at :run-time :depends-on with-flet)
  "Test FORM-TYPE on BLOCK with FUNCTION form"

  (is-form-type (member v1 v2)
    (block nil
      (flet ((f (x)
	       (pprint x)
	       (return 'v1)))

	(mapc #'f list))

      'v2)

    :test type=))

(test (with-function-lambda :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with FUNCTION form with LAMBDA Expression"

  (is-form-type (member v1 v2 v3 v4)
    (block nil
      (mapc
       (lambda (x &optional (y (return 'v1)) &key (z (return 'v2)))
	 (pprint x)
	 (return 'v3))

       list)
      'v4)

    :test type=))

(test (with-lambda-operator :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with expression with LAMBDA operator"

  (is-form-type (member v1 v2 v3 v4)
    (block nil
      ((cl:lambda (x &optional (y (return 'v1)) &key (z (return 'v2)))
	 (pprint x)
	 (return 'v3)
	 'v4)
       a b :z c))

    :test type=))


;;; Test with LET/LET* forms

(test (with-let :compile-at :run-time :depends-on with-if)
  "Test FORM-TYPE on BLOCK with LET forms"

  (is-form-type (or (eql in-init) (eql in-let) (eql in-last))
    (block b1
      (let ((x 1)
	    y
	    (z (return-from b1 'in-init)))

	(if (test x)
	    y
	    (return-from b1 'in-let))

	'in-last))

    :test type=))

(test (with-let* :compile-at :run-time :depends-on with-if)
  "Test FORM-TYPE on BLOCK with LET* forms"

  (is-form-type (or (eql in-init) (eql in-let) (eql in-last))
    (block b1
      (let ((x 1)
	    y
	    (z (return-from b1 'in-init)))

	(if (test x)
	    y
	    (return-from b1 'in-let))

	'in-last))

    :test type=))

(test (with-locally :compile-at :run-time :depends-on with-if)
  "Test FORM-TYPE on BLOCK with LOCALLY forms"

  (is-form-type (or (eql in-locally) (eql default-value))
    (block bk
      (locally (declare (optimize speed))
	(pprint a)
	(pprint b)
	(when (< a b)
	  (return-from bk 'in-locally)))

      'default-value)

    :test type=))


;;; Test unevaluated and non-processed forms

(test (with-go :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with GO forms"

  (is-form-type (eql value)
    (block tag
      (go tag)
      'value)

    :test type=))

(test (with-load-time-value :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with LOAD-TIME-VALUE forms"

  (is-form-type (eql 2)
    (block nil
      (pprint (load-time-value (return 1)))
      2)

    :test type=))

(test (with-quote :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with QUOTE forms"

  (is-form-type (eql 2)
    (block name
      (something '(return-from name 1))
      2)

    :test type=))


;;; Test MACROLET and SYMBOL-MACROLET forms

(test (with-macrolet :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with MACROLET forms"

  (macrolet ((local-pass (form)
	       `(pass-form ,form)))

    (is-form-type (or (eql 1) (eql 2))
      (block bname
	(macrolet ((finish (form)
		     `(local-pass (return-from bname ,form))))

	  (pprint (+ x y))
	  (finish 1))

	2)

      :test type=)))

(test (with-symbol-macrolet :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with SYMBOL-MACROLET forms"

  (macrolet ((local-pass (form)
	       `(pass-form ,form)))

    (is-form-type (or (eql end) (eql default))
      (block bname
	(symbol-macrolet
	    ((finish-end (local-pass (return-from bname 'end))))

	  (something x finish-end)
	  'default))

      :test type=)))


;;; With other special forms

(test (with-catch :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with CATCH forms"

  (is-form-type (member a b)
    (block b
      (catch 'tag
	(something 1)
	(something (return-from b 'a)))
      'b)

    :test type=))

(test (with-eval-when :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with EVAL-WHEN forms"

  (is-form-type (member 1 2)
    (block nil
      (something 1)
      (eval-when (:compile-toplevel :load-toplevel :execute)
	(something 2)
	(something (return 1)))

      2)

    :test type=))

(test (with-if :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with IF forms"

  (is-form-type (member cond true false)
    (block nil
      (if (something (return 'cond))
	  (return 'true)
	  (return 'false)))

    :test type=))

(test (with-multiple-value-call :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with MULTIPLE-VALUE-CALL forms"

  (is-form-type (member 1 2 3)
    (block nil
      (fn (return 1))
      (multiple-value-call #'f a (return 2))
      3)

    :test type=))

(test (with-multiple-value-prog1 :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with MULTIPLE-VALUE-PROG1 forms"

  (is-form-type (member 1 2 3)
    (block nil
      (fn (return 1))
      (multiple-value-prog1 (+ a b)
	(return 2))
      3)

    :test type=))

(test (with-progn :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with PROGN forms"

  (is-form-type (member a b c)
    (block b
      (progn
	(something x)
	(return-from b 'a)
	(return-from b 'b)
	'c))

    :test type=))

(test (with-progv :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with PROGV forms"

  (is-form-type (member a b c)
    (block b
      (progv '(x y) (list 1 (return-from b 'a))
	(something x)
	(return-from b 'b)
	'c))

    :test type=))

(test (with-setq :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with SETQ forms"

  (is-form-type (member 2 3)
    (block nil
      (setq x 1
	    y (return 2))

      3)

    :test type=))

(test (with-malformed-setq :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with malformed SETQ forms"

  (signals malformed-form-error
    (form-type
     '(block nil
       (setq x 1 y)
       3)

     nil))

  (signals malformed-form-error
    (form-type
     '(block nil
       (setq (return 1) 2)
       3)

     nil)))

(test (with-tagbody :depends-on with-symbol-macrolet)
  "Test FORM-TYPE on BLOCK with TAGBODY forms"

  (is-form-type (member 1 2)
    (block nil
      (symbol-macrolet
	  ((ret3 (return 3)))

	(tagbody
	 ret3
	   (go tag2)
	   (return 1)

	 tag2
	   (return 2)
	   (go ret3))))

    :test type=))

(test (with-the :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with THE forms"

  (is-form-type (member a b)
   (block b1
     (something (the number (return-from b1 'a)))
     'b)

   :test type=))

(test (with-throw :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with THE forms"

  (is-form-type (member a b)
   (block b1
     (something (throw 'tag (return-from b1 'a)))
     'b)

   :test type=))

(test (with-unwind-protect :compile-at :run-time)
  "Test FORM-TYPE on BLOCK with UNWIND-PROTECT forms"

  (is-form-type (member a b c)
    (block b1
      (unwind-protect
	   (something (throw 'tag (return-from b1 'a)))

	(something-else x)
	(return-from b1 'b))

      'c)

    :test type=))
