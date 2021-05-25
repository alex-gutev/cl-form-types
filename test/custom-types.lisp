;;;; custom-types.lisp
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

;;;; Unit tests for custom type deduction/inference logic.

(defpackage :cl-form-types/test.custom-types
  (:use :cl-environments-cl
	:cl-form-types
	:alexandria

	:fiveam
	:cl-form-types/test))

(in-package :cl-form-types/test.custom-types)


;;; Test Suite Definition

(def-suite custom-form-types
    :description "Test custom type deduction logic in function call expressions"
    :in cl-form-types)

(in-suite custom-form-types)


;;; Tests

(test (make-instance :compile-at :run-time)
  "Test FORM-TYPE on MAKE-INSTANCE forms"

  (symbol-macrolet ((quote-class-a 'class-a))
    (let ((quote-class-b 'class-b))
      (declare (type (eql class-b) quote-class-b)
	       (ignorable quote-class-b))

      (is-form-type my-class
	(make-instance 'my-class :arg1 1 :arg2 b)
	:strict t)

      (is-form-type class-a
	(make-instance quote-class-a :init1 1)
	:strict t)

      (is-form-type class-b
	(make-instance quote-class-b :init1 1)
	:strict t)

      (is-form-type t
	(make-instance the-class :init args)
	:strict t)

      (is-form-type t
	(make-instance)
	:strict t))))

(test make-array
  "Test FORM-TYPE on MAKE-ARRAY forms"

  (is-every equalp
    ('(array * (3)) (form-type '(make-array 3 (first a) #(0 0 0)) nil))
    ('(simple-array t (3))
      (form-type '(make-array 3 :initial-contents #(0 0 0)) nil))
    ('(simple-array t *)
      (form-type '(make-array a :initial-contents #(0 0 0)) nil))
    ('(array t *)
      (form-type '(make-array a :displaced-to #(0 0 0)) nil))))

(test make-string
  "Test FORM-TYPE on MAKE-STRING forms"

  (is (subtypep (form-type '(make-string 3) nil) '(simple-string 3)))
  (is (subtypep (form-type '(make-string a) nil) '(simple-string cl:*)))
  (is (subtypep (form-type '(make-string 3 :element-type 'base-char) nil)
                '(simple-base-string 3)))
  (is (subtypep (form-type '(make-string a :element-type 'base-char) nil)
                '(simple-base-string cl:*))))

(test (coerce :compile-at :run-time)
  "Test FORM-TYPE on COERCE forms"

  (symbol-macrolet ((type1 'vector))
    (let ((type2 'list))
      (declare (type (eql list) type2)
	       (ignorable type2))

      (is-form-type string
	(coerce '(#\h #\e #\l #\l #\o) 'string))

      (is-form-type vector
	(coerce '(1 2 3 4) type1))

      (is-form-type list
	(coerce #(a b c d) type2))

      (is-form-type t
	(coerce x var)
	:strict t)

      (is-form-type t
	(coerce)
	:strict t))))
