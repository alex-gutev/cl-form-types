;;;; basic-forms.lisp
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

;;;; Unit tests for basic form types

(defpackage :cl-form-types/test.basic
  (:use :cl-environments-cl
	:cl-form-types
	:alexandria

	:fiveam
	:cl-form-types/test))

(in-package :cl-form-types/test.basic)


;;; Test Suite Definition

(def-suite basic-forms
    :description "Test determining type of basic forms."
    :in cl-form-types)

(in-suite basic-forms)


;;; Definitions used by tests

(defparameter *global-var* "a global variable")

(defconstant +a-global-constant+ 10)

(declaim (type string *global-var*))

(define-symbol-macro character-x #\x)

(defun inc (n)
  (1+ n))

(define-compiler-macro inc (&whole form n)
  (if (constantp n)
      ''x
      form))

(declaim (ftype (function (number) number) inc))

(defmacro pass-form (form)
  form)

(defmacro inc-twice (thing)
  `(inc (inc ,thing)))


;;; Literal Constant Tests

(test (number-constant-type :compile-at :run-time)
  "Test FORM-TYPE on numeric constants"

  (is-form-type (eql 1) 1)
  (is-form-type (eql 12) 12)
  (is-form-type (eql 2.3) 2.3))

(test (character-constant-type :compile-at :run-time)
  "Test FORM-TYPE on character constants"

  (is-form-type (eql #\c) #\c)
  (is-form-type (eql #\x) #\x))

(test (string-constant-type :compile-at :run-time)
  "Test FORM-TYPE on string constants"

  (is-form-type string "hello")
  (is-form-type string "bye"))

(test (array-constant-type :compile-at :run-time)
  "Test FORM-TYPE on array constants"

  (is-form-type array #(1 2 3 4))
  (is-form-type array #(a b c))
  (is-form-type array #2A((1 2 3) (4 5 6))))


;;; Variable Form Tests

(test (constant-type :compile-at :run-time)
  "Test FORM-TYPE on symbols which are defined as constant"

  (is-form-type (eql 10) +a-global-constant+)
  (is-form-type number pi)
  (is-form-type (eql t) t)
  (is-form-type (eql nil) nil)
  (is-form-type (eql :a-keyword) :a-keyword))

(test (variable-type :compile-at :run-time)
  "Test FORM-TYPE on symbols which are defined as variables"

  (let ((local-var #(1 2 3 4)))
    (declare (type simple-array local-var)
	     (ignorable local-var))

    (is-form-type simple-array local-var)
    (is-form-type string *global-var*)
    (is-form-type t not-a-variable :strict t)))

(test (symbol-macro-type :compile-at :run-time)
  "Test FORM-TYPE on symbol-macro forms"

  (symbol-macrolet ((number-10 +a-global-constant+)
		    (the-global-var *global-var*))

    (is-form-type (eql #\x) character-x)
    (is-form-type (eql 10) number-10)
    (is-form-type string the-global-var)))


;;; List Form Tests

(test (function-call-type :compile-at :run-time)
  "Test FORM-TYPE on function call expressions"

  (flet ((add (a b) (+ a b)))
    (declare (ftype (function (integer integer) integer) add)
	     (ignorable #'add))

    (is-form-type number (inc x))
    (is-form-type integer (add (inc x) y))

    (is-form-type t (unknown-function a b c) :strict t)))

(test (macro-form-type :compile-at :run-time)
  "Test FORM-TYPE on macro forms"

  (macrolet ((local-pass (form)
	       `(pass-form ,form)))

    (symbol-macrolet ((inc-100 (inc 100)))

      (is-form-type string (pass-form "hello world"))
      (is-form-type (eql 10) (pass-form +a-global-constant+))
      (is-form-type number (inc-twice (+ x y)))
      (is-form-type number inc-100)
      (is-form-type number (local-pass inc-100))
      (is-form-type string (local-pass "Hello"))
      (is-form-type number (local-pass (inc-twice (* inc-100 z)))))))

(test (compiler-macro-expansion :compile-at :run-time)
  "Test FORM-TYPE with expansion of compiler-macros"

  (is-form-type (eql x)
    (inc 50)
    :expand-compiler-macros t)

  (is-form-type number
    (inc z)
    :expand-compiler-macros t))
