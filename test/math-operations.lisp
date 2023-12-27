;;;; math-operations.lisp
;;;;
;;;; Copyright 2023 Alexander Gutev <alex.gutev@gmail.com>
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

;;;; Unit tests for type deduction logic of mathematical functions in
;;;; CL standard

(defpackage :cl-form-types/test.math-operations
  (:use :cl-environments-cl
	:cl-form-types
	:alexandria

	:fiveam
	:cl-form-types/test))

(in-package :cl-form-types/test.math-operations)


;;; Test Suite Definition

(def-suite math-operations
    :description "Test type deduction logic for mathematical functions"
    :in cl-form-types)

(in-suite math-operations)


;;; Tests

(test (math+-same-types :compile-at :run-time)
  "Test FORM-TYPE on (+ ...) forms with arguments of same type"

  (let ((a 1) (b 2) (c 3))
    (declare (ignorable a b c))

    (is-form-type integer
      (+ (the fixnum a) (the fixnum b) (the fixnum c))
      :strict t)

    (is-form-type integer
      (+ (the integer a) (the integer b) (the integer c))
      :strict t)

    (is-form-type rational
      (+ (the rational a) (the rational b) (the rational c))
      :strict t)

    (is-form-type float
      (+ (the float a) (the float b) (the float c))
      :strict t)

    (is-form-type single-float
      (+ (the single-float a) (the single-float b) (the single-float c))
      :strict t)

    (is-form-type double-float
      (+ (the double-float a) (the double-float b) (the double-float c))
      :strict t)))

(test (math+-different-types :compile-at :run-time)
  "Test FORM-TYPE on (+ ...) forms with arguments of different type"

  (let ((a 1) (b 2) (c 3))
    (declare (ignorable a b c))

    (is-form-type integer
      (+ (the fixnum a) (the fixnum b) (the integer c))
      :strict t)

    (is-form-type rational
      (+ (the integer a) (the rational b) (the integer c))
      :strict t)

    (is-form-type float
      (+ (the float a) (the single-float b) (the double-float c))
      :strict t)

    (is-form-type float
      (+ (the double-float a) (the single-float b) (the single-float c))
      :strict t)

    (is-form-type number
      (+ (the rational a) (the float b) (the double-float c))
      :strict t)))
