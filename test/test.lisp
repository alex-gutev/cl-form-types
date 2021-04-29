;;;; test.lisp
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

(defpackage :cl-form-types/test
  (:use :cl-environments-cl
	:cl-form-types
	:alexandria
	:optima

	:fiveam)

  (:shadowing-import-from :fiveam :fail)

  (:export :cl-form-types
	   :test-cl-form-types

	   :in-lexical-environment
	   :is-form-type
	   :form-type=))

(in-package :cl-form-types/test)


;;; Test Suite Definition

(def-suite cl-form-types
    :description "CL-FORM-TYPES master test suite.")

(in-suite cl-form-types)

(defun test-cl-form-types ()
  (run! 'cl-form-types))


;;; Utilities

(defmacro in-lexical-environment ((env-var) &body forms)
  "Evaluate forms in the current lexical environment.

   ENV-VAR is the name of the variable to which the lexical
   environment is bound. This binding is visible to FORMS.

   FORMS is the list of forms which are evaluated in an explicit
   PROGN. The forms are evaluated during macroexpansion, and this form
   is substituted by a quoted list containing the all the return
   values of the last form in FORMS."

  (with-gensyms (expand)
    `(macrolet ((,expand (&environment ,env-var)
		  `',(multiple-value-list (progn ,@forms))))
       (,expand))))

(defmacro is-form-type (type &body (form &key strict) &environment env)
  "Check that a form is of a given type, by FORM-TYPE, in its environment.

   TYPE (not evaluated) is the expected form type, which is be
   compared to the actual type using FORM-TYPE=.

   FORM (not evaluated) is the form of which to determine the type
   using FORM-TYPE. The type is determined in the environment of this
   macro form.

   If STRICT is true the type returned by FORM-TYPE must equal TYPE
   exactly, by EQUAL."

  (let ((form-type (form-type form env)))
    `(is (,(if strict 'equal 'form-type=)
	   ',type
	   ',form-type)

	 "~%Form type of ~s is:~%~%~s~%~%~2T which is not a subtype of:~%~%~s"
	 ',form ',form-type ',type)))

(defun form-type= (expected actual)
  "Check whether ACTUAL is a subtype of EXPECTED."

  (labels ((values-type= (expected actual)
	     (multiple-value-match (values expected actual)
	       (('&rest '&rest) t)
	       ((_ _) (type-equal? expected actual))))

	   (type-equal? (expected actual)
	     (subtypep actual expected)))

    (multiple-value-match (values expected actual)
      (((list* 'values expected)
	(list* 'values actual))
       (every #'values-type= expected actual))

      ((_ (list* 'values actual _))
       (type-equal? expected actual))

      ((_ _)
       (type-equal? expected actual)))))
