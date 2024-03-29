;;;; package.lisp
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

(defpackage :cl-form-types
  (:use :cl-environments-cl
	:agutil
	:alexandria
	:anaphora
	:optima
	:arrows)

  (:export :form-types
	   :form-type
	   :nth-form-type
	   :nth-value-type
	   :custom-form-type
           :*handle-sb-lvars*

	   :malformed-form-error
	   :unknown-special-operator
       :return-default-type
       :*expand-compiler-macros-blacklist*)

  (:intern :walk-form)

  (:documentation
   "Exports utilities for determining the types of common lisp
    forms, based on information found in the environment."))

(defpackage :cl-form-types.walker
  (:use)

  (:import-from :cl-form-types
                :walk-form)

  (:export :walk-form)

  (:documentation
   "Export code-walker used internally for type analysis."))
