;;;; cl-form-types.asd
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

(asdf:defsystem #:cl-form-types
  :description "Library for determining types of Common Lisp forms."
  :author "Alexander Gutev"
  :license  "MIT"
  :version "0.1"
  :serial t
  :components ((:module
		"src"
		:serial t
		:components
		((:file "package")
		 (:file "form-types"))))

  :depends-on (#:cl-environments
	       #:alexandria
	       #:anaphora
	       #:optima
	       #:arrows
	       #:introspect-environment)

  :in-order-to ((asdf:test-op (asdf:test-op :cl-form-types/test))))

(asdf:defsystem #:cl-form-types/test
  :description "Tests for cl-form-types"
  :author "Alexander Gutev"
  :license "MIT"
  :serial t
  :depends-on (#:cl-form-types #:fiveam)
  :components ((:module
		"test"
		:serial t
		:components
		((:file "test")
		 (:file "basic-forms")
		 (:file "special-forms"))))

  :perform (test-op (o s)
		    (uiop:symbol-call :cl-form-types/test :test-cl-form-types)))
