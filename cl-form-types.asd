;;;; cl-form-types.asd
;;
;;;; Copyright (c) 2021 Alexander Gutev


(asdf:defsystem #:cl-form-types
  :description "Describe cl-form-types here"
  :author "Alexander Gutev"
  :license  "MIT"
  :version "0.0.1"
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
	       #:optima))
