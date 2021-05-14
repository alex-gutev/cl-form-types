;;;; block-types.lisp
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

;;;; Determining the type of CL:BLOCK forms

(in-package :cl-form-types)

(define-constant +cl-special-forms+
    '(cl:block cl:catch cl:eval-when cl:flet cl:function
      cl:go cl:if cl:labels cl:let cl:let* cl:load-time-value
      cl:locally cl:macrolet cl:multiple-value-call cl:multiple-value-prog1
      cl:progn cl:progv cl:quote cl:return-from cl:setq cl:symbol-macrolet
      cl:tagbody cl:the cl:throw cl:unwind-protect)

  :test #'equal
  :documentation
  "List of standard common lisp special forms")

(defvar *block-name* nil
  "The name of the BLOCK of which the type is currently being
   determined.")

(defvar *in-block* nil
  "Flag for whether RETURN-FROM forms to the block name *BLOCK-NAME*
   are treated as returning from the block, of which the type is
   currently being determined, (true) or they are treated as returning
   from a block nested in it (false).")

(defvar *local-fns* nil
  "Association list of the types of values returned from the current
   block by RETURN-FROM forms located within functions defined locally
   within the block.")

(defmethod special-form-type ((operator (eql 'cl:block)) operands env)
  (match-form operands
    ((list* (and (type symbol) name)
	    (and (type proper-list) forms))

     (let ((*block-name* name)
	   (*in-block* t)
	   (*local-fns* nil))

       `(or
	 ,(form-type% (lastcar forms) env)
	 ,@(-> (walk-forms forms env)
	     (remove-duplicates :test #'equal)))))))


;;; Code Walking

(defun walk-forms (forms env &optional types)
  "Extract the types of (RETURN-FROM *BLOCK-NAME* ...) from a list of forms.

   The types of the result forms of all RETURN-FROM forms, to the
   block with name *BLOCK-NAME*, located with FORMS, or subforms of,
   are extracted and returned.

   If *IN-BLOCK* is NIL only the type of RETURN-FROM forms located in
   locally defined functions, store in the ALIST *LOCAL-FNS*, that are
   called are returned.

   FORMS is the list of forms to walk.

   ENV is the environment in which the forms are found.

   TYPES is a list of type specifiers, which is appended to the list
   returned by the function.

   Returns a list of type specifiers."

  (reduce
   (lambda (&optional types form)
     (walk-form form env types))

   forms
   :initial-value types))

(defun walk-form (form env types)
  "Extract RETURN-FROM value types from a form (and its subforms)

   FORM is the form.

   ENV is the environment in which the form is found.

   TYPES is a list of type specifiers, which is appended to the
   returned list.

   Returns a list of type specifiers."

  (match (macroexpand form env)
    ((list* op (and (type proper-list) args))
     (walk-list-form op args env types))

    (_ types)))

(defgeneric walk-list-form (operator operands env types)
  (:documentation
   "Extract RETURN-FROM value types from a function call expression.

    OPERATOR is the expression operator.

    OPERANDS is the expression operand list.

    ENV is the environment in which the form is found.

   TYPES is a list of type specifiers, which is appended to the
   returned list.

   Returns a list of type specifiers."))

(defmethod walk-list-form (operator operands env types)
  (match operator
    ((list* 'cl:lambda def)
     (->> (walk-fn-def def env types)
	  (walk-forms operands env)))

    (_
     (when (and (special-operator-p operator)
		(not (member operator +cl-special-forms+)))

       (error 'unknown-special-operator
	      :operator operator
	      :operands operands))

     (->> (cdr (assoc operator *local-fns*))
	  (append types)
	  (walk-forms operands env)))))


;;; Grouping Forms

(defmethod walk-list-form ((operator (eql 'cl:block)) operands env types)
  (match-form operands
    ((list* name forms)
     (let ((*in-block* (not (eq name *block-name*))))
       (walk-forms forms env types)))))

(defmethod walk-list-form ((operator (eql 'cl:eval-when)) operands env types)
  (match-form operands
    ((list* _ forms)
     (walk-forms forms env types))))

(defmethod walk-list-form ((operator (eql 'cl:locally)) operands env types)
  (walk-body operands env types))


;;; Control flow forms

(defmethod walk-list-form ((operator (eql 'cl:go)) operands env types)
  (declare (ignore operands env))
  types)

(defmethod walk-list-form ((operator (eql 'cl:return-from)) operands env types)
  "Method for RETURN-FROM forms.

   If the block name is equal to *BLOCK-NAME* and *IN-BLOCK* is true,
   the type of the result form as if by FORM-TYPE% is returned, as
   well as the types of any nested RETURN-FROM forms."

  (match-form operands
    ((list name result)
     (-<>> (when (and *in-block* (eq name *block-name*))
	     (form-type% result env))

	   (cons <> types)
	   (walk-form result env)))))

(defmethod walk-list-form ((operator (eql 'cl:the)) operands env types)
  (match-form operands
    ((list _ form)
     (walk-form form env types))))

(defmethod walk-list-form ((operator (eql 'cl:tagbody)) operands env types)
  (flet ((walk-form (types form env)
	   (typecase form
	     (symbol types)
	     (otherwise
	      (walk-form form env types)))))

    (reduce (rcurry #'walk-form env) operands :initial-value types)))


;;; Other special forms

(defmethod walk-list-form ((operator (eql 'cl:load-time-value)) operands env types)
  (declare (ignore operands env))

  ;; Does nothing, since LOAD-TIME-VALUE forms since they are
  ;; evaluated in a null lexical environment anyway.

  types)

(defmethod walk-list-form ((operator (eql 'cl:quote)) operands env types)
  (declare (ignore operands env))
  types)


;;; Function forms

(defmethod walk-list-form ((operator (eql 'cl:function)) operands env types)
  (match-form operands
    ((list (list* 'cl:lambda def))
     (walk-fn-def def env types))

    ((list name)
     (append types (cdr (assoc name *local-fns*))))))


;;; Walking function definition forms

(defmethod walk-list-form ((operator (eql 'cl:flet)) operands env types)
  (flet ((extract-function (binding)
	   "Extract the function name from a definition."

	   (match-form binding
	     ((list* (and (type symbol) name) _)
	      name))))

    (match-form operands
      ((list* (and (type proper-list) functions)
	      body)

       (let* ((names (mapcar #'extract-function functions))
	      (ftypes (mapcar (rcurry #'walk-local-fn env nil) functions))
	      (*local-fns* (append ftypes *local-fns*)))

	 (-<> (augment-environment env :function names)
	      (walk-body body <> types)))))))

(defmethod walk-list-form ((operator (eql 'cl:labels)) operands env types)
  (flet ((extract-function (binding)
	   "Extract the function name from a definition."

	   (match-form binding
	     ((list* (and (type symbol) name) _)
	      name))))

    (match-form operands
      ((list* (and (type proper-list) functions)
	      body)

       (let* ((names (mapcar #'extract-function functions))
	      (env (augment-environment env :function names))
	      (ftypes (mapcar (rcurry #'walk-local-fn env nil) functions))
	      (*local-fns* (append ftypes *local-fns*)))

	 (walk-body body env types))))))

(defun walk-local-fn (def env types)
  "Extract RETURN-FROM value types from a local named function definition.

   DEF is the list containing the function definition, with the first
   element being the function name.

   ENV is the environment in which the form is found.

   TYPES is a list of type specifiers, which is appended to the
   returned list.

   Returns a list of type specifiers."

  (match-form def
    ((list* (and (type symbol) name) def)
     (cons name (walk-fn-def def env types)))))

(defun walk-fn-def (def env types)
  "Extract RETURN-FROM value types from a function definition.

   DEF is the list containing the function definition, with the first
   element being the function's lambda-list.

   ENV is the environment in which the form is found.

   TYPES is a list of type specifiers, which is appended to the
   returned list.

   Returns a list of type specifiers."

  (match-form def
    ((list* (and (type proper-list) lambda-list)
	    (and (type proper-list) body))

     (multiple-value-bind (types env)
	 (walk-lambda-list lambda-list env types)

       (walk-body body env types :documentation t)))))

(defun walk-lambda-list (list env types)
  "Extract RETURN-FROM value types from an ordinary lambda list.

   LIST is the lambda-list.

   ENV is the environment in which the function definition is found.

   TYPES is a list of type specifiers, which is appended to the
   returned list.

   Returns two values:

     1. List of type specifiers.

     2. The environment of the function body created by augmenting ENV
         with the argument variables."

  (labels ((walk-args (args env types)
	     (if args
		 (destructuring-bind (spec &rest args) args
		   (multiple-value-bind (types env)
		       (walk-arg spec env types)

		     (walk-args args env types)))

		 (values types env)))

	   (walk-arg (spec env types)
	     (ematch spec
	       ((or (list (list _ name) init sp)
		    (list name init sp)
		    (list name init))

		(values
		 (walk-form init env types)
		 (augment-environment
		  env
		  :variable (list* name (ensure-list sp))))))))

    (multiple-value-bind (required optional rest key allow-other-keys aux)
	(parse-ordinary-lambda-list list)

      (declare (ignore allow-other-keys))

      (multiple-value-bind (types env)
	  (walk-args optional (augment-environment env :variable required) types)

	(multiple-value-bind (types env)
	    (walk-args key (augment-environment env :variable (ensure-list rest)) types)

	  (multiple-value-bind (types env)
	      (walk-args aux (augment-environment env) types)

	    (values
	     types
	     env)))))))

(defun walk-body (body env types &key (documentation nil))
  "Extract RETURN-FROM value types from the body of an environment-modifying form.

   BODY is the list of containing form's body. The first element of
   the list may be a DECLARE expression.

   ENV is the environment of the form body. This excludes declaration
   information in DECLARE expressions located in BODY.

   TYPES is a list of type specifiers, which is appended to the
   returned list.

   DOCUMENTATION is a flag for whether BODY may contain documentation
   strings (true).

   Returns a list of type specifiers."

  (multiple-value-bind (forms decl)
      (parse-body body :documentation documentation)

    (let ((env (augment-environment env :declare (mappend #'cdr decl))))
      (walk-forms forms env types))))


;;; Variable Binding Forms

(defmethod walk-list-form ((operator (eql 'cl:let)) operands env types)
  (flet ((extract-init (binding)
	   (match binding
	     ((list _ init)
	      (list init))))

	 (extract-var (binding)
	   "Extract the variable name from a binding."

	   (match-form binding
	     ((or (and (type symbol) variable)
		  (list* (and (type symbol) variable) _))

	      variable))))

   (match-form operands
     ((list* (and (type proper-list) bindings)
	     body)

      (->> (walk-forms (mappend #'extract-init bindings) env types)
	   (walk-body
	    body
	    (augment-environment
	     env
	     :variable (mapcar #'extract-var bindings))))))))

(defmethod walk-list-form ((operator (eql 'cl:let*)) operands env types)
  (labels ((walk-bindings (bindings env &optional types)
	     (if bindings
		 (destructuring-bind (binding &rest rest) bindings
		   (->> (walk-binding binding env types)
			(walk-bindings
			 rest
			 (augment-environment env :variable (list (extract-var binding))))))

		 (values types env)))

	   (walk-binding (binding env types)
	     (match binding
	       ((list _ init)
		(walk-form init env types))))

	   (extract-var (binding)
	     "Extract the variable name from a binding."

	     (match-form binding
	       ((or (and (type symbol) variable)
		    (list* (and (type symbol) variable) _))

		variable))))

    (match-form operands
      ((list* (and (type proper-list) bindings)
	      body)

       (multiple-value-bind (types env)
	   (walk-bindings bindings env)

	 (walk-body body env types))))))


;;; Lexcial Macro Forms

(defmethod walk-list-form ((operator (eql 'cl:macrolet)) operands env types)
  (flet ((make-macro (def)
	   (match-form def
	     ((list* (and (type symbol) name)
		     (and (type proper-list) lambda-list)
		     (and (type proper-list) body))

	      (list name (enclose-macro name lambda-list body env))))))

    (match-form operands
      ((list* (and (type proper-list) macros)
	      (and (type proper-list) body))

       (-<> (augment-environment env :macro (mapcar #'make-macro macros))
	    (walk-body body <> types))))))

(defmethod walk-list-form ((operator (eql 'cl:symbol-macrolet)) operands env types)
  (match-form operands
    ((list* (and (type proper-list) symbol-macros)
	    (and (type proper-list) body))

     (-<> (augment-environment env :symbol-macro symbol-macros)
	  (walk-body body <> types)))))
