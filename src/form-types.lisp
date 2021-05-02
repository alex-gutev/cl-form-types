;;;; form-types.lisp
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

;;;; Functions for obtaining the types of forms based on the type
;;;; information stored in the environment.

(in-package :cl-form-types)

(define-condition malformed-form-error (error)
  ((form :initarg :form
	 :reader form))

  (:documentation
   "Condition signalling that a form passed to FORM-TYPE is malformed."))

(defun return-default-type (&optional (type t))
  "Invoke the RETURN-DEFAULT-TYPE restart for `MALFORMED-FORM-ERROR' conditions.

   This restart returns the type TYPE for the type of the malformed
   form."

  (invoke-restart 'return-default-type type))

(defmacro with-default-type-restart (&body forms)
  "Evaluate forms in a RESTART-CASE with the RETURN-DEFAULT-TYPE
   restart established."

  (with-gensyms (type)
    `(restart-case (progn ,@forms)
       (return-default-type (,type) ,type))))

(defmacro match-form (form &body clauses)
  "Like MATCH but with a default clause which signals a
   `MALFORMED-FORM-ERROR' condition."

  (with-gensyms (whole)
    `(match ,form
       ,@clauses
       (,whole
	(error 'malformed-form-error :form ,whole)))))

(defvar *constant-eql-types* nil
  "Flag for whether EQL type specifiers should be returned for all constant forms.

   If NIL EQL types are only returned if a form evaluates to a
   constant which is comparable with EQL, such as NUMBERS, CHARACTERS
   and SYMBOLS. If T, an EQL type is returned for every form which
   evaluates to a constant value.")

(defun form-types (forms env &key ((:constant-eql-types *constant-eql-types*)))
  "Determines the type of each form in FORMS.

   FORMS is a list of forms.

   ENV is the environment in which the forms occur.

   :CONSTANT-EQL-TYPES if a flag for whether EQL type specifiers
   should be returned for all constant forms. If NIL EQL types
   specifiers are only returned for constants which are comparable
   with EQL, that is NUMBERS, CHARACTERS and SYMBOLS.

   Returns a list where each element is the type to which the
   corresponding form in FORMS evaluates to"

  (mapcar (rcurry #'form-type env) forms))

(defun nth-form-type (form env &optional (n 0) *constant-eql-types*)
  "Determines the type of the N'th value of a form.

   The difference between this and FORM-TYPE is that, FORM-TYPE
   returns a VALUES type if the form evaluates to multiple values,
   whereas this function returns only the type of the N'th value.

   FORM is the form of which to determine the type.

   ENV is the environment in which the form occurs.

   N is the index of the return value of which to return the type.

   CONSTANT-EQL-TYPES if a flag for whether EQL type specifiers
   should be returned for all constant forms. If NIL EQL types
   specifiers are only returned for constants which are comparable
   with EQL, that is NUMBERS, CHARACTERS and SYMBOLS.

   Returns the type of the N'th return value of FORM. If there is no
   type information for the N'th value, that is FORM does not evaluate
   to multiple values or evaluates to less values than N, NIL is
   returned."

  (labels ((extract-type (type)
	     (match type
	       ((list* 'values types)
		(nth-type types n))

	       ((list 'not type)
		`(not ,(extract-type type)))

	       ((list* (and (or 'or 'and) op)
		       types)
		`(,op ,@(mapcar #'extract-type types)))

	       (type
		(if (zerop n)
		    type
		    nil))))

	   (nth-type (types n)
	     (match types
	       ((list '&rest type)
		type)

	       ((list* '&optional type _)
		(if (zerop n)
		    type
		    (nth-type (cdr types) n)))

	       ((list* type rest)
		(if (zerop n)
		    type
		    (nth-type rest (1- n)))))))

    (extract-type (form-type form env))))

(defun form-type (form env &key ((:constant-eql-types *constant-eql-types*)))
  "Determines the type of a form in an environment.

   FORM is the form of which to determine the type.

   ENV is the environment in which the form occurs.

   :CONSTANT-EQL-TYPES if a flag for whether EQL type specifiers
   should be returned for all constant forms. If NIL EQL types
   specifiers are only returned for constants which are comparable
   with EQL, that is NUMBERS, CHARACTERS and SYMBOLS.

   Returns the type of the value to which FORM evaluates to. Returns a
   VALUES type if FORM evaluates to multiple values. Returns T if the
   type could not be determined."

  (form-type% form env))

(defun form-type% (form env)
  (if (constantp form env)
      (handler-case
	  (constant-type
	   (constant-form-value form env))
	(error () (expand-form-type form env)))

      (expand-form-type form env)))

(defun expand-form-type (form env)
  "Determines the type of a form, in an environment, after macroexpand.

   FORM is the form of which to determine the type.

   ENV is the environment in which the form occurs."

  (multiple-value-bind (form expanded?) (macroexpand-1 form env)
    (cond
      (expanded?
       (form-type% form env))

      ((constantp form env)
       (handler-case
	   (constant-type (constant-form-value form env))
	 (error () t)))

      (t
       (match form
	 ((type symbol)
	  (variable-type form env))

	 ((list* op args)
	  (list-form-type op args env))

	 (_ t))))))

(defun constant-form-value (form env)
  "Determine the value of a constant form.

   FORM is the form of which to determine the value. Must be a form
   for which CONSTANTP returns true.

   ENV is the environment in which FORM is found."

  #+(or ccl sbcl cmucl)
  (introspect-environment:constant-form-value form env)

  ;; Use the following as it has more chance of being successful, than
  ;; a fallback to EVAL, due to evaluating FORM in the correct
  ;; environment.

  #-(or ccl sbcl cmucl)
  (funcall (enclose `(lambda () ,form) env)))


;;; Basic Form Types

(defvar *use-local-declared-types* t
  "Flag for whether the declared types, in the local environment, should be used.

   If NIL the declared types in the global NIL environment are used.

   The purpose of this is to support LOAD-TIME-VALUE, in which the
   form is macroexpanded in the local environment but is actually
   evaluated in the global environment, therefore it does not have
   access to local variables.")

(defun list-form-type (operator arguments env)
  "Determine the type of a list form.

   OPERATOR is the list form operator.

   ARGUMENTS is the list of arguments following the operator.

   ENV is the environment in which the form occurs."

  (match operator
    ((type symbol)
     (function-call-expression-type operator arguments env))

    (_ t)))

(defun function-call-expression-type (operator arguments env)
  "Determine the type of a function call expression.

   OPERATOR is the expression operator, which may be a special
   operator.

   ARGUMENTS is the list of arguments following the operator.

   ENV is the environment in which the expression is found."

  (flet ((get-ftype (decl)
	   (match (cdr (assoc 'ftype decl))
	     ((list 'function _ (and (not (eq '*)) return-type))
	      return-type)

	     (_ t))))

    (multiple-value-bind (type local decl)
	(function-information operator (when *use-local-declared-types* env))

      (declare (ignore local))

      (case type
	(:function
	 (get-ftype decl))

	(:special-form
	 (special-form-type operator arguments env))

	(otherwise t)))))

(defun variable-type (variable env)
  "Determine the type of a variable.

   VARIABLE is the symbol naming the variable. This may name a
   constant as well.

   ENV is the environment in which the variable is found."

  (flet ((get-vtype (decl)
	   (aif (assoc 'type decl)
		(cdr it)
		t)))

    (multiple-value-bind (type local decl)
	(variable-information variable (when *use-local-declared-types* env))

      (declare (ignore local))

      (case type
	(:constant
	 `(eql ,(eval variable)))

	((:lexical :special)
	 (get-vtype decl))

	(otherwise t)))))

(defun constant-type (value)
  "Return the type specifier of a constant value.

   If the value is a CHARACTER, NUMBER or SYMBOL an EQL type specifier
   is returned. Otherwise the type specifier returned by TYPE-OF is
   returned.

   VALUE is the constant value."

  (if *constant-eql-types*
      `(eql ,value)

      (typecase value
	((or number character symbol) `(eql ,value))
	(otherwise
	 (type-of value)))))


;;; Special Form Types

(defgeneric special-form-type (operator operands env)
  (:documentation
   "Determine the type of a special form.

    OPERATOR is the form operator, which must name a special operator.

    OPERANDS is the for argument list.

    ENV is the environment in which the form is found.")

  (:method (operator operands env)
    (declare (ignore operator operands env))
    t))

(defmethod special-form-type :around (operator operands env)
  (with-default-type-restart
    (call-next-method)))

;;;; QUOTE and FUNCTION

(defmethod special-form-type ((operator (eql 'cl:quote)) operands env)
  (match-form operands
    ((list thing)
     (constant-type thing))))

(defmethod special-form-type ((operator (eql 'cl:function)) operands env)
  (match operands
    ((list (and
	    (or (type symbol)
		(list 'cl:setf (type symbol)))
	    name))

     (multiple-value-bind (type local decl)
	 (function-information name env)

       (declare (ignore local))

       (case type
	 ((or nil :function)
	  (or (cdr (assoc 'ftype decl))
	      'cl:function))

	 (otherwise t))))

    (_ 'cl:function)))

;;;; THE

(defmethod special-form-type ((operator (eql 'cl:the)) operands env)
  (match-form operands
    ((list type _)
     type)))

;;;; LOAD-TIME-VALUE

(defmethod special-form-type ((operator (eql 'cl:load-time-value)) operands env)
  (match-form operands
    ((list* value _)
     (let ((*use-local-declared-types* nil))
       (form-type% value env)))))


;;;; SETQ

(defmethod special-form-type ((operator (eql 'cl:setq)) operands env)
  (match-form operands
    ((guard operands
	    (and (proper-list-p operands)
		 (evenp (length operands))))
     (form-type% (lastcar operands) env))))

;;;; Conditionals

(defmethod special-form-type ((operator (eql 'cl:if)) operands env)
  (match-form operands
    ((list _ if-true if-false)
     `(or ,(form-type% if-true env)
	  ,(form-type% if-false env)))

    ((list _ if-true)
     `(or ,(form-type% if-true env) null))))

;;;; Multiple value call and PROG1

(defmethod special-form-type ((operator (eql 'cl:multiple-value-prog1)) operands env)
  (match-form operands
    ((list* first-form _)
     (form-type% first-form env))))


;;; Grouping Forms

(defmethod special-form-type ((operator (eql 'cl:progn)) operands env)
  (match-form operands
    ((type proper-list)
     (form-type% (lastcar operands) env))))

(defmethod special-form-type ((operator (eql 'cl:progv)) operands env)
  (match-form operands
    ((list* _ _ (and (type proper-list) forms))
     (form-type% (lastcar forms) env))))

(defmethod special-form-type ((operator (eql 'cl:eval-when)) operands env)
  (match-form operands
    ((list* (and (type proper-list) situation)
	    (and (type proper-list) forms))

     (if (or (member :execute situation)
	     (member 'eval situation))
	 (form-type% (lastcar forms) env)
	 'null))))

(defmethod special-form-type ((operator (eql 'cl:unwind-protect)) operands env)
  (match-form operands
    ((list* form _)
     (form-type% form env))))

(defmethod special-form-type ((operator (eql 'cl:locally)) operands env)
  (match-form operands
    ((type proper-list)
     (multiple-value-bind (body declarations)
	 (parse-body operands :documentation nil)

       (form-type%
	(lastcar body)
	(augment-environment
	 env
	 :variable (extract-declared-vars declarations)
	 :function (extract-declared-funcs declarations)
	 :declare (mappend #'cdr declarations)))))))

#-sbcl
(defun extract-declared-vars (declarations)
  (declare (ignore declarations))
  nil)

#-sbcl
(defun extract-declared-funcs (declarations)
  (declare (ignore declarations))
  nil)

;;; SBCL does not record declarations added to the environment, by
;;; AUGMENT-ENVIRONMENT, if the corresponding variable/function is not
;;; added as well, even if it is already present in the environment.

#+sbcl
(defun extract-declared-vars (declarations)
  "Extract variable names from TYPE declaration expressions.

   DECLARATIONS is a list of DECLARE expressions, with `DECLARE` in
   the CAR of each element of DECLARATIONS."

  (labels ((extract-vars (decl-expression)
	     "Extract variable names from DECLARE TYPE expressions."

	     (match-form decl-expression
	       ((list* _ (and (type proper-list) decls))
		(mappend #'extract-var decls))))

	   (extract-var (decl)
	     "Extract variable names from TYPE declarations."

	     (match decl
	       ((list* 'type _
		       (guard vars
			      (and (proper-list-p vars)
				   (every #'symbolp vars))))
		vars))))

    (mappend #'extract-vars declarations)))

#+sbcl
(defun extract-declared-funcs (declarations)
  "Extract function names from FTYPE declaration expressions.

   DECLARATIONS is a list of DECLARE expressions, with `DECLARE` in
   the CAR of each element of DECLARATIONS."

  (labels ((extract-funcs (decl-expression)
	     "Extract function names from DECLARE FTYPE expressions."

	     (match-form decl-expression
	       ((list* _ (and (type proper-list) decls))
		(mappend #'extract-func decls))))

	   (function-name-p (name)
	     (match name
	       ((or (type symbol)
		    (list 'cl:setf (type symbol)))
		t)))

	   (extract-func (decl)
	     "Extract function names from FTYPE declarations."

	     (match decl
	       ((list* 'ftype _
		       (guard fns
			      (and (proper-list-p fns)
				   (every #'function-name-p fns))))
		fns))))

    (mappend #'extract-funcs declarations)))


;;; Control Flow Context

(defmethod special-form-type ((operator (eql 'cl:tagbody)) operands env)
  (declare (ignore operands env))
  nil)

;;;; Control Transfer Forms

;; These all return NIL since they do not return any value, but rather
;; execute a jump.

(defmethod special-form-type ((operator (eql 'cl:go)) operands env)
  (declare (ignore operands env))
  nil)

(defmethod special-form-type ((operator (eql 'cl:throw)) operands env)
  (declare (ignore operands env))
  nil)

(defmethod special-form-type ((operator (eql 'cl:return-from)) operands env)
  (declare (ignore operands env))
  nil)


;;; Local Macro Definitions

(defmethod special-form-type ((operator (eql 'cl:macrolet)) operands env)
  (flet ((make-macro (def)
	   (match-form def
	     ((list* (and (type symbol) name)
		     (and (type proper-list) lambda-list)
		     (and (type proper-list) body))

	      (list name (enclose-macro name lambda-list body env))))))

    (match-form operands
      ((list* (and (type proper-list) macros)
	      (and (type proper-list) body))

       (multiple-value-bind (body declarations)
	   (parse-body body :documentation nil)

	 (form-type%
	  (lastcar body)
	  (augment-environment
	   env
	   :variable (extract-declared-vars declarations)
	   :function (extract-declared-funcs declarations)
	   :macro (mapcar #'make-macro macros)
	   :declare (mappend #'cdr declarations))))))))

(defmethod special-form-type ((operator (eql 'cl:symbol-macrolet)) operands env)
  (match-form operands
    ((list* (and (type proper-list) symbol-macros)
	    (and (type proper-list) body))

     (multiple-value-bind (body declarations)
	 (parse-body body :documentation nil)

       (form-type%
	(lastcar body)
	(augment-environment
	 env
	 :variable (extract-declared-vars declarations)
	 :function (extract-declared-funcs declarations)
	 :symbol-macro symbol-macros
	 :declare (mappend #'cdr declarations)))))))


;;; Local Variable Binding Forms

(defmethod special-form-type ((operator (eql 'cl:let)) operands env)
  (let-form-type operands env))

(defmethod special-form-type ((operator (eql 'cl:let*)) operands env)
  (let-form-type operands env))

(defun let-form-type (operands env)
  "Determine the type of a LET/LET* form.

   OPERANDS is the list of operands to the form.

   ENV is the environment in which the form is found."

  (flet ((extract-var (binding)
	   "Extract the variable name from a binding."

	   (match-form binding
	     ((or (and (type symbol) variable)
		  (list* (and (type symbol) variable) _))

	      (list variable)))))

    (match-form operands
      ((list* (and (type proper-list) bindings)
	      (and (type proper-list) body))

       (multiple-value-bind (body declarations)
	   (parse-body body :documentation nil)

	 (form-type%
	  (lastcar body)

	  (augment-environment
	   env
	   :variable (-> (mappend #'extract-var bindings)
			 (union (extract-declared-vars declarations)))

	   :function (extract-declared-funcs declarations)

	   :declare (mappend #'cdr declarations))))))))


;;; Local Function Binding Forms

(defmethod special-form-type ((operator (eql 'cl:flet)) operands env)
  (flet-form-type operands env))

(defmethod special-form-type ((operator (eql 'cl:labels)) operands env)
  (flet-form-type operands env))

(defun flet-form-type (operands env)
  "Determine the type of a FLET/LABELS form.

   OPERANDS is the list of operands to the form.

   ENV is the environment in which the form is found."

  (flet ((extract-function (binding)
	   "Extract the function name from a definition."

	   (match-form binding
	     ((list* (and (type symbol) name) _)
	      (list name)))))

    (match-form operands
      ((list* (and (type proper-list) functions)
	      (and (type proper-list) body))

       (multiple-value-bind (body declarations)
	   (parse-body body :documentation nil)

	 (form-type%
	  (lastcar body)

	  (augment-environment
	   env
	   :variable (extract-declared-vars declarations)

	   :function (-> (mappend #'extract-function functions)
			 (union (extract-declared-funcs declarations)))

	   :declare (mappend #'cdr declarations))))))))
