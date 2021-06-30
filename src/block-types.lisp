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

     (flet ((combine (type1 &optional (type2 nil type2-sp))
	      (if type2-sp
		  (combine-values-types 'or type1 type2)
		  type1)))

       (-<> (extract-return-from-types name `(progn ,@forms) env)
	    (remove-duplicates :test #'equal)
	    (reduce #'combine <> :initial-value (form-type% (lastcar forms) env)))))))

(defun local-function-type (name)
  (cdr (assoc name *local-fns* :test #'equal)))


;;; Code Walking

(defvar *block-types* nil
  "List containing the list of value types returned by the current
   BLOCK form.")

(defun extract-return-from-types (name form env)
  "Extract the list of value types returned by a BLOCK form.

   *BLOCK-NAME* is the label naming the block.

   FORM is a PROGN form representing the block body.

   ENV is the environment in which the BLOCK form is contained.

   Returns the list of value types which may be returned from the
   BLOCK form by an explicit RETURN-FROM form."

  (let ((*block-name* name)
        (*block-types*)
        (*in-block* t)
        (*local-fns* nil))
    (block-type-walk-form form env)
    *block-types*))

(defun block-type-walk-form (form env)
  "Walk a subform of a BLOCK form and extract the value types returned
   from the block by RETURN-FROM."

  (labels ((walk (form env)
             (match form
               ((list* operator operands)
                (block-type-walk-list-form operator operands env))

               (_ form))))

    (walk-form #'walk form env :result-type nil)))

(defgeneric block-type-walk-list-form (operator operands env)
  (:documentation
   "Extract RETURN-FROM types from a function call expression form
    appearing within a BLOCK form."))

(defmethod block-type-walk-list-form (operator operands env)
  (when (symbolp operator)
    (when (and (special-operator-p operator)
	       (not (member operator +cl-special-forms+))
               (null (macro-function operator env)))

      (error 'unknown-special-operator
	     :operator operator
	     :operands operands))

    (appendf *block-types* (cdr (assoc operator *local-fns*))))

  (cons operator operands))


;;; Walker Methods for Special Forms

(defmethod block-type-walk-list-form ((operator (eql 'cl:block)) operands env)
  (match-form operands
    ((list* (and (type symbol) name) forms)
     (let ((*in-block* (not (eq name *block-name*))))
       (values
        (block-type-walk-form `(progn ,@forms) env)
        t)))))

(defmethod block-type-walk-list-form ((operator (eql 'cl:return-from)) operands env)
  "Method for RETURN-FROM forms.

   If the block name is equal to *BLOCK-NAME* and *IN-BLOCK* is true,
   the type of the result form as if by FORM-TYPE% is returned, as
   well as the types of any nested RETURN-FROM forms."

  (match-form operands
    ((list (and (type symbol) name) result)
     (when (and *in-block* (eq name *block-name*))
       (push (form-type% result env) *block-types*))

     (cons operator operands))))

(defmethod block-type-walk-list-form ((operator (eql 'cl:load-time-value)) operands env)
  (declare (ignore operands env))

  ;; Does nothing, since LOAD-TIME-VALUE forms are evaluated in a null
  ;; lexical environment, therefore they cannot RETURN-FROM to the
  ;; BLOCK.

  (values nil t))

(defmethod block-type-walk-list-form ((operator (eql 'cl:function)) operands env)
  (declare (ignore env))

  (match operands
    ((list (and (type function-name) name))
     (appendf *block-types* (local-function-type name))))

  (cons operator operands))


;;; Function Definition Forms

(defmethod block-type-walk-list-form ((operator (eql 'cl:flet)) operands env)
  (flet ((function-name (binding)
	   (match-form binding
	     ((list* (and (type function-name) name) _)
	      name))))

    (match-form operands
      ((list* (and (type proper-list) functions)
	      body)

       (let* ((names (mapcar #'function-name functions))
	      (ftypes (mapcar (rcurry #'block-type-walk-local-fn env) functions))
	      (*local-fns* (append ftypes *local-fns*)))

         (values
          (->> (augment-environment env :function names)
               (block-type-walk-form `(cl:locally ,@body)))

          t))))))

(defmethod block-type-walk-list-form ((operator (eql 'cl:labels)) operands env)
  (flet ((function-name (binding)
	   (match-form binding
	     ((list* (and (type function-name) name) _)
	      name))))

    (match-form operands
      ((list* (and (type proper-list) functions)
	      body)

       (let* ((names (mapcar #'function-name functions))
	      (env (augment-environment env :function names))
	      (ftypes (local-function-types names functions env))
	      (*local-fns* (append ftypes *local-fns*)))

         (values
          (block-type-walk-form `(cl:locally ,@body) env)
          t))))))

(defun local-function-types (names functions env)
  "Determine the types of the RETURN-FROM forms in lexical functions.

   NAMES is the list of the function names.

   FUNCTIONS is the list of the function definitions as they appear in
   the FLET or LABELS form.

   ENV is the environment in which the lexical function definitions occur."

  (subst-local-function-types
   (let ((*local-fns*
	  (-> (mapcar
	       (lambda (name)
		 `(,name . ((call ,name))))
	       names)

	      (append *local-fns*))))

     (mapcar (rcurry #'block-type-walk-local-fn env) functions))))

(defun subst-local-function-types (ftypes)
  "Substitute (CALL ...) types with actual types returned by the function.

   In the types of the RETURN-FROM forms, located within a function,
   types which reference the types of the RETURN-FROM forms in another
   function `(CALL ...)` are replaced with the actual list of the
   types of the RETURN-FROM forms in that function.

   FTYPES is an association list with each entry of the form (FN
   . TYPES) where FN is the function name and TYPES is the list of
   type specifiers.

   Returns the association list with the new RETURN-FORM form type
   lists."

  (labels ((replace-call-fn (ftypes fn)
	     (let ((types (-> (cdr (assoc fn ftypes))
			      (remove-call-fn fn))))

	       (mapcar (rcurry #'replace-calls fn types) ftypes)))

	   (remove-call-fn (types fn)
	     (remove `(call ,fn) types :test #'equal))

	   (replace-calls (entry fn new-types)
	     (destructuring-bind (name . old-types) entry
	       (cons name (mappend (rcurry #'replace-call fn new-types) old-types))))

	   (replace-call (type fn types)
	     (match type
	       ((list 'call (equal fn))
		types)

	       (_ (list type)))))

    (nlet process ((ftypes ftypes) (fns ftypes))
      (if fns
	  (-> (replace-call-fn ftypes (caar fns))
	      (process (cdr fns)))
	  ftypes))))

(defun block-type-walk-local-fn (def env)
  "Extract RETURN-FROM value types from a local named function definition.

   DEF is the list containing the function definition, with the first
   element being the function name.

   ENV is the environment in which the form is found.

   Returns the list of value types which may be returned when the
   function is called."

  (let ((*block-types* nil))
    (destructuring-bind (name &rest def) def
      (block-type-walk-form `#'(cl:lambda ,@def) env)

      (cons name *block-types*))))
