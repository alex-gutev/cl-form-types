;;;; walker.lisp
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

;;;; Common Lisp code walker

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

(defvar *walk-result-type* nil
  "The type of result which should be returned by the walk function.")

(defvar *walk-fn* nil
  "The walk function to call on each form.")

(defun walk-form (fn form env &key (result-type 'list))
  "Apply a function on each subform in a form.

   FN:

      A function of two arguments that is called for each subform in
      FORM, with two arguments: the form and the environment in which
      it occurs.

      It should return the following return values:

       1. The new form. The subforms of this form are walked and it is
          substituted in place of the old form, in the result returned
          by WALK-FORM.

       2. A Boolean flag. If true the subforms of the form returned
          in (1) are not walked, and it is substituted as it is in the
          result. Otherwise the subforms are walked.

   FORM:

     The form to walk. The function is first applied on FORM itself,
     then on its subforms.


   ENV:

     The environment in which FORM is found.

   RESULT-TYPE:

     A symbol indicating the type of result that should be returned
     from WALK-FORM:

     LIST - The new form, built out of the subforms returned by FN, is
            returned. This is the default.

     NIL  - No new form is constructed, meaning the return value of FN
            is used only to determine which forms to walk next.

   Returns the new transformed form, if RESULT-TYPE is LIST."

  (check-type result-type (member list nil))

  (let ((*walk-result-type* result-type)
        (*walk-fn* fn))

    (walk-form% form env)))


;;; Utilities

(defmacro with-result ((result form) &body forms)
  "Evaluate FORMS returning the correct result type per *WALK-RESULT-TYPE*

   RESULT is a symbol naming the symbol-macro introduced to FORMS, by
   SYMBOL-MACROLET, which expands to FORM.

   If *WALK-RESULT-TYPE* is LIST, FORMS are evaluated in an implicit
   PROGN. These may reference the binding to the symbol-macro named by
   RESULT.

   If *WALK-RESULT-TYPE* is NIL, FORM is returned directly."

  `(symbol-macrolet ((,result ,form))
     (ecase *walk-result-type*
       (list
        ,@forms)

       ((nil) ,result))))


;;; Internals

(defun walk-forms (forms env)
  "Walk each form in FORMS as if by WALK-FORM.

   FORMS is a list of FORMS.

   ENV is the environment in which to walk the forms.

   Returns a list containing the results of walking each form, if
   *WALK-RESULT-TYPE* is LIST. Otherwise returns NIL."

  (map *walk-result-type* (rcurry #'walk-form% env) forms))

(defun walk-form% (form env)
  "Internal code-walker function, that calls the current walk
   function (*WALK-FN*)."

  (multiple-value-bind (form stop)
      (funcall *walk-fn* form env)

    (if stop
        form
        (walk-new-form form env))))

(defun walk-new-form (form env)
  "Walk the subforms in a new form returned by the walk function."

  (match-form form
    ((type atom)
     (multiple-value-bind (form expanded?)
         (macroexpand-1 form env)

       (if expanded?
           (walk-form% form env)
           form)))

    ((list* op (and (type proper-list) args))
     (walk-list-form op args env))))

(defgeneric walk-list-form (operator operands env)
  (:documentation
   "Walk a list form.

    OPERATOR is the form operator.

    OPERANDS is the form's operand list.

    ENV is the environment in which to walk the form."))

(defmethod walk-list-form (operator operands env)
  (match-form operator
    ((list* 'cl:lambda def)

     (let ((operator (walk-fn-def def env)))
       (with-result (result (walk-forms operands env))
         `((cl:lambda ,@operator) ,@result))))

    ((type symbol)
     (multiple-value-bind (form expanded?)
         (macroexpand-1 (cons operator operands) env)

       (cond
         (expanded?
          (walk-form% form env))

         (t
          (when (and (special-operator-p operator)
		     (not (member operator +cl-special-forms+)))

            (error 'unknown-special-operator
	           :operator operator
	           :operands operands))

          (with-result (result (walk-forms operands env))
            `(,operator ,@result))))))))


;;; Grouping Forms

(defmethod walk-list-form ((operator (eql 'cl:block)) operands env)
  (match-form operands
    ((list* (and (type symbol) name) forms)
     (with-result (result (walk-forms forms env))
       `(cl:block ,name ,@result)))))

(defmethod walk-list-form ((operator (eql 'cl:eval-when)) operands env)
  (match-form operands
    ((list* situation forms)
     (with-result (result (walk-forms forms env))
       `(cl:eval-when ,situation ,@result)))))

(defmethod walk-list-form ((operator (eql 'cl:locally)) operands env)
  (with-result (result (walk-body operands env))
    `(cl:locally ,@result)))


;;; Control flow forms

(defmethod walk-list-form ((operator (eql 'cl:go)) operands env)
  (declare (ignore env))
  (cons operator operands))

(defmethod walk-list-form ((operator (eql 'cl:return-from)) operands env)
  (match-form operands
    ((list (and (type symbol) name) form)
     (with-result (result (walk-form% form env))
       `(cl:return-from ,name ,result)))))

(defmethod walk-list-form ((operator (eql 'cl:tagbody)) operands env)
  (flet ((walk-form (form)
	   (typecase form
	     (symbol form)
	     (otherwise
	      (walk-form% form env)))))

    (with-result (result (map *walk-result-type* #'walk-form operands))
      `(cl:tagbody ,@result))))


;;; Other special forms

(defmethod walk-list-form ((operator (eql 'cl:load-time-value)) operands env)
  (destructuring-bind (form &optional read-only-p) operands
    (with-result (result (walk-form% form env))
      `(cl:load-time-value ,result ,read-only-p))))

(defmethod walk-list-form ((operator (eql 'cl:quote)) operands env)
  (declare (ignore env))
  (cons operator operands))

(defmethod walk-list-form ((operator (eql 'cl:setq)) operands env)
  (flet ((symbol-macro? (sym)
           (eq :symbol-macro
               (variable-information sym env))))

    (unless (evenp (length operands))
      (error 'malformed-form-error :form (cons 'cl:setq operands)))

    (let ((setf? nil))
      (loop
         for var in operands by #'cddr
         do
           (unless (symbolp var)
             (error 'malformed-form-error :form (cons 'cl:setq operands)))

           (when (symbol-macro? var)
             (setf setf? t)))

      (if setf?
          (walk-form% (cons 'cl:setf operands) env)

          `(cl:setq
            ,@(loop
                 for (var form) on operands by #'cddr
                 append (list var (walk-form% form env))))))))

(defmethod walk-list-form ((operator (eql 'cl:the)) operands env)
  (match-form operands
    ((list type form)
     (with-result (result (walk-form% form env))
       `(cl:the ,type ,result)))))


;;; Function forms

(defmethod walk-list-form ((operator (eql 'cl:function)) operands env)
  (match-form operands
    ((list (list* 'cl:lambda def))
     (with-result (result (walk-fn-def def env))
       `#'(cl:lambda ,@result)))

    #+clisp
    ((list name (list* 'cl:lambda def))
     (with-result (result (walk-local-fn (cons name def) env))
       (destructuring-bind (name . def) result
         `(function ,name (cl:lambda ,@def)))))

    #+ecl
    ((list (list* 'ext:lambda-block name def))
     (with-result (result (walk-local-fn (cons name def) env))
       (destructuring-bind (name . def) result
         `(function (ext:lambda-block ,name ,@def)))))

    #+abcl
    ((list (list* 'system:named-lambda name def))
     (with-result (result (walk-local-fn (cons name def) env))
       (destructuring-bind (name . def) result
         `(function (system:named-lambda ,name ,@def)))))

    ((list name)
     `#',name)))


;;; Walking function definition forms

(defmethod walk-list-form ((operator (eql 'cl:flet)) operands env)
  (flet ((function-name (binding)
	   (match-form binding
	     ((list* (and (type symbol) name) _)
	      name))))

    (match-form operands
      ((list* (and (type proper-list) functions) body)

       (let* ((names (mapcar #'function-name functions))
	      (functions (mapcar (rcurry #'walk-local-fn env) functions)))

         (with-result (result (walk-body body env :function names))
           `(cl:flet ,functions ,@result)))))))

(defmethod walk-list-form ((operator (eql 'cl:labels)) operands env)
  (flet ((function-name (binding)
	   (match-form binding
	     ((list* (and (type symbol) name) _)
	      name))))

    (match-form operands
      ((list* (and (type proper-list) functions)
	      body)

       (let* ((names (mapcar #'function-name functions))
	      (env (augment-environment env :function names))
	      (functions (mapcar (rcurry #'walk-local-fn env) functions)))

         (with-result (result (walk-body body env :function names))
           `(cl:labels ,functions ,@result)))))))

(defun walk-local-fn (def env)
  "Walk a local function definition as by FLET or LABELS.

   DEF the local function definition.

   ENV the environment in which to walk the definition."

  (match-form def
    ((list* (and (type symbol) name) def)
     (cons name (walk-fn-def def env)))))

(defun walk-fn-def (def env)
  "Walk a function definition.

   DEF the function definition starting from the LAMBDA-LIST.

   ENV the environment in which to walk the definition."

  (match-form def
    ((list* (and (type proper-list) lambda-list)
	    (and (type proper-list) body))

     (multiple-value-bind (lambda-list variables)
	 (walk-lambda-list lambda-list env)

       (with-result (result (walk-body body env :variable variables :documentation t))
         `(,lambda-list
           ,@result))))))

(defun walk-lambda-list (list env)
  "Walk an ordinary lambda-list.

   LIST is the lambda-list.

   ENV is the environment in which the correspondign function
   definition is found.

   Returns two values:

     1. The new lambda-list.

     2. List of variables introduced by the lambda-list."

  (labels ((walk-args (args env)
             (loop
                for arg in args
                for (new-arg names) =
                  (multiple-value-list (walk-arg arg env))
                collect new-arg into new-args
                do
                  (setf env (augment-environment env :variable names))

                finally
                  (return (values new-args env))))

	   (walk-arg (spec env)
	     (ematch spec
               ((list (list key name) init sp)
                (values
                 `((,key ,name) ,(walk-form% init env) ,@(ensure-list sp))
                 (list* name (ensure-list sp))))

               ((list name init sp)
                (values
                 `(,name ,(walk-form% init env) ,@(ensure-list sp))
                 (list* name (ensure-list sp))))

               ((list name init)(values
                 `(,name ,(walk-form% init env))
                 (list name))))))

    (multiple-value-bind (required optional rest key allow-other-keys aux keyp)
	(parse-ordinary-lambda-list list)

      (multiple-value-bind (optional env)
	  (walk-args optional (augment-environment env :variable required))

	(multiple-value-bind (key env)
	    (walk-args key (augment-environment env :variable (ensure-list rest)))

	  (let ((aux (walk-args aux env)))
            (values
             (append
              required
              (when optional `(&optional ,@optional))
              (when rest `(&rest ,rest))
              (when keyp '(&key))
              key
              (when allow-other-keys '(&allow-other-keys))
              (when aux `(&aux ,@aux)))

             (append
              required
              (mapcar #'car optional)
              (mappend #'cddr optional)
              (ensure-list rest)
              (mapcar #'cadar key)
              (mappend #'cddr key)
              (mapcar #'car aux)))))))))

(defun walk-body (body env &key variable function symbol-macro macro documentation)
  "Walk the body of a form which modifies the lexical environment.

   BODY is the list containing the form's body. The first element of
   the list may be a DECLARE expression.

   ENV is the environment of the form body. This excludes declaration
   information in DECLARE expressions located in BODY.

   DOCUMENTATION is a flag for whether BODY may contain documentation
   strings (true).

   The remaining keyword arguments are additional arguments to pass to
   AUGMENT-ENVIRONMENT on environment ENV.

   Returns the new form body."

  (multiple-value-bind (forms decl docstring)
      (parse-body body :documentation documentation)

    (let ((env (augment-environment
        	env
        	:variable variable
        	:function function
        	:symbol-macro symbol-macro
        	:macro macro
        	:declare (mappend #'cdr decl))))

      (with-result (result (walk-forms forms env))
        (append
         (ensure-list docstring)
         (ensure-list decl)
         result)))))


;;; Variable Binding Forms

(defmethod walk-list-form ((operator (eql 'cl:let)) operands env)
  (flet ((walk-binding (binding)
	     (match-form binding
               ((or (and (type symbol) var)
                    (list (and (type symbol) var)))

                (list var))

	       ((list (and (type symbol) var) init)
                (list var
		      (walk-form% init env)))))

	   (var-name (binding)
	     (match-form binding
	       ((or (and (type symbol) variable)
		    (list* (and (type symbol) variable) _))

		variable))))

   (match-form operands
     ((list* (and (type proper-list) bindings) body)

      (let ((bindings (mapcar #'walk-binding bindings)))
        (with-result (result (walk-body body env :variable (mapcar #'var-name bindings)))
          `(cl:let ,bindings ,@result)))))))

(defmethod walk-list-form ((operator (eql 'cl:let*)) operands env)
  (labels ((walk-bindings (bindings env)
             (loop
                for binding in bindings
                collect (walk-binding binding env)
                do
                  (setf env (augment-environment env :variable (list (var-name binding))))))

	   (walk-binding (binding env)
	     (match-form binding
               ((or (and (type symbol) var)
                    (list (and (type symbol) var)))

                (list var))

	       ((list (and (type symbol) var) init)
                (list var
		      (walk-form% init env)))))

	   (var-name (binding)
	     (match-form binding
	       ((or (and (type symbol) variable)
		    (list* (and (type symbol) variable) _))

		variable))))

    (match-form operands
      ((list* (and (type proper-list) bindings)
	      body)

       (let* ((bindings (walk-bindings bindings env))
              (names (mapcar #'var-name bindings)))

         (with-result (result (walk-body body env :variable names))
           `(cl:let* ,bindings ,@result)))))))


;;; Lexcial Macro Forms

(defmethod walk-list-form ((operator (eql 'cl:macrolet)) operands env)
  (flet ((make-macro (def)
	   (match-form def
	     ((list* (and (type symbol) name)
		     (and (type proper-list) lambda-list)
		     (and (type proper-list) body))

	      (list name (enclose-macro name lambda-list body env))))))

    (match-form operands
      ((list* (and (type proper-list) macros)
	      (and (type proper-list) body))

       (with-result (result (walk-body body env :macro (mapcar #'make-macro macros)))
         `(cl:macrolet ,macros ,@result))))))

(defmethod walk-list-form ((operator (eql 'cl:symbol-macrolet)) operands env)
  (match-form operands
    ((list* (and (type proper-list) symbol-macros)
	    (and (type proper-list) body))

     (with-result (result (walk-body body env :symbol-macro symbol-macros))
       `(cl:symbol-macrolet ,symbol-macros ,@result)))))


;;; SBCL specific forms

#+sbcl
(defmethod walk-list-form ((operator (eql 'sb-ext:truly-the)) operands env)
  (match-form operands
    ((list type form)
     (with-result (result (walk-form% form env))
       `(cl:the ,type ,result)))))

#+sbcl
(defmethod walk-list-form ((operator (eql 'sb-kernel:the*)) operands env)
  (match-form operands
    ((list type form)
     (with-result (result (walk-form% form env))
       `(cl:the ,type ,result)))))

#+sbcl
(defmethod walk-list-form ((operator (eql 'sb-int:named-lambda)) operands env)
  (with-result (result (walk-local-fn operands env))
    `(sb-int:named-lambda ,@result)))


;;; CCL specific forms

#+ccl
(defmethod walk-list-form ((operator (eql 'ccl::nfunction)) operands env)
  (match-form operands
    ((list name (list* 'cl:lambda def))
     (with-result (result (walk-local-fn (cons name def) env))
       (destructuring-bind (name . def) result
         `(ccl::nfunction ,name (cl:lambda ,@def)))))))

#+ccl
(defmethod walk-list-form ((operator (eql 'ccl::compiler-let)) operands env)
  (match-form operands
    ((list* bindings body)
     (with-result (result (walk-forms body env))
       `(ccl::compiler-let ,bindings ,@result)))))


;;; ECL specific forms

#+ecl
(defmethod walk-list-form ((operator (eql 'cl:multiple-value-bind)) operands env)
  "ECL has a buggy macroexpansion for MULTIPLE-VALUE-BIND which
   results in an error at runtime if more/less values are returned
   than expected."

  (match-form operands
    ((list* (and (type proper-list) vars) form body)
     (let ((form (walk-form% form env)))
       (with-result (result (walk-body body env :variable vars))
         `(cl:multiple-value-bind ,vars ,form
            ,@result))))))


;;; Clisp specific forms

#+clisp
(defmethod walk-list-form ((operator (eql 'system::function-macro-let)) operands env)
  (match-form operands
    ((list* (and (type proper-list) fns) body)
     (with-result (result (walk-body body env :function (mapcar #'first fns)))
       `(system::function-macro-let ,fns ,@result)))))
