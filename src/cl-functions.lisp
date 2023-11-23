;;; functions.lisp
;;;
;;; Adapted from trivial-form-type / inferrers.lisp

;;; MIT License
;;;
;;; Copyright (c) 2021 Shubhamkar Ayare
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(in-package :cl-form-types)

(defmacro lm (&rest var-body)
  `(lambda ,(butlast var-body)
     ,@(last var-body)))

(defun maybe-constant-form-value (form env &key default)
  (match (nth-form-type form env 0 t *expand-compiler-macros*)
    ((list (or 'eql 'member) value)
     (values value t))

    (_
     (values default nil))))

(defmethod custom-form-type ((first (eql 'make-instance)) form env)
  (match form
    ((list* class-form _)
     (maybe-constant-form-value class-form env :default t))

    (_ t)))

(defmethod custom-form-type ((first (eql 'make-array)) form env)
  (values
   (let* ((dimensions (let ((dim-form (first form)))
                        (if (constantp dim-form env)
                            (ensure-list (constant-form-value dim-form env))
                            'cl:*)))
          ;; Make sure that a keyword argument does not
          ;; come from a non-constant form, for example:
          ;;   (make-array 2 (identity :displaced-to) #(0 0 0))
          (constant-key-forms-p (loop :for i :from 0
                                      :for sub-form :in (cdr form)
                                      ;; :do (print (list i sub-form
                                      ;;                  (constantp sub-form *environment*)))
                                      :always (or (oddp i)
                                                  (constantp sub-form env)))))
     (if (not constant-key-forms-p)
         `(array * ,dimensions)
         (let* ((element-type (let ((key-pos
                                      (position
                                       :element-type form
                                       :key (lm form
                                                (maybe-constant-form-value form env)))))
                                (if (not key-pos)
                                    t
                                    (let ((elt-form (elt form (1+ key-pos))))
                                      (maybe-constant-form-value elt-form env
                                                                 :default 'cl:*)))))

                ;; Assume that we can know if or not the array is adjustable

                (adjustable-p-p t)
                (adjustable-p   (let ((key-pos (position :adjustable form)))
                                  (if (not key-pos)
                                      nil
                                      (let ((value-form (elt form (+ 1 key-pos))))
                                        (if (constantp value-form env)
                                            ;; It may be simple if it is not adjustable
                                            (constant-form-value value-form env)
                                            ;; In this case, we do not even know if or
                                            ;; not the array is adjustable
                                            (setq adjustable-p-p nil))))))
                (fill-pointer-p (find :fill-pointer form
                                      :key (lm form (maybe-constant-form-value form  env))))
                (displaced-p    (intersection
                                 '(:displaced-index-offset :displaced-to)
                                 form
                                 :key (lm form (maybe-constant-form-value form env)))))

           (let ((simple-p (if (null adjustable-p-p)
                               nil ; we cannot tell if or not the array is adjustable
                               (and (null adjustable-p)
                                    (null fill-pointer-p)
                                    (null displaced-p)))))

             (if simple-p
                 `(simple-array ,element-type ,dimensions)
                 `(array ,element-type ,dimensions))))))
   t))

(defmethod custom-form-type ((first (eql 'make-string)) form env)
  (values
   (let* ((length (let ((length-form (first form)))
                    (if (constantp length-form env)
                        (constant-form-value length-form env)
                        'cl:*)))
          ;; Make sure that a keyword argument does not
          ;; come from a non-constant form, for example:
          ;;   (make-array 2 (identity :displaced-to) #(0 0 0))
          (constant-key-forms-p (loop :for i :from 0
                                      :for sub-form :in (cdr form)
                                      ;; :do (print (list i sub-form
                                      ;;                  (constantp sub-form env)))
                                      :always (or (oddp i)
                                                  (constantp sub-form env)))))

     (cond ((type= 'character 'base-string)
            `(simple-base-string ,length))
           ((not constant-key-forms-p)
            `(simple-string ,length))
           (t
            (let* ((element-type
                     (if-let (key-pos
                              (position
                               :element-type form
                               :key (lm form (maybe-constant-form-value form env))))
                       (let ((elt-form (elt form (1+ key-pos))))
                         (maybe-constant-form-value elt-form env :default 'cl:*))
                       'character)))
              (if (type= element-type 'base-char)
                  `(simple-base-string ,length)
                  `(simple-string ,length))))))
   t))

(defmethod custom-form-type ((first (eql 'aref)) args env)
  (let ((form-type (introspect-environment:typexpand (form-type (first args) env))))
    (match form-type
      ((list* (or 'cl:array 'cl:simple-array) element-type _)
       (values (case element-type
                 (cl:* t)
                 (t element-type))
               t))
      (_
       (values t nil)))))

(defmethod custom-form-type ((first (eql 'row-major-aref)) args env)
  (let ((form-type (introspect-environment:typexpand (form-type (first args) env))))
    (match form-type
      ((list* (or 'cl:array 'cl:simple-array) element-type _)
       (values (case element-type
                 (cl:* t)
                 (t element-type))
               t))
      (_
       (values t nil)))))

(defmethod custom-form-type ((first (eql 'values)) args env)
  (values `(values ,@(loop :for form :in args
                           :collect (nth-form-type form env)))
          t))

(defmethod custom-form-type ((first (eql 'coerce)) arguments env)
  (match arguments
    ((list _ type)
     (maybe-constant-form-value type env :default t))

    (_ t)))

(defstruct (numeric-op (:conc-name %numeric-op-))
  closed-under-fixnum-p
  closed-under-integers-p
  closed-under-rationals-p
  closed-under-float-p
  result-necessarily-float-p
  result-necessarily-integer-p
  result-necessarily-real-p)

(macrolet ((def (slot-name)
             `(defun ,(symbolicate 'numeric-op '- slot-name) (op)
                (multiple-value-bind (value existsp)
                    (gethash op *numeric-op-table*)
                  (if existsp
                      (,(symbolicate '%numeric-op- slot-name) value)
                      nil)))))
  (def closed-under-fixnum-p)
  (def closed-under-integers-p)
  (def closed-under-rationals-p)
  (def closed-under-float-p)
  (def result-necessarily-float-p)
  (def result-necessarily-integer-p)
  (def result-necessarily-real-p))

(defparameter *numeric-op-table*
  (alist-hash-table
   (nconc
    (list (cons '+ (make-numeric-op :closed-under-fixnum-p nil :closed-under-integers-p t   :closed-under-rationals-p t :closed-under-float-p t))
          (cons '- (make-numeric-op :closed-under-fixnum-p nil :closed-under-integers-p t   :closed-under-rationals-p t :closed-under-float-p t))
          (cons '* (make-numeric-op :closed-under-fixnum-p nil :closed-under-integers-p t   :closed-under-rationals-p t :closed-under-float-p t))
          (cons '/ (make-numeric-op :closed-under-fixnum-p nil :closed-under-integers-p nil :closed-under-rationals-p t :closed-under-float-p t))

          (cons '1+ (make-numeric-op :closed-under-fixnum-p nil :closed-under-integers-p t :closed-under-rationals-p t :closed-under-float-p t))
          (cons '1- (make-numeric-op :closed-under-fixnum-p nil :closed-under-integers-p t :closed-under-rationals-p t :closed-under-float-p t)))

    #+sbcl
    (list (cons 'max (make-numeric-op :closed-under-fixnum-p t :closed-under-integers-p t :closed-under-rationals-p t :closed-under-float-p t :result-necessarily-real-p t))
          (cons 'min (make-numeric-op :closed-under-fixnum-p t :closed-under-integers-p t :closed-under-rationals-p t :closed-under-float-p t :result-necessarily-real-p t)))

    ;; Implementations are free to decide whether to apply contagions or not: http://clhs.lisp.se/Body/f_max_m.htm
    #-sbcl
    (list (cons 'max (make-numeric-op :closed-under-fixnum-p nil :closed-under-integers-p nil :closed-under-rationals-p nil :closed-under-float-p nil :result-necessarily-real-p t))
          (cons 'min (make-numeric-op :closed-under-fixnum-p nil :closed-under-integers-p nil :closed-under-rationals-p nil :closed-under-float-p nil :result-necessarily-real-p t)))

    (list (cons 'floor    (make-numeric-op :closed-under-fixnum-p t :closed-under-integers-p t :closed-under-rationals-p t :result-necessarily-integer-p t))
          (cons 'ceiling  (make-numeric-op :closed-under-fixnum-p t :closed-under-integers-p t :closed-under-rationals-p t :result-necessarily-integer-p t))
          (cons 'truncate (make-numeric-op :closed-under-fixnum-p t :closed-under-integers-p t :closed-under-rationals-p t :result-necessarily-integer-p t))
          (cons 'round    (make-numeric-op :closed-under-fixnum-p t :closed-under-integers-p t :closed-under-rationals-p t :result-necessarily-integer-p t))

          (cons 'ffloor    (make-numeric-op :closed-under-float-p t :result-necessarily-float-p t))
          (cons 'fceiling  (make-numeric-op :closed-under-float-p t :result-necessarily-float-p t))
          (cons 'ftruncate (make-numeric-op :closed-under-float-p t :result-necessarily-float-p t))
          (cons 'fround    (make-numeric-op :closed-under-float-p t :result-necessarily-float-p t)))

    #+sbcl
    (list (cons 'sin (make-numeric-op :result-necessarily-float-p t))
          (cons 'cos (make-numeric-op :result-necessarily-float-p t))
          (cons 'tan (make-numeric-op :result-necessarily-float-p t)))
    #-sbcl
    (list (cons 'sin (make-numeric-op))
          (cons 'cos (make-numeric-op))
          (cons 'tan (make-numeric-op))))))

(defun numeric-result-type (op arg-types env)

  (flet ((some-subtypep (type)
           (some (lambda (arg-type)
                   (subtypep arg-type type env))
                 arg-types))
         (all-subtypep (type)
           (every (lambda (arg-type)
                    (subtypep arg-type type env))
                  arg-types)))

    (let* ((realp (numeric-op-result-necessarily-real-p op))
           (complex-possible-p (not realp)))

      (when (numeric-op-closed-under-fixnum-p op)
        (when (all-subtypep 'fixnum)
          (return-from numeric-result-type 'fixnum))
        (when (and complex-possible-p
                   (all-subtypep '(complex fixnum)))
          (return-from numeric-result-type '(complex fixnum))))

      (when (numeric-op-closed-under-integers-p op)
        (when (all-subtypep 'integer)
          (return-from numeric-result-type 'integer))
        (when (and complex-possible-p
                   (all-subtypep '(complex integer)))
          (return-from numeric-result-type '(complex integer))))

      (when (numeric-op-closed-under-rationals-p op)
        (when (all-subtypep 'rational)
          (return-from numeric-result-type 'rational))
        (when (and complex-possible-p
                   (all-subtypep '(complex rational)))
          (return-from numeric-result-type '(complex rational))))

      (when (numeric-op-closed-under-float-p op)
        (when (all-subtypep 'single-float)
          (return-from numeric-result-type 'single-float))
        (when (all-subtypep 'double-float)
          (return-from numeric-result-type 'double-float))

        (when complex-possible-p
          (when (all-subtypep '(complex single-float))
            (return-from numeric-result-type '(complex single-float)))
          (when (all-subtypep '(complex double-float))
            (return-from numeric-result-type '(complex double-float)))))

      (when (numeric-op-result-necessarily-integer-p op)
        (return-from numeric-result-type 'integer))

      (when (numeric-op-result-necessarily-float-p op)
        (when (some-subtypep 'double-float)
          (return-from numeric-result-type 'double-float))
        ;; What happens if all the results are rational?
        (return-from numeric-result-type 'single-float))

      (return-from numeric-result-type 'number))))

(defun every-eql-type-p (types env)
  (let (values)
    (every (lambda (type)
             (optima:match type
               ((list 'eql value)
                (push value values))
               (_
                (return-from every-eql-type-p
                  (values nil nil)))))
           types)
    (values t (nreverse values))))

(defun numeric-op-form-type (op args env)
  (let ((arg-types (mapcar (lambda (arg)
                             (introspect-environment:typexpand
                              (form-type arg env)
                              env))
                           args)))
    (multiple-value-bind (all-eql-p values)
        (every-eql-type-p arg-types env)
      (cond (all-eql-p
             (or (ignore-errors `(eql ,(apply op values)))
                 `number))
            (t
             (numeric-result-type op arg-types env))))))

(macrolet ((def (&rest ops)
             `(progn
                ,@(mapcar (lambda (op)
                            `(defmethod custom-form-type ((op (eql ',op)) args env)
                               (numeric-op-form-type op args env)))
                          ops))))
  (def + - / * 1+ 1- max min
    floor ceiling truncate round
    ffloor fceiling fruncate fround

    sin cos tan atan))
