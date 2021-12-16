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

(defmethod custom-form-type ((first (eql 'aref)) form env)
  (let ((element-type (introspect-environment:typexpand (form-type form env))))
    (if (listp element-type)
        (values (second element-type) t)
        (values t nil))))

(defmethod custom-form-type ((first (eql 'row-major-aref)) form env)
  (let ((element-type (introspect-environment:typexpand (form-type form env))))
    (if (listp element-type)
        (values (second element-type) t)
        (values t nil))))

(defmethod custom-form-type ((first (eql 'values)) args env)
  (values `(values ,@(loop :for form :in args
                           :collect (nth-form-type form env)))
          t))

(defmethod custom-form-type ((first (eql 'coerce)) arguments env)
  (match arguments
    ((list _ type)
     (maybe-constant-form-value type env :default t))

    (_ t)))
