;; clcc
;; C++ language model and compiler in Common Lisp
;;
;; Copyright 2022 Jörgen Brandt <joergen@cuneiform-lang.org>
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(in-package :clcc)

;;        ex            tp
;;        ^^            ^^
;;        | \-- name --/ |
;;        |      ^^^     |
;; e-string      |||     t-string
;; "blub"        |||     std::string
;;               |||
;;               || \-- n-x     c
;;               | \--- n-arg   c<T>
;;                \---- n-sub   c::d





;;------------------------------------------------------------
;; Types
;;------------------------------------------------------------

(defclass c-tp () ())

(defmethod c-tp-p (x)
  (typep x 'c-tp))

;;------------------------------------------------------------
;; Arguments
;;------------------------------------------------------------

;; c-argument-ellipses

(defclass c-argument-ellipses ()
  ((doc
    :initarg  :doc
    :initform nil
    :reader   doc)))

(defmethod c-argument-ellipses (&optional doc)
  (when doc
    (unless (stringp doc)
      (error "c-argument-ellipses doc slot, if set, must be string")))
  (make-instance 'c-argument-ellipses
		 :doc doc))

(defmethod c-argument-ellipses-p (x)
  (typep x 'c-argument-ellipses))

(defmethod c-->string ((arg c-argument-ellipses))
  "...")
		
(defmethod c-find-dep-single ((x c-argument-ellipses))
  '())

;; c-argument

(defclass c-argument (c-argument-ellipses)
  ((name
    :initarg  :name
    :initform (error "c-argument must have name slot")
    :reader   name)
   (param-type
    :initarg  :param-type
    :initform (error "c-argument must have param-type slot")
    :reader   param-type)))
	      
(defmethod c-argument ((name string) (param-type c-tp) &optional doc)
  (when doc
    (unless (stringp doc)
      (error "c-argument doc slot, if set, must be string")))
  (make-instance 'c-argument
		 :name       name
		 :param-type param-type
		 :doc        doc))

(defmethod print-object ((obj c-argument) stream)
  (with-accessors ((name       name)
				   (param-type param-type)
				   (doc        doc))
      obj
    (format stream "(c-argument ~s ~a" name param-type)
	(when doc
	  (format stream " ~s" doc))
	(format stream ")")))

(defmethod c-argument-p (x)
  (typep x 'c-argument))

(defmethod c-->string ((arg c-argument))
  (with-accessors ((param-type param-type)
		   (name       name))
      arg
    (cond
      ((c-t-function-p param-type)
       (with-accessors ((ret-type ret-type)
			(arg-list arg-list))
	   param-type
	 (let ((lst (if arg-list
			(format nil
				"~{~a~^, ~}"
				(mapcar #'c-->string arg-list))
			"void")))
	   (format nil
		   "~a (*~a)( ~a )"
		   (c-->string ret-type)
		   name
		   lst))))
      (t
       (format nil "~a ~a" (c-->string param-type) name)))))

(defmethod c-find-dep-single ((x c-argument))
  (c-find-dep-single (param-type x)))


;;------------------------------------------------------------
;; Type Qualifiers
;;------------------------------------------------------------

;; c-t-const

(defclass c-t-const (c-tp)
  ((param-type
    :initarg  :param-type
    :initform (error "c-t-const must have param-type slot")
    :reader   param-type)))

(defmethod c-t-const ((param-type c-tp))
  (make-instance 'c-t-const
		 :param-type param-type))

(defmethod c-t-const-p (x)
  (typep x 'c-t-const))

(defmethod print-object ((obj c-t-const) stream)
  (with-accessors ((const-param-type param-type))
      obj
    (format stream "(c-t-const ~a)" const-param-type)))

(defmethod c-->string ((arg c-t-const))
  (format nil "~a const" (c-->string (param-type arg))))

;; c-t-ref

(defclass c-t-ref (c-tp)
  ((param-type
    :initarg  :param-type
    :initform (error "c-t-ref must have param-type slot")
    :reader   param-type)))

(defmethod c-t-ref ((param-type c-tp))
  (make-instance 'c-t-ref
		 :param-type param-type))

(defmethod print-object ((obj c-t-ref) stream)
  (format stream "(c-t-ref ~a)" (param-type obj)))

(defmethod c-->string ((arg c-t-ref))
  (format nil
		  "~a&"
		  (c-->string (param-type arg))))

(defmethod c-find-dep-single ((x c-t-ref))
  (c-find-dep-single (param-type x)))

;; c-t-pointer

(defclass c-t-pointer (c-tp)
  ((param-type
    :initarg  :param-type
    :initform (error "c-t-pointer must have param-type slot")
    :reader   param-type)))

(defmethod c-t-pointer ((param-type c-tp))
  (make-instance 'c-t-pointer
		 :param-type param-type))

(defmethod print-object ((obj c-t-pointer) stream)
  (format stream "(c-t-pointer ~a)" (param-type obj)))

(defmethod c-->string ((arg c-t-pointer))
  (format nil
		  "~a*"
		  (c-->string (param-type arg))))

(defmethod c-find-dep-single ((x c-t-pointer))
  (c-find-dep-single (param-type x)))

;;------------------------------------------------------------
;; Function Types
;;------------------------------------------------------------

(defclass c-t-function  (c-tp)
  ((arg-list
    :initarg  :arg-list
    :initform '()
    :reader   arg-list)
   (ret-type
    :initarg  :ret-type
    :initform (error "c-t-function must have ret-type slot")
    :reader   ret-type)))

(defmethod c-t-function ((arg-list list) (ret-type c-tp))
  (unless (every #'c-argument-p arg-list)
    (error "t-function arg-list must have argument items"))
  (make-instance 'c-t-function
		 :arg-list arg-list
		 :ret-type ret-type))

(defmethod c-t-function-p (x)
  (typep x 'c-t-function))

(defmethod c-->string ((t1 c-t-function))
  (format nil
	  "std::function<~a~a>"
	  (c-->string (ret-type t1))
	  (let ((al (arg-list t1)))
	    (if al
		(format nil
			"( ~{~a~^, ~} )"
			(mapcar #'c-->string
				(mapcar #'param-type
					(arg-list t1))))
		"()"))))

(defmethod c-find-dep-single ((x c-t-function))
  (c-find-dep (ret-type x)
	      (arg-list x)))

;;------------------------------------------------------------
;; Sugar
;;------------------------------------------------------------

(defmethod c-t-const-ref ((param-type c-tp))
  (c-t-ref (c-t-const param-type)))

(defmethod c-t-const-pointer ((param-type c-tp))
  (c-t-pointer (c-t-const param-type)))

(defmethod c-const-ref-argument ((name string) (param-type c-tp) &optional doc)
  (c-argument name (c-t-const-ref param-type) doc))


