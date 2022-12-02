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

;;         /---- using ------\
;;        v                   v
;;     memb      enum --> toplevel <----- template
;;     ^^^^^                   ^  ^
;;     |||| \--- cls -------\  |   \------ static-func
;;     ||||                  v |
;;     ||| \- destructor  templatable <--- func
;;     || \-- operator==
;;     | \--- default-constructor <-- constructor
;;     |
;; protectable <-- abstract-meth <-- meth
;;         ^
;;          \----- field


(defgeneric c-protect-p (x)
  (:documentation "(c-protect-p X)

Predicate determining whether a value is a protection symbol.
Protection symbols are :public, :protected, and :private.

Examples:
(c-protect-p :public)    --> t
(c-protect-p :protected) --> t
(c-protect-p :private)   --> t
(c-protect-p :blub)      --> nil
(c-protect-p 5)          --> nil"))

(defmethod c-protect-p (x)
  (or (eq :public x)
      (eq :protected x)
      (eq :private x)))

(defgeneric protect (x)
  (:documentation "(protect X)

Reads a protectable's protection property. Returns value
sufficing c-protect-p.")
  (:method (x)
	(error "protect is applicable only to instances of c-protectable: ~a" (type-of x))))

;;------------------------------------------------------------
;; Toplevel
;;------------------------------------------------------------

(defclass c-toplevel () ())

(defmethod c-toplevel-p (x)
  (typep x 'c-toplevel))


;;------------------------------------------------------------
;; Templatable
;;------------------------------------------------------------

(defclass c-templatable (c-toplevel) ())

(defmethod c-templatable-p (x)
  (typep x 'c-templatable))


;;------------------------------------------------------------
;; Memb
;;------------------------------------------------------------
  
(defclass c-memb () ())

(defmethod c-memb-p (x)
  (typep x 'c-memb))


;;------------------------------------------------------------
;; 7.5 Protectable
;;------------------------------------------------------------

(defclass c-protectable (c-memb)
  ((protect
    :initarg  :protect
    :initform (error "c-protectable must have protect slot")
    :reader   protect)))

(defmethod c-protectable-p (x)
  (typep x 'c-protectable))

(defmethod protect ((x c-protectable))
  (error "protect NYI for ~a" (type-of x)))


;;------------------------------------------------------------
;; Toplevel Instances
;;------------------------------------------------------------

;; c-static-func

(defclass c-static-func (c-toplevel)
  ((name
    :initarg  :name
    :initform (error "c-static-func must have name slot")
    :reader   name)
   (arg-list
    :initarg  :arg-list
    :initform '()
    :reader   arg-list)
   (ret-type
    :initarg  :ret-type
    :initform (error "c-static-func must have ret-type slot")
    :reader   ret-type)
   (body
    :initarg  :body
    :initform '()
    :reader   body)))

(defmethod c-static-func ((name string) (arg-list list) (ret-type c-tp) &rest body)
  (unless (every #'c-argument-ellipses-p arg-list)
    (error (format nil "c-static-func arg-list must have c-argument-ellipses members: ~a" arg-list)))
  (let ((body     (alexandria:flatten body))
	(arg-list (mapcar #'c-simplify-arg arg-list)))
    (unless (every #'c-st-p body)
      (error "c-static-func body must have c-st members"))
    (make-instance 'c-static-func
		   :name     name
		   :arg-list arg-list
		   :ret-type ret-type
		   :body     body)))

(defmethod print-object ((obj c-static-func) stream)
  (with-accessors ((name     name)
		   (arg-list arg-list)
		   (ret-type ret-type)
		   (body     body))
      obj
    (format stream
	    "(c-static-func ~s ~a ~a~a)"
	    name
	    (if arg-list
		(format nil "(list ~{~a~^ ~})" arg-list)
		"'()")
	    ret-type
	    (if body
		(format nil " ~{~a~^ ~}" body)
		""))))

(defmethod c-static-func-p (x)
  (typep x 'c-static-func))



;; c-func

(defclass c-func (c-templatable)
  ((info
    :initarg  :info
    :initform (error "c-func must have info slot")
    :reader   info)
   (arg-list
    :initarg  :arg-list
    :initform '()
    :reader   arg-list)
   (ret-type
    :initarg  :ret-type
    :initform (error "c-func must have ret-type slot")
    :reader   ret-type)
   (body
    :initarg  :body
    :initform '()
    :reader   body)))

(defmethod c-func ((info c-info) (arg-list list) (ret-type c-tp) &rest body)
  (unless (every #'c-argument-p arg-list)
    (error "c-func arg-list must have c-argument members"))
  (let ((body     (alexandria:flatten body))
	(arg-list (mapcar #'c-simplify-arg arg-list)))
    (unless (every #'c-st-p body)
      (error "c-func body must have c-st members"))
    (make-instance 'c-func
		   :info     info
		   :arg-list arg-list
		   :ret-type ret-type
		   :body     body)))

(defmethod c-func-p (x)
  (typep x 'c-func))



;;  using

(defclass c-using (c-toplevel c-memb)
  ((info
    :initarg  :info
    :initform (error "c-using must have info slot")
    :reader   info)
   (param-type
    :initarg  :param-type
    :initform (error "c-using must have param-type slot")
    :reader   param-type)))

(defmethod c-using ((info c-info) (param-type c-tp))
  (make-instance 'c-using
		 :info       info
		 :param-type param-type))

(defmethod c-using-p (x)
  (typep x 'c-using))

(defmethod protect ((c c-using))
  :public)

(defmethod print-object ((obj c-using) stream)
  (with-accessors ((info       info)
		   (param-type param-type))
      obj
    (format stream "(c-using ~a ~a)" info param-type)))


;;------------------------------------------------------------
;; 7.3 Template
;;------------------------------------------------------------

(defclass c-template (c-toplevel)
  ((arg-list
    :initarg  :arg-list
    :initform '()
    :reader   arg-list)
   (operand
    :initarg  :operand
    :initform (error "c-template must have operand")
    :reader   operand)))

(defmethod c-template ((arg-list list) (operand c-templatable))
  (unless (every #'stringp arg-list)
    (error "c-template arg-list elements must be of type string"))
  (unless (> (length arg-list) 0)
    (error "template must have at least one argument"))
  (make-instance 'c-template
		 :arg-list arg-list
		 :operand  operand))

(defmethod print-object ((obj c-template) stream)
  (with-accessors ((arg-list arg-list)
		   (operand  operand))
      obj
    (format stream
	    "(c-template (list ~{~a~^ ~}) ~a)"
	    arg-list
	    operand)))

(defmethod c-template-p (x)
  (typep x 'c-template))

;;------------------------------------------------------------
;; Class
;;------------------------------------------------------------

(defclass c-cls (c-templatable c-memb)
  ((info
    :initarg  :info
    :initform (error "c-cls must have ifno slot")
    :reader   info)
   (inherit-list
    :initarg  :inherit-list
    :initform '()
    :reader   inherit-list)
   (body
    :initarg  :body
    :initform '()
    :reader   body)))

(defmethod c-cls ((info c-info) (inherit-list list) &rest body)
  (unless (every #'c-name-p inherit-list)
    (error "c-cls inherit list must have c-name members"))
  (let ((body (alexandria:flatten body)))
    (unless (every #'c-memb-p body)
      (error "c-cls must have memb instances in body"))
    (make-instance 'c-cls
		   :info         info
		   :inherit-list inherit-list
		   :body         body)))

(defmethod protect ((c c-cls))
  :public)

(defmethod print-object ((obj c-cls) stream)
  (with-accessors ((info info)
		   (body body))
      obj
    (format stream
	    "(c-cls ~a~a)"
	    info
	    (if body
		(format nil " ~{~a~^ ~}" body)
		""))))

(defmethod c-cls-p (x)
  (typep x 'c-cls))

