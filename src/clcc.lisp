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

;;============================================================
;; CC: A Model of the C++ Programming Language
;;============================================================
;;
;; This module provides a model of the C++ programming
;; language suitable as a target for compilation. Its goal is
;; to both cover a large portion of C++ language features and
;; be readable.

;;------------------------------------------------------------
;; 1 Introduction
;;------------------------------------------------------------
;;
;; Section 2 provides convenience functions for comments and
;; indentation. Section 3 introduces the concept of types and
;; arguments. Section 4 introduces expressions and L-values.
;; Section 5 introduces symbols which are at the intersection
;; of types and L-values. Section 6 provides statements.
;; Section 7 introduces top-level forms. Section 8 introduces
;; source modules. Section 9 covers the compiler. Section 10
;; gives utility functions.
;;
;;
;;
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
;;
;; Observations:
;; - memb and toplevel are the two roots of this digraph
;; - cls and using are nodes below both memb and toplevel
;; So, memb, toplevel, cls, and using are somewhat special
;; and, thus, deserve their dedicated generic functions
;; c-X->cc and c-X->hh.


(defclass c-info ()
  ((name
    :initarg  :name
    :initform (error "info must have name slot")
    :reader   name)
   (doc
    :initarg  :doc
    :initform nil
    :reader   doc)
   (ret
    :initarg  :ret
    :initform nil
    :reader   ret)))

(defmethod c-info ((name string) &optional doc ret)
  (when doc
    (unless (stringp doc)
      (error "info doc, if set, must be string")))
  (when ret
    (unless (stringp ret)
      (error "info ret, if set, must be string")))
  (make-instance 'c-info
		 :name  name
		 :doc   doc
		 :ret   ret))

(defmethod print-object ((obj c-info) stream)
  (with-accessors ((name name)
		   (doc  doc)
		   (ret  ret))
      obj
    (format stream "(c-info ~s" name)
    (if doc
      (format stream " ~s" doc)
      (when ret
	(format stream " nil")))
    (when ret
      (format stream " ~s" ret))
    (format stream ")")))
	    


    
;; 7.7 Constructors
;;

(defclass c-default-constructor (c-memb) ())

(defmethod c-default-constructor ()
  (make-instance 'c-default-constructor))

(defmethod print-object ((obj c-default-constructor) stream)
  (format stream "(c-default-constructor)"))

(defmethod c-default-constructor-p (x)
  (typep x 'c-default-constructor))

(defmethod protect ((c c-default-constructor))
  :public)

(defclass c-constructor (c-default-constructor)
  ((arg-list
    :initarg  :arg-list
    :initform '()
    :reader   arg-list)
   (set-list
    :initarg  :set-list
    :initform '()
    :reader   set-list)
   (body
    :initarg  :body
    :initform '()
    :reader   body)))

(defmethod c-constructor ((arg-list list) (set-list list) &rest body)
  (unless (every #'c-argument-p arg-list)
    (error "c-constructor arg-list must have c-argument items"))
  (unless (every #'string-c-ex-p set-list)
    (error "c-constructor set-list must have string-c-ex pair elements"))
  (let ((body (alexandria:flatten body)))
    (unless (every #'c-st-p body)
      (error "c-constructor body must have c-st items"))
    (make-instance 'c-constructor
		   :arg-list  arg-list
		   :set-list  set-list
		   :body      body)))

(defmethod c-constructor-p (x)
  (typep x 'c-constructor))

;; 7.8 Destructors
;;

(defclass c-destructor (c-memb)
  ((body
    :initarg  :body
    :initform '()
    :reader   body)))

(defmethod c-destructor (&rest body)
  (let ((body (alexandria:flatten body)))
    (unless (every #'c-st-p body)
      (error "c-destructor body must have st items"))
    (make-instance 'c-destructor
		   :body    body)))

(defmethod print-object ((obj c-destructor) stream)
  (with-accessors ((body body))
      obj
    (format stream "(c-destructor~a)"
	    (if body
		(format nil " ~{~a~^ ~}" body)
		""))))

(defmethod c-destructor-p (x)
  (typep x 'c-destructor))

(defmethod protect ((c c-destructor))
  :public)

;; 7.9 Equality
;;

(defclass c-operator== (c-memb) ())

(defmethod c-operator== ()
  (make-instance 'c-operator==))

(defmethod c-operator==-p (x)
  (typep x 'c-operator==))

(defmethod protect ((c c-operator==))
  :public)

;; 7.10 Field
;;

(defclass c-field (c-protectable)
  ((arg
    :initarg  :arg
    :initform (error "c-protectable must have arg slot")
    :reader   arg)
   (expr-list
    :initarg  :expr-list
    :initform (error "c-protectable must have expr-list slot")
    :reader   expr-list)))

(defmethod c-field ((protect symbol) (arg c-argument) &rest expr-list)
  (let ((expr-list (alexandria:flatten expr-list)))
    (unless (every #'c-ex-p expr-list)
      (error "c-field expr-list must have c-ex members"))
    (unless (c-protect-p protect)
      (error "c-field protect must be :public, :protected, or :private"))
    (make-instance 'c-field
		   :protect    protect
		   :arg        arg
		   :expr-list  expr-list)))

(defmethod print-object ((obj c-field) stream)
  (with-accessors ((field-protect   protect)
		   (field-arg       arg)
		   (field-expr-list expr-list))
      obj
    (format stream
	    "(c-field :~a ~a~a)"
	    field-protect
	    field-arg
	    (if field-expr-list
		(format nil " ~{~a~^ ~}" field-expr-list)
		""))))

(defmethod c-field-p (x)
  (typep x 'c-field))

;; 7.11 Methods
;;

(defclass c-abstract-meth (c-protectable)
  ((info
    :initarg  :info
    :initform (error "c-abstract-meth must have info slot")
    :reader   info)
   (arg-list
    :initarg  :arg-list
    :initform '()
    :reader   arg-list)
   (ret-type
    :initarg  :ret-type
    :initform (error "c-abstract-meth must have ret-type slot")
    :reader   ret-type)
   (const-p
    :initarg  :const-p
    :initform (error "c-abstract-meth must have const-p slot")
    :reader   const-p)))

(defmethod c-abstract-meth ((protect symbol) const-p (info c-info) (arg-list list) (ret-type c-tp))
  (unless (c-protect-p protect)
    (error "c-abstract-meth protect must be :public, :protected, or :private"))
  (unless (every #'c-argument-p arg-list)
    (error "c-abstract-meth arg-list must have c-argument members"))
  (make-instance 'c-abstract-meth
		 :protect  protect
		 :const-p  (if const-p t nil)
		 :info     info
		 :arg-list arg-list
		 :ret-type ret-type))

(defmethod print-object ((obj c-abstract-meth) stream)
  (with-accessors ((protect  protect)
		   (const-p  const-p)
		   (info     info)
		   (arg-list arg-list)
		   (ret-type ret-type))
      obj
    (format stream
	    "(c-abstract-meth :~a ~a ~a ~a ~a)"
	    protect
	    const-p
	    info
	    (if arg-list
		(format nil "(list ~{~a~^ ~})" arg-list)
		"'()")
	    ret-type)))

(defmethod c-abstract-meth-p (x)
  (typep x 'c-abstract-meth))

(defclass c-meth (c-abstract-meth)
  ((body
    :initarg  :body
    :initform '()
    :reader   body)))

(defmethod c-meth ((protect symbol) const-p (info c-info) (arg-list list) (ret-type c-tp) &rest body)
  (unless (c-protect-p protect)
    (error "c-meth protect must be :public, :protected, or :private"))
  (unless (every #'c-argument-p arg-list)
    (error "c-meth arg-list must have c-argument members"))
  (let ((body (alexandria:flatten body))
	(arg-list (mapcar #'c-simplify-arg arg-list)))
    (unless (every #'c-st-p body)
      (error "c-meth body must have c-st members"))
    (make-instance 'c-meth
		   :protect  protect
		   :const-p  (if const-p t nil)
		   :info     info
		   :arg-list arg-list
		   :ret-type ret-type
		   :body     body)))

(defmethod print-object ((obj c-meth) stream)
  (with-accessors ((protect  protect)
		   (const-p  const-p)
		   (info     info)
		   (arg-list arg-list)
		   (ret-type ret-type)
		   (body     body))
      obj
    (format stream
	    "(c-meth :~a ~a ~a ~a ~a~a)"
	    protect
	    const-p
	    info
	    (if arg-list
		(format nil "(list ~{~a~^ ~})" arg-list)
		"'()")
	    ret-type
	    (if body
		(format nil " ~{~a~^ ~}" body)
		""))))

(defmethod c-meth-p (x)
  (typep x 'c-meth))
  
;; 7.13 Enum
;;

(defclass c-enum (c-toplevel)
  ((info
    :initarg  :info
    :initform (error "c-enum must have info slot")
    :reader   info)
   (arg-list
    :initarg  :arg-list
    :initform '()
    :reader   arg-list)))

(defmethod c-enum ((info c-info) &rest arg-list)
  (let ((arg-list (alexandria:flatten arg-list)))
    (unless (every #'stringp arg-list)
      (error "c-enum arg-list must have string items"))
    (make-instance 'c-enum
		   :info     info
		   :arg-list arg-list)))

(defmethod c-enum-p (x)
  (typep x 'c-enum))

(defmethod print-object ((obj c-enum) stream)
  (with-accessors ((info     info)
		   (arg-list arg-list))
      obj
    (format stream "(c-enum ")
    (format stream "~a" info)
    (if arg-list
	(format stream " ~{~s~^ ~}" arg-list)
	"")
    (format stream ")")))

	  

;;------------------------------------------------------------
;; 8 Source
;;------------------------------------------------------------

(defclass c-src ()
  ((name
    :initarg  :name
    :initform (error "c-src must have name slot")
    :reader   name)
   (system-list
    :initarg  :system-list
    :initform '()
    :reader   system-list)
   (user-list
    :initarg  :user-list
    :initform '()
    :reader   user-list)
   (namespace
    :initarg  :namespace
    :initform (error "c-src must have namespace slot")
    :reader   namespace)
   (body
    :initarg  :body
    :initform '()
    :reader   body)))
    
(defmethod c-src ((name string) (system-list list) (user-list list) (namespace string) &rest body)
  (unless (every #'stringp system-list)
    (error "c-src must have string instances in system-list"))
  (unless (every #'stringp user-list)
    (error "c-src must have string instances in user-list"))
  (let ((body (alexandria:flatten body)))
    (unless (every #'c-toplevel-p body)
      (error "c-src must have toplevel instances in body"))
    (make-instance 'c-src
		   :name        name
		   :system-list system-list
		   :user-list   user-list
		   :namespace   namespace
		   :body        body)))



;;------------------------------------------------------------
;; 9 Compiler
;;------------------------------------------------------------

(defgeneric cls-name (n))

(defmethod cls-name ((n c-n-x))
  (name n))

(defmethod cls-name ((n c-n-sub))
  (cls-name (rhs n)))

(defgeneric full-name (prefix cls-name))

(defmethod full-name ((prefix c-name) (cls-name string))
  (c-n-sub prefix (c-n-x cls-name)))

(defmethod full-name ((prefix null) (cls-name string))
  (c-n-x cls-name))


(defgeneric c-memb->hh (m cls-name)
  (:documentation "(c-memb->hh M CLS-NAME)

Compiles the header part of a c-memb. The argument M is the member to compile.
The argument CLS-NAME is the name of the class that the member is a part of.
Knowing the class name is necessary to compile constructor and destructor
members.
"))

(defmethod c-memb->hh ((m c-using) (cls-name string))
  (c-using->hh m))

(defmethod c-memb->hh ((m c-cls) (cls-name string))
  (c-cls->hh m))


(defmethod c-memb->hh ((m c-field) (cls-name string))
  (with-accessors ((field-arg     arg)
		   (field-protect protect))
      m
    (with-accessors ((field-name       name)
		     (field-param-type param-type)
		     (field-doc        doc))
	field-arg
      (let ((core (format nil "~a ~a;" (c-->string field-param-type) field-name)))
	(if (and (eq field-protect :public) field-doc)
	    (format nil "~32a /**< ~a */" core field-doc)
	    core)))))



(defmethod c-memb->hh ((m c-abstract-meth) (cls-name string))
  (with-accessors ((protect  protect)
		   (const-p  const-p)
		   (info     info)
		   (arg-list arg-list)
		   (ret-type ret-type))
      m
    (with-accessors ((name name)
		     (doc  doc)
		     (ret  ret))
	info
      (format nil
	      "~avirtual ~a ~a~a~a = 0;"
	      (if (and (eq protect :public) (or doc ret (some #'doc arg-list)))
		  (format nil
			  "~%/**~%~a~{~a~}~a */~%"
			  (if doc
			      (format nil " * \\brief ~a~%" doc)
			      "")
			  (loop for arg in arg-list
				as d = (doc  arg)
				as n = (name arg)
				when d
				  collect (format nil " * \\param ~a ~a~%" n d))
			  (if ret
			      (format nil " * \\return ~a~%" ret)
			      ""))
		  "")
	      (c-->string ret-type)
	      name
	      (if arg-list
		  (format nil "( ~{~a~^, ~} )" (mapcar #'c-->string arg-list))
		  "( void )")
	      (if (const-p m)
		  " const"
		  "")))))
  
(defmethod c-memb->hh ((m c-meth) (cls-name string))
  (with-accessors ((protect  protect)
		   (const-p  const-p)
		   (info     info)
		   (arg-list arg-list)
		   (ret-type ret-type))
      m
    (with-accessors ((name name)
		     (doc  doc)
		     (ret  ret))
	info
      (format nil
	      "~a~a ~a~a~a;"
	      (if (and (eq protect :public) (or doc ret (some #'doc arg-list)))
		  (format nil
			  "~%/**~%~a~{~a~}~a */~%"
			  (if doc
			      (format nil " * \\brief ~a~%" doc)
			      "")
			  (loop for arg in arg-list
				as d = (doc  arg)
				as n = (name arg)
				when d
				  collect (format nil " * \\param ~a ~a~%" n d))
			  (if ret
			      (format nil " * \\return ~a~%" ret)
			      ""))
		  "")
	      (c-->string ret-type)
	      name
	      (if arg-list
		  (format nil "( ~{~a~^, ~} )" (mapcar #'c-->string arg-list))
		  "( void )")
	      (if (const-p m)
		  " const"
		  "")))))

(defmethod c-memb->hh ((m c-default-constructor) (cls-name string))
  (format nil "~a( void );" cls-name))

(defmethod c-memb->hh ((m c-constructor) (cls-name string))
  (with-accessors ((arg-list arg-list))
      m
    (let ((z (if arg-list
		 (format nil "( ~{~a~^, ~} )" (mapcar #'c-->string arg-list))
		 "( void )")))
      (format nil "~a~a;" cls-name z))))

(defmethod c-memb->hh ((m c-destructor) (cls-name string))
  (format nil "~~~a( void );" cls-name))

(defmethod c-memb->hh ((m c-operator==) (cls-name string))
  (let* ((arg (c-argument "other" (c-t-const-ref (c-n-x cls-name))))
	 (s   (c-->string arg)))
    (format nil "bool operator==( ~a ) const;~%bool operator!=( ~a ) const;" s s)))


(defgeneric c-memb->cc (m cls-name cls-body prefix typename-list)
  (:documentation "
(c-memb->cc M CLS-NAME CLS-BODY PREFIX TYPENAME-LIST)

M:             the class member to compile; instance of c-memb
CLS-NAME:      the name of the class that M is a member of; string
CLS-BODY:      the body of the class that M is a member of; list of c-memb instances
PREFIX:        the full prefix of M; instance of c-name
               see also: (full-name PREFIX CLS-NAME)
TYPENAME-LIST: the names of the template parameters of the class; list of string instances

Returns a string that is the C++14 source code representation of M.

Examples:
  Say we have
  class c1 { public: int x; };

  (defparameter m
    (c-field :public (c-argument \"x\" (c-t-int))))

  (defparameter cls-name
    \"c1\")

  (defparameter cls-body
    (list m))

  (defparameter prefix
    (full-name nil cls-name))

  (defparameter typename-list
    '())

  (c-memb->cc m cls-name cls-body prefix typename-list)
  ==> \"int x;\"
"))

(defmethod c-memb->cc ((m c-cls) (cls-name string) (cls-body list) prefix (typename-list list))
  (with-accessors ((i info)
		   (b body))
      m
    (with-accessors ((n name))
	i
      (format nil
	      "~{~a~^~%~%~}"
	      (loop for c in b
		    unless (c-using-p c)
		      unless (c-field-p c)
			collect (c-memb->cc c n b (full-name prefix n) typename-list))))))

(defmethod c-memb->cc ((m c-meth) (cls-name string) (cls-body list) prefix (typename-list list))
  (with-accessors ((meth-ret-type ret-type)	
		   (meth-info     info)
		   (meth-arg-list arg-list)
		   (meth-const-p  const-p)
		   (meth-body     body))
      m
    (with-accessors ((meth-name name))
	meth-info
      (format nil
	      "~a~a ~a~a~a~%~a"
	      (if typename-list
		  (format nil "template<~{typename ~a~^, ~}>~%" typename-list)
		  "")
	      (c-->string meth-ret-type)
	      (c-->string (full-name prefix meth-name))
	      (if meth-arg-list
		  (format nil "( ~{~a~^, ~} )" (mapcar #'c-->string meth-arg-list))
		  "( void )")
	      (if meth-const-p
		  " const"
		  "")
	      (c-->string (c-s-block meth-body))))))
	  
(defmethod c-memb->cc ((m c-default-constructor) (cls-name string) (cls-body list) prefix (typename-list list))
  (let* ((field-list       (loop for mm in cls-body
				 when (and (c-field-p mm) (expr-list mm))
				   collect mm))
	 (init-list        (mapcar
			    #'(lambda (field)
				(format nil
					"~a ( ~{~a~^, ~} )"
					(name (arg field))
					(mapcar #'c-->string (expr-list field))))
			    field-list))
	 (init             (if field-list
			       (format nil " :~%  ~{~a~^,~%  ~}" init-list)
			       "")))
    (format nil
	    "~a~a( void )~a~%{}"
	    (if typename-list
		(format nil "template<~{typename ~a~^, ~}>~%" typename-list)
		"")
	    (c-->string (full-name prefix cls-name))
	    init)))


(defmethod c-memb->cc ((m c-constructor) (cls-name string) (cls-body list) prefix (typename-list list))
  (with-accessors ((constructor-arg-list arg-list)
		   (constructor-set-list set-list)
		   (constructor-body     body))
      m
    (format nil
	    "~a~a~a~a~%~a"
	    (if typename-list
		(format nil "template<~{typename ~a~^, ~}>~%" typename-list)
		"")
	    (c-->string (full-name prefix cls-name))
	    (if constructor-arg-list
		(format nil "( ~{~a~^, ~} )" (mapcar #'c-->string constructor-arg-list))
		"( void )")
	    (if constructor-set-list
		(format nil
			" :~%  ~{~a~^,~%  ~}"
			(mapcar
			 #'(lambda (pair)
			     (format nil "~a ( ~a )" (car pair) (c-->string (cdr pair))))
			 constructor-set-list))
		"")
	    (c-->string (c-s-block constructor-body)))))

(defmethod c-memb->cc ((m c-destructor) (cls-name string) (cls-body list) prefix (typename-list list))
  (format nil
	  "~a~a( void )~%~a"
	  (if typename-list
	      (format nil "template<~{typename ~a~^, ~}>~%" typename-list)
	      "")
	  (c-->string (full-name prefix (format nil "~~~a" cls-name)))
	  (c-->string (c-s-block (body m)))))

(defmethod c-memb->cc ((m c-operator==) (cls-name string) (cls-body list) prefix (typename-list list))
  (let* ((a     (c-argument "other" (c-t-const-ref (c-n-x cls-name))))
	 (body1 (loop for mm in cls-body
		      when (c-field-p mm)
			collect (let ((field-name (name (arg mm))))
				  (c-s-if (c-e-!=
					   (c-n-x field-name)
					   (c-e-. (c-n-x "other") (c-n-x field-name)))
					  (c-s-block
					   (c-s-return (c-e-bool nil)))))))

		    
	 (body2 (list
		 (c-s-return (c-e-! (c-e-call (c-n-x "operator==") (c-n-x "other"))))))
	 (meth1 (c-meth
		 :public
		 t
		 (c-info "operator==")
		 (list a)
		 (c-t-bool)
		 (list (if body1
			   body1
			   (c-s-do (c-e-plain-cast (c-t-void) (c-n-x "other"))))
		       (c-s-return (c-e-bool t)))))
	 (meth2 (c-meth
		 :public
		 t
		 (c-info "operator!=")
		 (list a)
		 (c-t-bool)
		 body2)))
    (format nil
	    "~a~%~%~a"
	    (c-memb->cc meth1 cls-name cls-body prefix typename-list)
	    (c-memb->cc meth2 cls-name cls-body prefix typename-list))))


(defgeneric c-using->hh (using))

(defmethod c-using->hh ((using c-using))
  (with-accessors ((info info)
		   (param-type param-type))
      using
    (format nil
	    "using ~a = ~a;"
	    (name info)
	    (c-->string param-type))))


(defgeneric c-cls->hh (c))

(defmethod c-cls->hh ((c c-cls))
  (with-accessors ((cls-info         info)
		   (cls-inherit-list inherit-list)
		   (cls-body         body))
      c

    (multiple-value-bind (public-list protected-list private-list)
	(loop for m in cls-body
	      as p = (protect m)
	      when (eq :public p)
		collect m into pub
	      when (eq :protected p)
		collect m into prot
	      when (eq :private p)
		collect m into priv
	      finally
		 (return (values pub prot priv)))

      (with-accessors ((cls-name name)
		       (cls-doc  doc))
	  cls-info

	(labels ((f (m) (c-memb->hh m cls-name)))
	  (let ((public-str-list    (mapcar #'f public-list))
		(protected-str-list (mapcar #'f protected-list))
		(private-str-list   (mapcar #'f private-list)))

	    (format nil
		    "~aclass ~a~a~%{~a~a~a};"
		    (if cls-doc
			(format nil "/**~% * \\brief ~a~% */~%" cls-doc)
			"")
		    cls-name
		    (if cls-inherit-list
			(format nil " :~%  ~{public ~a~^,~%  ~}" (mapcar #'c-->string cls-inherit-list))
			"")
		    (if public-str-list
			(format nil "~%public:~%~a~%" (indent (format nil "~{~a~^~%~}" public-str-list)))
			"")
		    (if protected-str-list
			(format nil "~%protected:~%~a~%" (indent (format nil "~{~a~^~%~}" protected-str-list)))
			"")
		    (if private-str-list
			(format nil "~%private:~%~a~%" (indent (format nil "~{~a~^~%~}" private-str-list)))
			""))))))))


(defgeneric c-cls->cc (c prefix typename-list))

(defmethod c-cls->cc ((c c-cls) prefix (typename-list list))
  (with-accessors ((cls-info info)
		   (cls-body body))
      c
    (with-accessors ((cls-name name))
	cls-info
      (let ((meth-list (loop for m in cls-body
			     unless (c-using-p m)
			       unless (c-field-p m)
				 unless (and (c-abstract-meth-p m) (not (c-meth-p m)))
				   collect m)))
	(format
	 nil
	 "~{~a~^~%~%~}"
	 (mapcar
	  #'(lambda (m)
	      (c-memb->cc m cls-name cls-body (full-name prefix cls-name) typename-list))
	  meth-list))))))

(defgeneric c-toplevel->hh (tl src-name))

(defmethod c-toplevel->hh ((tl c-template) (src-name string))
  (with-accessors ((arg-list arg-list)
		   (operand  operand))
      tl
    (format nil
	    "template<~{typename ~a~^, ~}>~%~a"
	    arg-list
	    (c-toplevel->hh operand src-name))))

(defmethod c-toplevel->hh ((tl c-cls) (src-name string))
  (c-cls->hh tl))

(defmethod c-toplevel->hh ((tl c-static-func) (src-name string))
  (with-accessors ((ret-type ret-type)
		   (arg-list arg-list)
		   (name     name))
      tl
    (format nil
	    "static ~a ~a~a;"
	    (c-->string ret-type)
	    name
	    (if arg-list
		(format nil "( ~{~a~^, ~} )" (mapcar #'c-->string arg-list))
		"( void )"))))
	  
(defmethod c-toplevel->hh ((tl c-func) (src-name string))
  (with-accessors ((ret-type ret-type)
		   (arg-list arg-list)
		   (info     info))
      tl
    (format nil
	    "~a ~a~a;"
	    (c-->string ret-type)
	    (name info)
	    (if arg-list
		(format nil "( ~{~a~^, ~} )" (mapcar #'c-->string arg-list))
		"( void )"))))

(defmethod c-toplevel->hh ((tl c-using) (src-name string))
  (c-using->hh tl))

(defmethod c-toplevel->hh ((tl c-enum) (src-name string))
  (let* ((enum-name (name (info tl)))
	 (arg-list  (arg-list tl))
	 (l1        (format
		     nil
		     "#define LIST_OF_~a_~aS \\~%  ~{~a~^ \\~%  ~}~%"
		     (string-upcase src-name)
		     (string-upcase enum-name)
		     (mapcar #'(lambda (x) (format nil "X( ~a )" x)) arg-list)))
	 (l2        "#define X( name ) name,")
	 (l3        (format
		     nil
		     "enum ~a~%  {~%    LIST_OF_~a_~aS~%  };"
		     enum-name
		     (string-upcase src-name)
		     (string-upcase enum-name)))
	 (l4        (format nil "#undef X~%"))
	 (l5        (format nil "std::string format_~a( ~a arg );" enum-name enum-name)))
    (format nil "~{~a~^~%~}" (list l1 l2 l3 l4 l5))))


(defgeneric c-toplevel->cc (tl src-name prefix typename-list))

(defmethod c-toplevel->cc ((tl c-template) (src-name string) prefix (typename-list list))
  (with-accessors ((arg-list arg-list)
		   (body body))
      tl
    (c-toplevel->cc body src-name prefix arg-list)))



(defmethod c-toplevel->cc ((tl c-enum) (src-name string) prefix (typename-list list))
  (with-accessors ((enum-info info))
      tl
    (with-accessors ((enum-name name))
	enum-info
      (let* ((l1 "#define X( name ) case name: return #name;")
	     (l2 (format nil "std::string format_~a( ~a arg )" enum-name enum-name))
	     (l3 (format
		  nil
		  "{~%  switch( arg )~%    {~%      LIST_OF_~a_~aS~%    }"
		  (string-upcase src-name)
		  (string-upcase enum-name)))
	     (l4 (format nil "  throw std::runtime_error( \"~a not recognized\" );~%}" enum-name))
	     (l5 "#undef X"))
	(format nil "~{~a~^~%~}" (list l1 l2 l3 l4 l5))))))


(defmethod c-toplevel->cc ((tl c-cls) (src-name string) prefix (typename-list list))
  (c-cls->cc tl prefix typename-list))

(defmethod c-toplevel->cc ((tl c-func) (src-name string) prefix (typename-list list))
  (with-accessors ((tl-info     info)
		   (tl-arg-list arg-list)
		   (tl-ret-type ret-type)
		   (tl-body     body))
      tl
    (format nil
	    "~a~a ~a~a~%~a"
	    (if typename-list
		(format nil "template<~{typename ~a~^, ~}>~%" typename-list)
		"")
	    (c-->string tl-ret-type)
	    (name tl-info)
	    (if tl-arg-list
		(format nil "( ~{~a~^, ~} )" (mapcar #'c-->string tl-arg-list))
		"( void )")
	    (c-->string (c-s-block tl-body)))))

(defmethod c-toplevel->cc ((tl c-static-func) (src-name string) prefix (typename-list list))
  (with-accessors ((tl-name     name)
		   (tl-arg-list arg-list)
		   (tl-ret-type ret-type)
		   (tl-body     body))
      tl
    (format nil
	    "static ~a ~a~a~%~a"
	    (c-->string tl-ret-type)
	    tl-name
	    (if tl-arg-list
		(format nil "( ~{~a~^, ~} )" (mapcar #'c-->string tl-arg-list))
		"( void )")
	    (c-->string (c-s-block tl-body)))))
	
(defmethod c-src->hash ((src c-src))
  
  (let ((name        (name src))
	(system-list (system-list src))
	(user-list   (user-list src))
	(namespace   (namespace src))
	(body        (body src))
	(result      (make-hash-table :test #'equal))) ; hashtable to hold the compilation result

    (multiple-value-bind (static-func-list other-list)
	(loop for x in body
	      when (c-static-func-p x)
		collect x into sfl
	      else
		collect x into ol
	      finally
		 (return (values sfl ol)))

      (let ((path-hh (make-pathname :name name :type "hh")) ; header filename

	    (ifndef (format nil
			    "#ifndef __~a__~%#define __~a__"
			    (string-upcase name)
			    (string-upcase name)))
	    
	    (include (format nil
			     "~{~a~^~%~}"
			     (append
			      (mapcar #'(lambda (x)
					  (format nil "#include <~a>" x))
				      system-list)
			      (mapcar #'(lambda (x)
					  (format nil "#include \"~a\"" x))
				      user-list))))

	    (ns-hh  (format nil
			    "namespace ~a~%~a"
			    namespace
			    (if other-list
				(format nil
					"{~%~a~%}"
					(indent (format nil
							"~{~a~^~%~%~}"
							(mapcar #'(lambda (tl) (c-toplevel->hh tl name))
								other-list))))
				"{}")))
	    
	    (endif   (format nil "#endif /* __~a__ */" (string-upcase name))))


	(setf (gethash path-hh result)
	      (format nil
		      "~a~a~a~a"
		      (format nil "~a~%~%" ifndef)
		      (if (> (length include) 0)
			  (format nil "~a~%~%" include)
			  "")
		      (format nil "~a~%~%" ns-hh)
		      endif)))

      (let* ((path-cc          (make-pathname :name name :type "cc")) ; source filename

	     (header           (format nil
				       "#include \"~a.hh\""
				       name))

	     (static-func-decl (format nil
				       "~{~a~^~%~}"
				       (mapcar #'(lambda (tl) (c-toplevel->hh tl name))
					       static-func-list)))

	     (static-func-defn (format nil
				       "~{~a~^~%~%~}"
				       (mapcar #'(lambda (tl) (c-toplevel->cc tl name nil '()))
					       static-func-list)))

	     (func-cls-enum-list (loop for x in other-list
				       unless (or (c-using-p x))
					 collect x))
	     
	     (toplevel         (format nil
				       "~{~a~^~%~%~}"
				       (mapcar #'(lambda (tl) (c-toplevel->cc tl name nil '()))
					       func-cls-enum-list)))

	     (ns-cc            (format nil
				       "namespace ~a~%{~a~a~a}"
				       namespace
				       (if (> (length static-func-decl) 0)
					   (format nil "~%~a~%" (indent static-func-decl))
					   "")
				       (if (> (length toplevel) 0)
					   (format nil "~%~a~%" (indent toplevel))
					   "")
				       (if (> (length static-func-defn) 0)
					   (format nil "~%~a~%" (indent static-func-defn))
					   ""))))
	
	(setf (gethash path-cc result)
	      (format nil
		      "~{~a~^~%~%~}"
		      (list header ns-cc))))

      

	result)))


(defmethod c-find-dep-single ((c c-field))
  (c-find-dep-single (param-type (arg c))))

(defmethod c-find-dep-single ((x c-abstract-meth))
  (with-accessors ((arg-list arg-list)
		   (ret-type ret-type))
      x
    (c-find-dep arg-list ret-type)))

(defmethod c-find-dep-single ((x c-meth))
  (with-accessors ((arg-list arg-list)
		   (ret-type ret-type)
		   (body     body))
      x
    (c-find-dep arg-list ret-type body)))

(defmethod c-find-dep-single ((x c-template))
  (c-find-dep-single (body x)))

(defmethod c-find-dep-single ((x c-cls))
  (c-find-dep (body x)))

(defmethod c-find-dep-single ((x c-enum))
  '("stdexcept" "string"))

(defmethod c-find-dep-single ((x c-func))
  (with-accessors ((arg-list arg-list)
		   (ret-type ret-type)
		   (body     body))
      x
    (c-find-dep arg-list ret-type body)))

(defmethod c-find-dep-single ((x c-static-func))
  (with-accessors ((arg-list arg-list)
		   (ret-type ret-type)
		   (body     body))
      x
    (c-find-dep arg-list ret-type body)))

(defmethod c-find-dep-single ((x c-destructor))
  (c-find-dep (body x)))

(defmethod c-find-dep-single ((x c-constructor))
  (with-accessors ((arg-list arg-list)
		   (body     body))
      x
    (c-find-dep arg-list body)))

(defmethod c-find-dep-single ((x c-default-constructor))
  '())

(defmethod c-find-dep-single ((x c-operator==))
  '())

(defmethod c-find-dep-single ((x c-using))
  (c-find-dep-single (param-type x)))


  
