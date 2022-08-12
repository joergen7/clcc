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

;;------------------------------------------------------------
;; 2 Comments and Indentation
;;------------------------------------------------------------

(defgeneric indent (s)
  (:documentation "indent argument string"))

(defmethod indent ((s string))
  (line-pad s "  " :unless-starts-with #\#))

(defgeneric comment (s)
  (:documentation "comment argument string"))

(defmethod comment ((s string))
  (line-pad s "// "))

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
	    


    

;;------------------------------------------------------------
;; 3 Types and Arguments
;;------------------------------------------------------------

;; 3.1 Types
;;

(defclass c-tp () ())

(defmethod c-tp-p (x)
  (typep x 'c-tp))


;; 3.2 Arguments
;;

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
		   (param-type param-type))
      obj
    (format stream "(c-argument ~s ~a)" name param-type)))

(defmethod c-argument-p (x)
  (typep x 'c-argument))


;; 3.3 Type Qualifiers
;;

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

(defmethod c-t-const-ref ((param-type c-tp))
  (c-t-ref (c-t-const param-type)))

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

(defmethod c-t-const-pointer ((param-type c-tp))
  (c-t-pointer (c-t-const param-type)))

;; 3.4 Function Types
;;

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

;;------------------------------------------------------------
;; 4 Expressions and L-Values
;;------------------------------------------------------------

;; 4.1 Expressions
;;

(defclass c-ex () ())

(defmethod c-ex-p (x)
  (typep x 'c-ex))

(defmethod string-c-ex-p (x)
  (and (consp x)
       (stringp (car x))
       (c-ex-p (cdr x))))

;; 4.2 L-Values
;;

(defclass c-e-lval (c-ex) ())

(defmethod c-e-lval-p (x)
  (typep x 'c-e-lval))

;; 4.3 Abstract Expressions
;;

(defclass c-e-value (c-ex)
  ((value
    :initarg  :value
    :initform (error "c-e-value must have value slot")
    :reader   value)))

(defclass c-e-expr (c-ex)
  ((expr
    :initarg  :expr
    :initform (error "c-e-expr must have expr slot")
    :reader   expr)))

(defclass c-e-param-type (c-ex)
  ((param-type
    :initarg  :param-type
    :initform (error "c-e-param-type must have param-type slot")
    :reader   param-type)))

(defclass c-e-binary (c-ex)
  ((lhs
    :initarg  :lhs
    :initform (error "c-e-binary must have lhs slot")
    :reader   lhs)
   (rhs
    :initarg  :rhs
    :initform (error "c-e-binary must have rhs slot")
    :reader   rhs)))

(defclass c-e-arg-list (c-ex)
  ((arg-list
    :initarg  :arg-list
    :initform '()
    :reader   arg-list)))

(defclass c-e-name (c-ex)
  ((name
    :initarg  :name
    :initform (error "c-e-name must have name slot")
    :reader   name)))

;;------------------------------------------------------------
;; 5 Symbols
;;------------------------------------------------------------

;; 5.1 Symbols

(defclass c-name (c-tp c-e-lval) ())

(defmethod c-name-p (x)
  (typep x 'c-name))

;; 5.2 Names
;;

(defclass c-n-x (c-name)
  ((name
    :initarg  :name
    :initform (error "c-n-x must have name slot")
    :reader   name)))

(defmethod c-n-x ((name string))
  (make-instance 'c-n-x
		 :name name))

(defmethod print-object ((obj c-n-x) stream)
  (with-accessors ((x-name name))
      obj
    (format stream "(c-n-x ~s)" x-name)))

(defmethod c-n-x-p (x)
  (typep x 'c-n-x))

;; 5.3 Symbols with arguments
;;

(defclass c-n-arg (c-name)
  ((name
    :initarg  :name
    :initform (error "c-n-arg must have name slot")
    :reader   name)
   (arg-list
    :initarg  :arg-list
    :initform '()
    :reader   arg-list)))
   

(defmethod c-n-arg ((name c-name) &rest arg-list)
  (let ((arg-list (alexandria:flatten arg-list)))
    (unless (every #'c-tp-p arg-list)
      (error "c-n-arg arg-list must have c-tp members"))
    (make-instance 'c-n-arg
		   :name     name
		   :arg-list arg-list)))

(defmethod print-object ((obj c-n-arg) stream)
  (with-accessors ((name     name)
		   (arg-list arg-list))
      obj
    (format stream
	    "(c-n-arg ~a~a)"
	    name
	    (if arg-list
		(format nil " ~{~a~^ ~}" arg-list)
		""))))

(defmethod c-n-arg-p (x)
  (typep x 'c-n-arg))

;; 5.4 Sub-Symbols
;;

(defclass c-n-sub (c-name)
  ((lhs
    :initarg  :lhs
    :initform (error "c-n-sub must have lhs slot")
    :reader   lhs)
   (rhs
    :initarg  :rhs
    :initform (error "c-n-sub must have rhs slot")
    :reader   rhs)))

(defmethod c-n-sub ((n1 c-name) &rest n-list)
  (let ((n-list (alexandria:flatten n-list)))
    (if n-list
	(let ((rhs (car n-list)))
	  (unless (c-name-p rhs)
	    (error "c-n-sub n-list must have c-name members"))
	  (when (c-n-sub-p rhs)
	    (error "c-n-sub n-list must not have any c-n-sub member"))
	  (c-n-sub (make-instance 'c-n-sub :lhs n1 :rhs (car n-list)) (cdr n-list)))
	n1)))

(defmethod print-object ((obj c-n-sub) stream)
  (with-accessors ((sub-lhs lhs)
		   (sub-rhs rhs))
      obj
    (format stream "(c-n-sub ~a ~a)" sub-lhs sub-rhs)))

(defmethod c-n-sub-p (x)
  (typep x 'c-n-sub))
   
;; 5.5 Expression Instances
;;

(defclass c-e-obj (c-ex) ())

(defmethod c-e-obj ()
  (make-instance 'c-e-obj))

(defmethod c-e-map ()
  (c-e-obj))

(defmethod c-e-list ()
  (c-e-obj))

(defclass c-e-++ (c-e-expr) ())

(defmethod c-e-++ ((expr c-e-lval))
  (make-instance 'c-e-++
		 :expr expr))

(defclass c-e--- (c-e-expr) ())

(defmethod c-e--- ((expr c-e-lval))
  (make-instance 'c-e---
		 :expr expr))

(defclass c-e-! (c-e-expr) ())

(defmethod c-e-! ((expr c-ex))
  (make-instance 'c-e-!
		 :expr expr))

(defclass c-e-string (c-e-value) ())

(defmethod c-e-string ((value string))
  (make-instance 'c-e-string
		 :value value))

(defmethod print-object ((obj c-e-string) stream)
  (with-accessors ((string-value value))
      obj
    (format stream "(c-e-string ~s)" string-value)))

(defclass c-e-int (c-e-value) ())

(defmethod c-e-int ((value integer))
  (make-instance 'c-e-int
		 :value value))

(defmethod print-object ((obj c-e-int) stream)
  (with-accessors ((value value))
      obj
    (format stream "(c-e-int ~a)" value)))

(defclass c-e-double (c-e-value) ())

(defmethod c-e-double  ((value float))
  (make-instance 'c-e-double
		 :value value))

(defclass c-e-bool (c-e-value) ())

(defmethod c-e-bool (value)
  (make-instance 'c-e-bool
		 :value (if value t nil)))

(defmethod c-e-true ()
  (c-e-bool t))

(defmethod c-e-false ()
  (c-e-bool nil))

(defclass c-e-char (c-e-value) ())

(defmethod c-e-char ((value character))
  (make-instance 'c-e-char
		 :value value))

(defclass c-e-- (c-e-binary) ())

(defmethod c-e-- ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e--
		 :lhs lhs
		 :rhs rhs))

(defclass c-e-/ (c-e-binary) ())

(defmethod c-e-/ ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e-/
		 :lhs lhs
		 :rhs rhs))

(defclass c-e-== (c-e-binary) ())

(defmethod c-e-== ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e-==
		 :lhs lhs
		 :rhs rhs))

(defclass c-e-!= (c-e-binary) ())

(defmethod c-e-!= ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e-!=
		 :lhs lhs
		 :rhs rhs))

(defmethod print-object ((obj c-e-!=) stream)
  (format stream "(c-e-!= ~a ~a)" (lhs obj) (rhs obj)))

(defclass c-e->= (c-e-binary) ())

(defmethod c-e->= ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e->=
		 :lhs lhs
		 :rhs rhs))

(defclass c-e-<  (c-e-binary) ())

(defmethod c-e-< ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e-<
		 :lhs lhs
		 :rhs rhs))

(defclass c-e-> (c-e-binary) ())

(defmethod c-e-> ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e->
		 :lhs lhs
		 :rhs rhs))

(defclass c-e-pair (c-e-binary) ())

(defmethod c-e-pair ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e-pair
		 :lhs lhs
		 :rhs rhs))

(defclass c-e-pointer (c-e-expr) ())

(defmethod c-e-pointer ((expr c-ex))
  (make-instance 'c-e-pointer
		 :expr expr))

(defclass c-e-plain-cast (c-e-param-type c-e-expr) ())

(defmethod c-e-plain-cast ((param-type c-tp) (expr c-ex))
  (make-instance 'c-e-plain-cast
		 :param-type param-type
		 :expr       expr))

(defclass c-e-dynamic-cast (c-e-param-type c-e-expr) ())

(defmethod c-e-dynamic-cast ((param-type c-tp) (expr c-ex))
  (make-instance 'c-e-dynamic-cast
		 :param-type param-type
		 :expr       expr))

(defclass c-e-static-cast (c-e-param-type c-e-expr) ())

(defmethod c-e-static-cast ((param-type c-tp) (expr c-ex))
  (make-instance 'c-e-static-cast
		 :param-type param-type
		 :expr       expr))

(defclass c-e-call (c-e-expr c-e-arg-list c-e-lval) ())

(defmethod c-e-call ((expr c-e-lval) &rest arg-list)
  (let ((arg-list (alexandria:flatten arg-list)))
    (unless (every #'c-ex-p arg-list)
      (error "c-e-call must have c-ex instances in arg-list"))
    (make-instance 'c-e-call :expr expr :arg-list arg-list)))

(defmethod print-object ((obj c-e-call) stream)
  (with-accessors ((expr     expr)
		   (arg-list arg-list))
      obj
    (format stream
	    "(c-e-call ~a~a)"
	    expr
	    (if arg-list
		(format nil " ~{~a~^ ~}" arg-list)
		""))))

(defclass c-e-? (c-ex)
  ((cnd
    :initarg  :cnd
    :initform (error "c-e-? must have cnd slot")
    :reader   cnd)
   (imp
    :initarg :imp
    :initform (error "c-e-? must have imp slot")
    :reader   imp)
   (alt
    :initarg  :alt
    :initform (error "c-e-? must have alt slot")
    :reader   alt)))
    
(defmethod c-e-? ((cnd c-ex) (imp c-ex) (alt c-ex))
  (make-instance 'c-e-? :cnd cnd :imp imp :alt alt))

(defclass c-e-optional (c-ex)
  ((expr
    :initarg  :expr
    :initform nil
    :reader   expr)))

(defmethod c-e-optional (&optional expr)
  (when expr
    (unless (c-ex-p expr)
      (error "c-e-optional expr must be either nil or instance of c-ex")))
  (make-instance 'c-e-optional
		 :expr expr))

(defclass c-e-+ (c-e-arg-list) ())

(defmethod c-e-+ (&rest arg-list)
  (unless (every #'c-ex-p arg-list)
    (error "c-e-+ must have c-ex instances in arg-list"))
  (make-instance 'c-e-+
		 :arg-list arg-list))

(defmethod print-object ((obj c-e-+) stream)
  (with-accessors ((arg-list arg-list))
      obj
    (format stream "(c-e-+")
    (when arg-list
      (format stream " ~{~a~^ ~}" arg-list))
    (format stream ")")))
    

(defclass c-e-* (c-e-arg-list) ())

(defmethod c-e-* (&rest arg-list)
  (unless (every #'c-ex-p arg-list)
    (error "c-e-* must have c-ex instances in arg-list"))
  (make-instance 'c-e-*
		 :arg-list arg-list))

(defclass c-e-or (c-e-arg-list) ())

(defmethod c-e-or (&rest arg-list)
  (unless (every #'c-ex-p arg-list)
    (error "c-e-or must have c-ex instances in arg-list"))
  (make-instance 'c-e-or
		 :arg-list arg-list))
  
(defclass c-e-&& (c-e-arg-list) ())

(defmethod c-e-&& (&rest arg-list)
  (unless (every #'c-ex-p arg-list)
    (error "c-e-&& must have c-ex instances in arg-list"))
  (make-instance 'c-e-&&
		 :arg-list arg-list))

(defclass c-e-vector (c-e-arg-list) ())

(defmethod c-e-vector (&rest arg-list)
  (unless (every #'c-ex-p arg-list)
    (error "c-e-vector must have c-ex instances in arg-list"))
  (make-instance 'c-e-vector
		 :arg-list arg-list))

(defmethod print-object ((obj c-e-vector) stream)
  (with-accessors ((arg-list arg-list))
      obj
    (format stream "(c-e-vector")
    (when arg-list
      (format stream " ~{~a~^ ~}" arg-list))
    (format stream ")")))
	

;; 5.6 L-Value Instances
;;

(defclass c-e-. (c-e-binary c-e-lval) ())

(defmethod c-e-. ((lhs c-e-lval) (rhs c-name))
  (make-instance 'c-e-.
		 :lhs lhs
		 :rhs rhs))

(defmethod print-object ((obj c-e-.) stream)
  (with-accessors ((lhs lhs)
		   (rhs rhs))
      obj
    (format stream "(c-e-. ~a ~a)" lhs rhs)))

(defclass c-e--> (c-e-binary c-e-lval) ())

(defmethod c-e--> ((lhs c-e-lval) (rhs c-name))
  (make-instance 'c-e-->
		 :lhs lhs
		 :rhs rhs))

(defmethod print-object ((obj c-e-->) stream)
  (with-accessors ((lhs lhs)
		   (rhs rhs))
      obj
    (format stream "(c-e--> ~a ~a)" lhs rhs)))

(defclass c-e-subscr (c-e-binary c-e-lval) ())

(defmethod c-e-subscr ((lhs c-e-lval) (rhs c-ex))
  (make-instance 'c-e-subscr
		 :lhs lhs
		 :rhs rhs))

(defclass c-e-deref (c-e-expr c-e-lval) ())

(defmethod c-e-deref ((expr c-e-lval))
  (make-instance 'c-e-deref
		 :expr expr))

(defmethod print-object ((obj c-e-deref) stream)
  (with-accessors ((expr expr))
      obj
    (format stream "(c-e-deref ~a)" expr)))


;; 5.7 Sugar
;;

(defmethod c-simplify-arg ((a c-argument-ellipses))
  a)

(defmethod c-simplify-arg ((a c-argument))
  (with-accessors ((name       name)
		   (param-type param-type)
		   (doc        doc))
      a
    (c-argument name (c-simplify-type param-type) doc)))

(defmethod c-simplify-type ((t1 c-tp))
  t1)

(defmethod c-simplify-type ((t1 c-t-ref))
  (with-accessors ((t2 param-type))
      t1
    (unless (c-t-const-p t2)
      (return-from c-simplify-type t1))
    (with-accessors ((t3 param-type))
	t2
      (unless (c-n-x-p t3)
	(return-from c-simplify-type t1))
      (with-accessors ((name name))
	  t3
	(cond
	  ((string= name "bool")         t3)
	  ((string= name "int")          t3)
	  ((string= name "unsigned int") t3)
	  ((string= name "time_t")       t3)
	  ((string= name "size_t")       t3)
	  ((string= name "char")         t3)
	  ((string= name "double")       t3)
	  (t                             t1))))))
	

(defmethod c-const-ref-argument ((name string) (param-type c-tp) &optional doc)
  (c-argument name (c-t-const-ref param-type) doc))

(defmethod c-t-stringstream ()
  (c-n-sub (c-n-x "std") (c-n-x "stringstream")))

(defmethod c-t-runtime-error ()
  (c-n-sub (c-n-x "std") (c-n-x "runtime_error")))

(defmethod c-t-exception ()
  (c-n-sub (c-n-x "std") (c-n-x "exception")))

(defmethod c-t-bool ()
  (c-n-x "bool"))

(defmethod c-t-string ()
  (c-n-sub (c-n-x "std") (c-n-x "string")))

(defmethod c-t-int ()
  (c-n-x "int"))

(defmethod c-t-nat ()
  (c-n-x "unsigned int"))

(defmethod c-t-time ()
  (c-n-x "time_t"))

(defmethod c-t-struct-tm ()
  (c-n-x "struct tm"))

(defmethod c-t-void ()
  (c-n-x "void"))

(defmethod c-t-size ()
  (c-n-x "size_t"))

(defmethod c-t-char ()
  (c-n-x "char"))

(defmethod c-t-double ()
  (c-n-x "double"))

(defmethod c-t-vector ((elem-type c-tp))
  (c-n-arg (c-n-sub (c-n-x "std") (c-n-x "vector")) elem-type))

(defmethod c-t-pair ((lhs-type c-tp) (rhs-type c-tp))
  (c-n-arg (c-n-sub (c-n-x "std") (c-n-x "pair")) lhs-type rhs-type))

(defmethod c-t-optional ((param-type c-tp))
  (c-n-arg (c-n-sub (c-n-x "std") (c-n-x "experimental") (c-n-x "optional")) param-type))

(defmethod c-t-map ((key-type c-tp) (value-type c-tp))
  (c-n-arg (c-n-sub (c-n-x "std") (c-n-x "map")) key-type value-type))

(defmethod c-t-unique-ptr ((param-type c-tp))
  (c-n-arg (c-n-sub (c-n-x "std") (c-n-x "unique_ptr")) param-type))


;;------------------------------------------------------------
;; 6 Statements
;;------------------------------------------------------------

;; 6.1 Statements
;;

(defclass c-st () ())

(defmethod c-st-p (x)
  (typep x 'c-st))

;; 6.2. Abstract Statements

(defclass c-s-expr (c-st)
  ((expr
    :initarg  :expr
    :initform (error "c-s-expr must have expr slot")
    :reader   expr)))

(defclass c-s-name (c-st)
  ((name
    :initarg  :name
    :initform (error "c-s-name must have name slot")
    :reader   name)))

(defclass c-s-binary (c-st)
  ((lhs
    :initarg  :lhs
    :initform (error "c-s-binary must have lhs slot")
    :reader   lhs)
   (rhs
    :initarg  :rhs
    :initform (error "c-s-binary must have rhs slot")
    :reader   rhs)))

(defclass c-s-body (c-st)
  ((body
    :initarg  :body
    :initform '()
    :reader   body)))

(defclass c-s-arg-list (c-st)
  ((arg-list
    :initarg  :arg-list
    :initform '()
    :reader   arg-list)))

(defclass c-s-cnd (c-st)
  ((cnd
    :initarg  :cnd
    :initform (error "c-s-cnd must have cnd slot")
    :reader   cnd)))

;; 6.3 Statement Instances
;;

(defclass c-s-do (c-s-expr) ())

(defmethod c-s-do ((expr c-ex))
  (make-instance 'c-s-do
		 :expr expr))

(defmethod print-object ((obj c-s-do) stream)
  (with-accessors ((expr expr))
      obj
    (format stream "(c-s-do ~a)" expr)))

(defclass c-s-assign (c-s-binary) ())

(defmethod c-s-assign ((lhs c-e-lval) (rhs c-ex))
  (make-instance 'c-s-assign
		 :lhs lhs :rhs rhs))

(defmethod print-object ((obj c-s-assign) stream)
  (with-accessors ((lhs lhs)
		   (rhs rhs))
      obj
    (format stream "(c-s-assign ~a ~a)" lhs rhs)))

(defclass c-s-+= (c-s-binary) ())

(defmethod c-s-+= ((lhs c-e-lval) (rhs c-ex))
  (make-instance 'c-s-+=
		 :lhs lhs
		 :rhs rhs))

(defclass c-s-continue (c-st) ())

(defmethod c-s-continue ()
  (make-instance 'c-s-continue))

(defclass c-s-break (c-st) ())

(defmethod c-s-break ()
  (make-instance 'c-s-break))

(defclass c-s-return (c-s-expr) ())

(defmethod c-s-return (&optional expr)
  (when expr
    (unless (c-ex-p expr)
      (error "c-s-return expr must be instance of ex")))
  (make-instance 'c-s-return
		 :expr expr))

(defmethod print-object ((obj c-s-return) stream)
  (with-accessors ((expr expr))
      obj
    (format stream "(c-s-return")
    (when expr
      (format stream " ")
      (format stream "~a" expr))
    (format stream ")")))
      

(defclass c-s-define (c-s-name)
  ((param-type
    :initarg  :param-type
    :initform (error "c-s-define must have param-type slot")
    :reader   param-type)
   (expr-list
    :initarg  :expr-list
    :initform '()
    :reader   expr-list)))

(defmethod c-s-define ((name string) (param-type c-tp) &rest expr-list)
  (let ((expr-list (alexandria:flatten expr-list)))
    (unless (every #'c-ex-p expr-list)
      (error "c-s-define expr-list must have c-ex members"))
    (make-instance 'c-s-define
		   :name       name
		   :param-type param-type
		   :expr-list  expr-list)))

(defmethod print-object ((obj c-s-define) stream)
  (with-accessors ((def-name       name)
		   (def-param-type param-type)
		   (def-expr-list  expr-list))
      obj
    (format stream
	    "(c-s-define ~s ~a~a)"
	    def-name
	    def-param-type
	    (if def-expr-list
		(format nil " ~{~a~^ ~}" def-expr-list)
		""))))
	    

(defclass c-s-block (c-s-body) ())

(defmethod c-s-block (&rest body)
  (let ((body (alexandria:flatten body)))
    (unless (every #'c-st-p body)
      (error "c-s-block must have c-st instances in body"))
    (make-instance 'c-s-block :body body)))

(defmethod print-object ((obj c-s-block) stream)
  (with-accessors ((body body))
      obj
    (format stream "(c-s-block")
    (when body
      (format stream " ~{~a~^ ~}" body))
    (format stream ")")))

(defclass c-s-doc (c-s-body)
  ((doc
    :initarg  :doc
    :initform (error "c-s-doc must have doc slot")
    :reader   doc)))

(defmethod c-s-doc ((doc string) &rest body)
  (let ((body (alexandria:flatten body)))
    (unless (every #'c-st-p body)
      (error "c-s-doc must have c-st instances in body"))
    (make-instance 'c-s-doc :doc doc :body body)))

(defclass c-case ()
  ((expr
    :initarg  :expr
    :initform nil
    :reader   expr)
   (body
    :initarg  :body
    :initform '()
    :reader   body)))

(defmethod c-case (expr &rest body)
  (let ((body (alexandria:flatten body)))
    (when expr
      (unless (c-ex-p expr)
	(error "c-case expr, if present, must be an instance of c-ex")))
    (unless (every #'c-st-p body)
      (error "c-case body must contain c-st instances"))
    (make-instance 'c-case
		   :expr expr
		   :body body)))

(defmethod c-case-p (x)
  (typep x 'c-case))

(defclass c-s-switch (c-s-expr)
  ((case-list
    :initarg  :case-list
    :initform '()
    :reader   case-list)))

(defmethod c-s-switch ((expr c-ex) &rest case-list)
  (let ((case-list (alexandria:flatten case-list)))
    (unless (every #'c-case-p case-list)
      (error "c-s-switch case-list must contain c-case instances"))
    (make-instance 'c-s-switch
		   :expr      expr
		   :case-list case-list)))

(defclass c-catch-block ()
  ((name
    :initarg  :name
    :initform (error "c-catch-block must have name")
    :reader   name)
   (except-name
    :initarg  :except-name
    :initform (error "c-catch-block must have except-name slot")
    :reader   except-name)
   (body
    :initarg  :body
    :initform '()
    :reader   body)))
    
(defmethod c-catch-block-p (x)
  (typep x 'c-catch-block))

(defmethod c-catch-block ((name string) (except-name c-name) &rest body)
  (unless (every #'c-st-p body)
    (error "c-catch-block must have c-st instances in body"))
  (make-instance 'c-catch-block
		 :name        name
		 :except-name except-name
		 :body        body))

(defclass c-s-try (c-s-body)
  ((catch-block-list
    :initarg  :catch-block-list
    :initform '()
    :reader   catch-block-list)))

(defmethod c-s-try ((body c-s-block) &rest catch-block-list)
  (unless (every #'c-catch-block-p catch-block-list)
    (error "c-s-try catch-block-list must have c-catch-block elements"))
  (make-instance 'c-s-try
		 :body             body
		 :catch-block-list catch-block-list))

(defclass c-s-foreach (c-s-name c-s-expr c-s-body)
  ((param-type
    :initarg  :param-type
    :initform (error "c-s-foreach must have param-type slot")
    :reader   param-type)))

(defmethod c-s-foreach ((name string) (param-type c-tp) (expr c-ex) &rest body)
  (let ((body (alexandria:flatten body)))
    (unless (every #'c-st-p body)
      (error "c-s-foreach must have c-st instances in body"))
    (make-instance 'c-s-foreach
		   :name       name
		   :param-type param-type
		   :expr       expr
		   :body       body)))

(defclass c-s-if (c-s-cnd)
  ((imp
    :initarg  :imp
    :initform (error "c-s-if must have then slot")
    :reader   imp)
   (alt
    :initarg  :alt
    :initform nil
    :reader   alt)))

(defmethod c-s-if ((cnd c-ex) (imp c-st) &optional alt)
  (when alt
    (unless (c-st-p alt)
      (error "c-s-if alt, if present, must be c-st")))
  (make-instance 'c-s-if :cnd cnd :imp imp :alt alt))

(defmethod print-object ((obj c-s-if) stream)
  (with-accessors ((cnd cnd)
		   (imp imp)
		   (alt alt))
      obj
    (format stream "(c-s-if ~a ~a" cnd imp)
    (when alt
      (format stream " ~a" alt)
    (format stream ")"))))
	

(defclass c-s-for (c-s-name c-s-cnd c-s-body)
  ((n-init
    :initarg  :n-init
    :initform 0
    :reader   n-init)
   (delta-n
    :initarg  :delta-n
    :initform 1
    :reader   delta-n)))

(defmethod c-s-for ((name string) (cnd c-ex) (body c-st) &key (n-init 0) (delta-n 1))
  (unless (integerp n-init)
    (error "n-init must be of type integer"))
  (unless (integerp delta-n)
    (error "delta-n must be of type integer"))
  (unless (not (= 0 delta-n))
    (error "delta-n must not be 0"))
  (make-instance 'c-s-for
		 :name      name
		 :cnd       cnd
		 :body      body
		 :n-init    n-init
		 :delta-n   delta-n))

(defclass c-s-<< (c-s-expr c-s-arg-list) ())

(defmethod c-s-<< (expr &rest arg-list)
  (unless (every #'c-ex-p arg-list)
    (error "c-s-<< must have c-ex instances in arg-list"))
  (make-instance 'c-s-<< :expr expr :arg-list arg-list))

(defclass c-s->> (c-s-expr c-s-arg-list) ())

(defmethod c-s->> (expr &rest arg-list)
  (unless (every #'c-ex-p arg-list)
    (error "c-s->> must have c-ex instances in arg-list"))
  (make-instance 'c-s->> :expr expr :arg-list arg-list))

(defclass c-s-while (c-s-cnd c-s-body) ())

(defmethod c-s-while ((cnd c-ex) (body c-st))
  (make-instance 'c-s-while
		 :cnd cnd
		 :body body))

(defclass c-s-rethrow (c-st) ())

(defmethod c-s-rethrow ()
  (make-instance 'c-s-rethrow))

(defclass c-s-throw (c-s-name c-s-arg-list) ())

(defmethod c-s-throw ((name c-name) &rest arg-list)
  (unless (every #'c-ex-p arg-list)
    (error "c-s-throw arg-list must have c-ex items"))
  (make-instance 'c-s-throw
		 :name     name
		 :arg-list arg-list))

(defmethod print-object ((obj c-s-throw) stream)
  (with-accessors ((name     name)
		   (arg-list arg-list))
      obj
    (format stream "(c-s-throw ~a" name)
    (when arg-list
      (format stream " ~{~a~^ ~}" arg-list))
    (format stream ")")))

;;------------------------------------------------------------
;; 7 Toplevel
;;------------------------------------------------------------

;; 7.1 Toplevel
;;

(defclass c-toplevel () ())

(defmethod c-toplevel-p (x)
  (typep x 'c-toplevel))

;; 7.2 Templatable
;;

(defclass c-templatable (c-toplevel) ())

(defmethod c-templatable-p (x)
  (typep x 'c-templatable))

;; 7.3 Template
;;

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

;; 7.4 Memb
;;
  
(defclass c-memb () ())

(defmethod c-memb-p (x)
  (typep x 'c-memb))

;; 7.5 Protectable
;;

(defmethod c-protect-p (x)
  (or (eq :public x)
      (eq :protected x)
      (eq :private x)))

(defclass c-protectable (c-memb)
  ((protect
    :initarg  :protect
    :initform (error "c-protectable must have protect slot")
    :reader   protect)))

(defmethod c-protectable-p (x)
  (typep x 'c-protectable))

;; 7.6 Classes
;;

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
  
;; 7.12 Functions
;;

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

	  

;; 7.14 Using
;;

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

(defgeneric c-->string (arg)
  (:documentation "convert argument to string")
  (:method (arg)
    (error (format nil
		   "c-->string not defined for value of type ~a"
		   (type-of arg)))))

;; type
(defmethod c-->string ((c c-n-sub))
  (with-accessors ((sub-lhs lhs)
		   (sub-rhs rhs))
      c
    (format nil "~a::~a" (c-->string sub-lhs) (c-->string sub-rhs))))

(defmethod c-->string ((c c-n-arg))
  (with-accessors ((name      name)
		   (arg-list arg-list))
      c
    (format nil "~a<~{~a~^, ~}>" (c-->string name) (mapcar #'c-->string arg-list))))

(defmethod c-->string ((arg c-t-const))        (format nil "~a const" (c-->string (param-type arg))))
(defmethod c-->string ((arg c-t-ref))          (format nil
						   "~a&"
						   (c-->string (param-type arg))))
(defmethod c-->string ((arg c-t-pointer))      (format nil
						   "~a*"
						   (c-->string (param-type arg))))

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

;; expression
(defmethod c-->string ((c c-n-x))
  (name c))

(defmethod c-->string ((arg c-e-or))
  (with-accessors ((arg-list arg-list))
      arg
    (cond
      ((null arg-list)         "false")
      ((= 1 (length arg-list)) (c-->string (car arg-list)))
      (t                       (format
				nil
				"( ~{~a~^ || ~} )"
				(mapcar #'c-->string arg-list))))))

(defmethod c-->string ((arg c-e-&&))
  (with-accessors ((arg-list arg-list))
      arg
    (cond
      ((null arg-list)         "true")
      ((= 1 (length arg-list)) (c-->string (car arg-list)))
      (t                       (format
				nil
				"( ~{~a~^ && ~} )"
				(mapcar #'c-->string arg-list))))))

(defmethod c-->string ((arg c-e-obj))          "{}")
(defmethod c-->string ((arg c-e-++))           (format nil "++~a" (c-->string (expr arg))))
(defmethod c-->string ((arg c-e---))           (format nil "--~a" (c-->string (expr arg))))
(defmethod c-->string ((arg c-e-!))            (format nil "!( ~a )" (c-->string (expr arg))))
(defmethod c-->string ((arg c-e-string))       (format nil "~s" (value arg)))
(defmethod c-->string ((arg c-e-int))          (format nil "~a" (value arg)))
(defmethod c-->string ((arg c-e-double))       (format nil "~a" (value arg)))
(defmethod c-->string ((arg c-e-bool))         (if (value arg) "true" "false"))
(defmethod c-->string ((arg c-e-char))         (let ((c (value arg)))
					     (cond
					       ((eq c #\linefeed)
						"'\\n'")
					       ((eq c #\return)
						"'\\r'")
					       ((eq c #\tab)
						"'\\t'")
					       ((eq c #\')
						"'\\''")
					       (t
						(format nil "'~c'" c)))))
					     
(defmethod c-->string ((arg c-e-call))
  (let ((arg-list (arg-list arg)))
    (if arg-list
	(format nil
		"~a( ~{~a~^, ~} )"
		(c-->string (expr arg))
		(mapcar #'c-->string
			arg-list))
	(format nil
		"~a()"
		(c-->string (expr arg))))))

(defmethod c-->string ((arg c-e-?))            (format nil
						   "( ~a ? ~a : ~a )"
						   (c-->string (cnd arg))
						   (c-->string (imp arg))
						   (c-->string (alt arg))))

(defmethod c-->string ((e c-e-+))
  (let ((al (arg-list e)))
    (cond
      ((null al)         "0")
      ((= 1 (length al)) (c-->string (car al)))
      (t                 (format nil
				 "( ~{~a~^+~} )"
				 (mapcar #'c-->string
					 al))))))

(defmethod c-->string ((arg c-e--))            (format nil
						   "( ~a-~a )"
						   (c-->string (lhs arg))
						   (c-->string (rhs arg))))

(defmethod c-->string ((e c-e-*))
  (let ((al (arg-list e)))
    (cond
      ((null al)         "1")
      ((= 1 (length al)) (c-->string (car al)))
      (t                 (format nil
				 "( ~{~a~^*~} )"
				 (mapcar #'c-->string
					 al))))))

  
(defmethod c-->string ((arg c-e-/))            (format nil
						   "( ~a/~a )"
						   (c-->string (lhs arg))
						   (c-->string (rhs arg))))

(defmethod c-->string ((arg c-e-==))           (format nil
						   "~a == ~a"
						   (c-->string (lhs arg))
						   (c-->string (rhs arg))))

(defmethod c-->string ((arg c-e-!=))           (format nil
						   "~a != ~a"
						   (c-->string (lhs arg))
						   (c-->string (rhs arg))))

(defmethod c-->string ((arg c-e->=))           (format nil
						   "~a >= ~a"
						   (c-->string (lhs arg))
						   (c-->string (rhs arg))))

(defmethod c-->string ((arg c-e-<))            (format nil
						   "~a < ~a"
						   (c-->string (lhs arg))
						   (c-->string (rhs arg))))

(defmethod c-->string ((arg c-e->))            (format nil
						   "~a > ~a"
						   (c-->string (lhs arg))
						   (c-->string (rhs arg))))

(defmethod c-->string ((arg c-e-vector))       (let ((arg-list (arg-list arg)))
					     (if arg-list
						 (format nil
							 "{ ~{~a~^, ~} }"
							 (mapcar #'c-->string
								 arg-list))
						 "{}")))

(defmethod c-->string ((e c-e-optional))
  (let ((e1 (expr e)))
    (if e1
	(format nil
		"{ ~a }"
		(c-->string e1))
	"std::experimental::nullopt")))

(defmethod c-->string ((arg c-e-pointer))      (format nil
						   "&~a"
						   (c-->string (expr arg))))

(defmethod c-->string ((arg c-e-plain-cast))   (format nil
						   "(~a)( ~a )"
						   (c-->string (param-type arg))
						   (c-->string (expr arg))))

(defmethod c-->string ((arg c-e-dynamic-cast)) (format nil
						   "dynamic_cast<~a>( ~a )"
						   (c-->string (param-type arg))
						   (c-->string (expr arg))))

(defmethod c-->string ((arg c-e-static-cast))  (format nil
						   "static_cast<~a>( ~a )"
						   (c-->string (param-type arg))
						   (c-->string (expr arg))))
						

;; lval
(defmethod c-->string ((c c-e-.))              (format nil
						   "~a.~a"
						   (c-->string (lhs c))
						   (c-->string (rhs c))))
(defmethod c-->string ((c c-e-->))             (format nil
						   "~a->~a"
						   (c-->string (lhs c))
						   (c-->string (rhs c))))

(defmethod c-->string ((arg c-e-subscr))       (format nil
						   "~a[~a]"
						   (c-->string (lhs arg))
						   (c-->string (rhs arg))))

(defmethod c-->string ((arg c-e-deref))        (format nil
						   "*~a"
						   (c-->string (expr arg))))

(defmethod c-->string ((e c-e-pair))
  (format nil "{ ~a, ~a }" (c-->string (lhs e)) (c-->string (rhs e))))

;; statements
(defmethod c-->string ((arg c-case))
  (with-accessors ((expr expr)
		   (body body))
      arg
    (format
     nil
     "~a:~%~a"
     (if expr
	 (format nil "case ~a" (c-->string expr))
	 "default")
     (indent
      (format
       nil
       "~{~a~^~%~}"
       (mapcar #'c-->string body))))))
			 

(defmethod c-->string ((arg c-s-switch))
  (with-accessors ((expr      expr)
		   (case-list case-list))
      arg
    (format
     nil
     "switch( ~a )~%~a"
     (c-->string expr)
     (indent
      (cond
	(case-list
	 (format
	  nil
	  "{~%~a~%}"
	  (format
	   nil
	   "~{~a~^~%~}"
	   (mapcar #'c-->string case-list))))
	(t
	 "{}"))))))
	
			    
	  

(defmethod c-->string ((s c-s-do))
  (format nil
	  "~a;"
	  (c-->string (expr s))))

(defmethod c-->string ((c c-s-define))
  (with-accessors ((def-name       name)
		   (def-param-type param-type)
		   (def-expr-list  expr-list))
      c
    (cond
      (def-expr-list (format nil
			     "~a ~a { ~{~a~^, ~} };"
			     (c-->string def-param-type)
			     def-name
			     (mapcar #'c-->string def-expr-list)))
      (t             (format nil
			     "~a ~a {};"
			     (c-->string def-param-type)
			     def-name)))))

(defmethod c-->string ((s c-s-assign))
  (let ((arg  (lhs s))
	(expr (rhs s)))
    (format nil
	    "~a = ~a;"
	    (c-->string arg)
	    (c-->string expr))))

(defmethod c-->string ((arg c-s-+=))
  (format nil
	  "~a += ~a;"
	  (c-->string (lhs arg))
	  (c-->string (rhs arg))))

(defmethod c-->string ((arg c-s-block))
  (let ((body (body arg)))
    (if body
	(format nil
		"{~%~a~%}"
		(indent
		 (format nil
			 "~{~a~^~%~}"
			 (mapcar #'c-->string
				 body))))
	"{}")))

(defmethod c-->string ((s c-s-doc))
  (format nil "~%~a~%~{~a~^~%~}" (comment (doc s)) (mapcar #'c-->string (body s))))


(defmethod c-->string ((arg c-s-try))
  (format nil
	  "try~%~a~{~a~}"
	  (indent (c-->string (body arg)))
	  (mapcar #'(lambda (c)
		      (let ((name        (name c))
			    (except-name (except-name c))
			    (body        (body c)))
			(format nil
				"~%catch( ~a ~a )~%~a"
				(c-->string (c-t-const-ref except-name))
				name
				(indent (c-->string (apply #'c-s-block body))))))
		  (catch-block-list arg))))

(defmethod c-->string ((arg c-s-foreach))
  (let ((name (name arg))
	(type (param-type arg))
	(expr (expr arg))
	(body (body arg)))
    (format nil
	    "for( ~a ~a : ~a )~%~a"
	    (c-->string type)
	    name
	    (c-->string expr)
	    (indent (c-->string (apply #'c-s-block body))))))

(defmethod c-->string ((arg c-s-return))
  (with-accessors ((expr expr))
      arg
    (if expr
	(format nil "return ~a;" (c-->string expr))
	"return;")))

(defmethod c-->string ((s c-s-if))
  (with-accessors ((cnd  cnd)
		   (then imp)
		   (else alt))
      s
    (with-output-to-string (buf)
      (format buf
	      "if( ~a )~%"
	      (c-->string cnd))
      (format buf (indent (c-->string then)))
      (when else
	(format buf
		"~%else~%~a"
		(indent (c-->string else)))))))

(defmethod c-->string ((arg c-s-continue))
  "continue;")

(defmethod c-->string ((arg c-s-break))
  "break;")

(defmethod c-->string ((arg c-s-for))
  (let ((name    (name arg))
	(n-init  (n-init arg))
	(delta-n (delta-n arg))
	(cnd     (cnd  arg))
	(body    (body arg)))
    (format nil
	    "for( size_t ~a { ~a }; ~a; ~a )~%~a"
	    name
	    n-init
	    (c-->string cnd)
	    (cond
	      ((= 1 delta-n)  (format nil "++~a" name))
	      ((= -1 delta-n) (format nil "--~a" name))
	      ((< delta-n 0)  (format nil "~a -= ~a" name (- delta-n)))
	      (t              (format nil "~a += ~a" name delta-n)))
	    (indent (c-->string body)))))

(defmethod c-->string ((arg c-s-<<))
  (format nil
	  "~{~a~^ << ~};"
	  (mapcar #'c-->string (cons (expr arg) (arg-list arg)))))

(defmethod c-->string ((arg c-s->>))
  (format nil
	  "~{~a~^ >> ~};"
	  (mapcar #'c-->string (cons (expr arg) (arg-list arg)))))

(defmethod c-->string ((arg c-s-while))
  (format nil
	  "while( ~a )~%~a"
	  (c-->string (cnd arg))
	  (indent (c-->string (body arg)))))

(defmethod c-->string ((arg c-s-rethrow))
  "throw;")

(defmethod c-->string ((arg c-s-throw))
  (format nil
	  "throw ~a ~a;"
	  (c-->string (name arg))
	  (let ((al (arg-list arg)))
	    (if al
		(format nil "{ ~{~a~^, ~} }" (mapcar #'c-->string al))
		"{}"))))

(defmethod c-->string ((arg c-argument-ellipses))
  "...")
		
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

;;------------------------------------------------------------
;; Utility Functions
;;------------------------------------------------------------

(defgeneric c-name-eq (a b))

(defmethod c-name-eq ((a c-name) (b c-name))
  (string= (c-->string a) (c-->string b)))

(defmethod c-name-eq ((a string) (b c-name))
  (string= a (c-->string b)))

(defmethod c-name-eq ((a c-name) (b string))
  (string= (c-->string a) b))

(defmethod c-name-eq ((a string) (b string))
  (string= a b))

(defmethod c-find-dep (&rest x-list)
  (let* ((x-list (alexandria:flatten x-list))
	 (y-list (mapcar #'c-find-dep-single x-list)))
    (sort
     (reduce
      #'(lambda (a b) (union a b :test #'string=))
      y-list
      :initial-value '())
     #'string<)))

(defgeneric c-find-dep-single (x)
  (:documentation
   "
(c-find-dep-single X)

Takes a single argument X which must be an instance of either c-tp or c-ex and traverses
it to find all system dependencies of X. Returns a string list enumerating the library
names the argument depends on.
"))

(defmethod c-find-dep-single ((x c-tp))
  '())

(defmethod c-find-dep-single ((x c-n-arg))
  (with-accessors ((name     name)
		   (arg-list arg-list))
      x
    (c-find-dep name arg-list)))

(defmethod c-find-dep-single ((c c-name))
  (cond
    ((c-name-eq c "std::string")                 (list "string"))
    ((c-name-eq c "std::vector")                 (list "vector"))
    ((c-name-eq c "std::pair")                   (list "utility"))
    ((c-name-eq c "std::map")                    (list "map"))
    ((c-name-eq c "std::unique_ptr")             (list "memory"))
    ((c-name-eq c "std::stringstream")           (list "sstream"))
    ((c-name-eq c "std::cin")                    (list "iostream"))
    ((c-name-eq c "std::cout")                   (list "iostream"))
    ((c-name-eq c "std::runtime_error")          (list "stdexcept"))
    ((c-name-eq c "std::exception")              (list "stdexcept"))
    ((c-name-eq c "std::experimental::optional") (list "experimental/optional"))
    ((c-name-eq c "std::function")               (list "functional"))
    ((c-name-eq c "uint64_t")                    (list "cstdint"))
    ((c-name-eq c "int64_t")                     (list "cstdint"))
    ((c-name-eq c "time_t")                      (list "ctime"))
    ((c-name-eq c "size_t")                      (list "cstddef"))
    ((c-name-eq c "memset")                      (list "cstring"))
    ((c-name-eq c "xmlSAXHandler")               (list "libxml/parser.h"))
    ((c-name-eq c "xmlTextWriterPtr")            (list "libxml/xmlwriter.h"))
    ((c-name-eq c "json_t")                      (list "jansson.h"))
    (t                                           '())))

(defmethod c-find-dep-single ((x c-t-ref))
  (c-find-dep-single (param-type x)))

(defmethod c-find-dep-single ((x c-t-pointer))
  (c-find-dep-single (param-type x)))

(defmethod c-find-dep-single ((x c-t-function))
  (c-find-dep (ret-type x)
	      (arg-list x)))

(defmethod c-find-dep-single ((x c-ex))
  ;; (format t "skipping expression ~a~%" x)
  '())

(defmethod c-find-dep-single ((x c-e-value))
  '())

(defmethod c-find-dep-single ((x c-e-.))
  (c-find-dep (lhs x) (rhs x)))

(defmethod c-find-dep-single ((x c-e-->))
  (c-find-dep (lhs x) (rhs x)))

(defmethod c-find-dep-single ((x c-e-call))
  (with-accessors ((expr expr)
		   (arg-list arg-list))
      x
    (c-find-dep expr arg-list)))

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

(defmethod c-find-dep-single ((x c-argument-ellipses))
  '())

(defmethod c-find-dep-single ((x c-argument))
  (c-find-dep-single (param-type x)))

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

(defmethod c-find-dep-single ((x c-s-while))
  (c-find-dep (cnd x) (body x)))

(defmethod c-find-dep-single ((x c-s-do))
  (c-find-dep-single (expr x)))

(defmethod c-find-dep-single ((c c-s-define))
  (with-accessors ((def-param-type param-type)
		   (def-expr-list  expr-list))
      c
    (c-find-dep def-param-type def-expr-list)))

(defmethod c-find-dep-single ((x c-s-<<))
  (with-accessors ((expr     expr)
		   (arg-list arg-list))
      x
    (c-find-dep expr arg-list)))

(defmethod c-find-dep-single ((x c-s->>))
  (with-accessors ((expr     expr)
		   (arg-list arg-list))
      x
    (c-find-dep expr arg-list)))

(defmethod c-find-dep-single ((x c-s-doc))
  (c-find-dep (body x)))

(defmethod c-find-dep-single ((x c-s-return))
  (with-accessors ((expr expr))
      x
    (if expr
	(c-find-dep-single expr)
	'())))

(defmethod c-find-dep-single ((x c-s-assign))
  (c-find-dep (lhs x) (rhs x)))

(defmethod c-find-dep-single ((x c-s-if))
  (c-find-dep (cnd x) (imp x) (alt x)))

(defmethod c-find-dep-single ((x c-s-switch))
  (c-find-dep (expr x) (case-list x)))

(defmethod c-find-dep-single ((x c-case))
  (c-find-dep (expr x) (body x)))

(defmethod c-find-dep-single ((x c-s-throw))
  (c-find-dep (name x) (arg-list x)))

(defmethod c-find-dep-single ((x c-s-block))
  (c-find-dep (body x)))

(defmethod c-find-dep-single ((x c-s-break))
  '())

(defmethod c-find-dep-single ((x c-s-continue))
  '())

(defmethod c-find-dep-single ((x c-s-for))
  (c-find-dep (cnd x) (body x)))

(defmethod c-find-dep-single ((x c-s-foreach))
  (c-find-dep (param-type x) (expr x) (body x)))

(defmethod c-find-dep-single ((x c-catch-block))
  (c-find-dep (body x)))

(defmethod c-find-dep-single ((x c-s-try))
  (c-find-dep (catch-block-list x)))

(defmethod c-find-dep-single ((x c-s-rethrow))
  '())

(defmethod c-find-dep-single ((x c-st))
  (format t "skipping statement ~a~%" x)
  '())

(defmethod c-find-dep-single ((x c-using))
  (c-find-dep-single (param-type x)))


  
