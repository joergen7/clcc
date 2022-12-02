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
;; Expressions
;;------------------------------------------------------------

(defclass c-ex () ())

(defmethod c-ex-p (x)
  (typep x 'c-ex))

(defmethod string-c-ex-p (x)
  (and (consp x)
       (stringp (car x))
       (c-ex-p (cdr x))))

(defmethod c-find-dep-single ((e c-ex))
  '())

;;------------------------------------------------------------
;; L-Values
;;------------------------------------------------------------

(defclass c-e-lval (c-ex) ())

;;------------------------------------------------------------
;; Expression Instances
;;------------------------------------------------------------

;; c-e-obj

(defclass c-e-obj (c-ex) ())

(defmethod c-e-obj ()
  (make-instance 'c-e-obj))

(defmethod c-->string ((arg c-e-obj))
  "{}")

;; c-e-++

(defclass c-e-++ (c-ex)
  ((expr
	:initarg  :expr
	:initform (error "c-e-++ must have expr slot")
	:reader   expr)))

(defmethod c-e-++ ((expr c-e-lval))
  (make-instance 'c-e-++
		 :expr expr))

(defmethod c-->string ((arg c-e-++))
  (format nil "++~a" (c-->string (expr arg))))

;; c-e---

(defclass c-e--- (c-ex)
  ((expr
	:initarg  :expr
	:initform (error "c-e--- must have expr slot")
	:reader   expr)))

(defmethod c-e--- ((expr c-e-lval))
  (make-instance 'c-e---
		 :expr expr))

(defmethod c-->string ((arg c-e---))
  (format nil "--~a" (c-->string (expr arg))))

;; c-e-!

(defclass c-e-! (c-ex)
  ((expr
	:initarg  :expr
	:initform (error "c-e-! must have expr slot")
	:reader   expr)))

(defmethod c-e-! ((expr c-ex))
  (make-instance 'c-e-!
		 :expr expr))

(defmethod c-->string ((arg c-e-!))
  (format nil "!( ~a )" (c-->string (expr arg))))

;; c-e-string

(defclass c-e-string (c-ex)
  ((value
	:initarg  :value
	:initform (error "c-e-string must have value slot")
	:reader   value)))

(defmethod c-e-string ((value string))
  (make-instance 'c-e-string
		 :value value))

(defmethod print-object ((obj c-e-string) stream)
  (with-accessors ((string-value value))
      obj
    (format stream "(c-e-string ~s)" string-value)))

(defmethod c-->string ((arg c-e-string))
  (format nil "~s" (value arg)))

;; c-e-int

(defclass c-e-int (c-ex)
  ((value
	:initarg  :value
	:initform (error "c-e-int must have value slot")
	:reader   value)))

(defmethod c-e-int ((value integer))
  (make-instance 'c-e-int
		 :value value))

(defmethod print-object ((obj c-e-int) stream)
  (with-accessors ((value value))
      obj
    (format stream "(c-e-int ~a)" value)))

(defmethod c-->string ((arg c-e-int))
  (format nil "~a" (value arg)))

;; c-e-double

(defclass c-e-double (c-ex)
  ((value
	:initarg  :value
	:initform (error "c-e-double must have value slot")
	:reader   value)))

(defmethod c-e-double  ((value float))
  (make-instance 'c-e-double
		 :value value))

(defmethod c-->string ((arg c-e-double))
  (format nil "~a" (value arg)))

;; c-e-bool

(defclass c-e-bool (c-ex)
  ((value
	:initarg  :value
	:initform (error "c-e-bool must have value slot")
	:reader   value)))

(defmethod c-e-bool (value)
  (make-instance 'c-e-bool
		 :value (if value t nil)))

(defmethod c-->string ((arg c-e-bool))
  (if (value arg) "true" "false"))

(defmethod print-object ((obj c-e-bool) stream)
  (format stream "(c-e-bool ~a)" (if (value obj) "t" "nil")))


;; c-e-char

(defclass c-e-char (c-ex)
  ((value
	:initarg  :value
	:initform (error "c-e-char must have value slot")
	:reader   value)))

(defmethod c-e-char ((value character))
  (make-instance 'c-e-char
		 :value value))

(defmethod c-->string ((arg c-e-char))
  (let ((c (value arg)))
	(cond
	  ((eq c #\linefeed) "'\\n'")
	  ((eq c #\return)   "'\\r'")
	  ((eq c #\tab)      "'\\t'")
	  ((eq c #\')        "'\\''")
	  (t                 (format nil "'~c'" c)))))

;; c-e--

(defclass c-e-- (c-ex)
  ((lhs
	:initarg  :lhs
	:initform (error "c-e-- must have lhs slot")
	:reader   lhs)
   (rhs
	:initarg  :rhs
	:initform (error "c-e-- must have rhs slot")
	:reader   rhs)))

(defmethod c-e-- ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e--
		 :lhs lhs
		 :rhs rhs))

(defmethod c-->string ((arg c-e--))
  (format nil
		  "( ~a-~a )"
		  (c-->string (lhs arg))
		  (c-->string (rhs arg))))

;; c-e-/

(defclass c-e-/ (c-ex)
  ((lhs
	:initarg  :lhs
	:initform (error "c-e-/ must have lhs slot")
	:reader   lhs)
   (rhs
	:initarg  :rhs
	:initform (error "c-e-/ must have rhs slot")
	:reader   rhs)))

(defmethod c-e-/ ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e-/
		 :lhs lhs
		 :rhs rhs))

(defmethod c-->string ((arg c-e-/))
  (format nil
		  "( ~a/~a )"
		  (c-->string (lhs arg))
		  (c-->string (rhs arg))))

;; c-e-==

(defclass c-e-== (c-ex)
  ((lhs
	:initarg  :lhs
	:initform (error "c-e-== must have lhs slot")
	:reader   lhs)
   (rhs
	:initarg  :rhs
	:initform (error "c-e-== must have rhs slot")
	:reader   rhs)))

(defmethod c-e-== ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e-==
		 :lhs lhs
		 :rhs rhs))

(defmethod c-->string ((arg c-e-==))
  (format nil
		  "~a == ~a"
		  (c-->string (lhs arg))
		  (c-->string (rhs arg))))

;; c-e-!=

(defclass c-e-!= (c-ex)
  ((lhs
	:initarg  :lhs
	:initform (error "c-e-!= must have lhs slot")
	:reader   lhs)
   (rhs
	:initarg  :rhs
	:initform (error "c-e-!= must have rhs slot")
	:reader   rhs)))

(defmethod c-e-!= ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e-!=
		 :lhs lhs
		 :rhs rhs))

(defmethod print-object ((obj c-e-!=) stream)
  (format stream "(c-e-!= ~a ~a)" (lhs obj) (rhs obj)))

(defmethod c-->string ((arg c-e-!=))
  (format nil
		  "~a != ~a"
		  (c-->string (lhs arg))
		  (c-->string (rhs arg))))

;; c-e->=

(defclass c-e->= (c-ex)
  ((lhs
	:initarg  :lhs
	:initform (error "c-e->= must have lhs slot")
	:reader   lhs)
   (rhs
	:initarg  :rhs
	:initform (error "c-e->= must have rhs slot")
	:reader   rhs)))


(defmethod c-e->= ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e->=
		 :lhs lhs
		 :rhs rhs))

(defmethod c-->string ((arg c-e->=))
  (format nil
		  "~a >= ~a"
		  (c-->string (lhs arg))
		  (c-->string (rhs arg))))

;; c-e-<

(defclass c-e-<  (c-ex)
  ((lhs
	:initarg  :lhs
	:initform (error "c-e-< must have lhs slot")
	:reader   lhs)
   (rhs
	:initarg  :rhs
	:initform (error "c-e-< must have rhs slot")
	:reader   rhs)))


(defmethod c-e-< ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e-<
		 :lhs lhs
		 :rhs rhs))

(defmethod c-->string ((arg c-e-<))
  (format nil
		  "~a < ~a"
		  (c-->string (lhs arg))
		  (c-->string (rhs arg))))

;; c-e->

(defclass c-e-> (c-ex)
  ((lhs
	:initarg  :lhs
	:initform (error "c-e-> must have lhs slot")
	:reader   lhs)
   (rhs
	:initarg  :rhs
	:initform (error "c-e-> must have rhs slot")
	:reader   rhs)))


(defmethod c-e-> ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e->
		 :lhs lhs
		 :rhs rhs))

(defmethod c-->string ((arg c-e->))
  (format nil
		  "~a > ~a"
		  (c-->string (lhs arg))
		  (c-->string (rhs arg))))

;; c-e-pair

(defclass c-e-pair (c-ex)
  ((lhs
	:initarg  :lhs
	:initform (error "c-e-pair must have lhs slot")
	:reader   lhs)
   (rhs
	:initarg  :rhs
	:initform (error "c-e-pair must have rhs slot")
	:reader   rhs)))


(defmethod c-e-pair ((lhs c-ex) (rhs c-ex))
  (make-instance 'c-e-pair
		 :lhs lhs
		 :rhs rhs))

(defmethod c-->string ((e c-e-pair))
  (format nil "{ ~a, ~a }" (c-->string (lhs e)) (c-->string (rhs e))))

;; c-e-pointer

(defclass c-e-pointer (c-ex)
  ((expr
	:initarg  :expr
	:initform (error "c-e-pointer must have expr slot")
	:reader   expr)))

(defmethod c-e-pointer ((expr c-ex))
  (make-instance 'c-e-pointer
		 :expr expr))

(defmethod c-->string ((arg c-e-pointer))
  (format nil
		  "&~a"
		  (c-->string (expr arg))))

;; c-e-plain-cast

(defclass c-e-plain-cast (c-ex)
  ((param-type
	:initarg  :param-type
	:initform (error "c-e-plain-cast must have param-type slot")
	:reader   param-type)
   (expr
	:initarg  :expr
	:initform (error "c-e-plain-cast must have expr slot")
	:reader   expr)))

(defmethod c-e-plain-cast ((param-type c-tp) (expr c-ex))
  (make-instance 'c-e-plain-cast
		 :param-type param-type
		 :expr       expr))

(defmethod c-->string ((arg c-e-plain-cast))
  (format nil
		  "(~a)( ~a )"
		  (c-->string (param-type arg))
		  (c-->string (expr arg))))

;; c-e-dynamic-cast

(defclass c-e-dynamic-cast (c-ex)
  ((param-type
	:initarg  :param-type
	:initform (error "c-e-dynamic-cast must have param-type slot")
	:reader   param-type)
   (expr
	:initarg  :expr
	:initform (error "c-e-dynamic-cast must have expr slot")
	:reader   expr)))

(defmethod c-e-dynamic-cast ((param-type c-tp) (expr c-ex))
  (make-instance 'c-e-dynamic-cast
		 :param-type param-type
		 :expr       expr))

(defmethod c-->string ((arg c-e-dynamic-cast))
  (format nil
		  "dynamic_cast<~a>( ~a )"
		  (c-->string (param-type arg))
		  (c-->string (expr arg))))

;; c-e-static-cast

(defclass c-e-static-cast (c-ex)
  ((param-type
	:initarg  :param-type
	:initform (error "c-e-static-cast must have param-type slot")
	:reader   param-type)
   (expr
	:initarg  :expr
	:initform (error "c-e-static-cast must have expr slot")
	:reader   expr)))

(defmethod c-e-static-cast ((param-type c-tp) (expr c-ex))
  (make-instance 'c-e-static-cast
		 :param-type param-type
		 :expr       expr))

(defmethod c-->string ((arg c-e-static-cast))  (format nil
						   "static_cast<~a>( ~a )"
						   (c-->string (param-type arg))
						   (c-->string (expr arg))))
						

;; c-e-call
;;
;; In general, calls are expressions. In addition, a call can be
;; an L-value, e.g., if the called function returns a reference.
;; Although some calls are no L-value, calls are often used
;; in places where only L-values can appear. Thus, we declare all
;; calls to be L-values to allow them to appear in places
;; restricted to L-values. We rely on the C++ compiler to catch
;; that inconsistency.

(defclass c-e-call (c-e-lval)
  ((expr
	:initarg  :expr
	:initform (error "c-e-call must have expr slot")
	:reader   expr)
   (arg-list
	:initarg  :arg-list
	:initform '()
	:reader   arg-list)))

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

(defmethod c-find-dep-single ((x c-e-call))
  (with-accessors ((expr expr)
		   (arg-list arg-list))
      x
    (c-find-dep expr arg-list)))


;; c-e-?

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

(defmethod c-->string ((arg c-e-?))
  (format nil
		  "( ~a ? ~a : ~a )"
		  (c-->string (cnd arg))
		  (c-->string (imp arg))
		  (c-->string (alt arg))))

;; c-e-optional

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

(defmethod c-->string ((e c-e-optional))
  (let ((e1 (expr e)))
    (if e1
	(format nil
		"{ ~a }"
		(c-->string e1))
	"std::experimental::nullopt")))


;; c-e-+

(defclass c-e-+ (c-ex)
  ((arg-list
	:initarg  :arg-list
	:initform '()
	:reader   arg-list)))

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
    
(defmethod c-->string ((e c-e-+))
  (let ((al (arg-list e)))
    (cond
      ((null al)         "0")
      ((= 1 (length al)) (c-->string (car al)))
      (t                 (format nil
				 "( ~{~a~^+~} )"
				 (mapcar #'c-->string
					 al))))))

;; c-e-*

(defclass c-e-* (c-ex)
  ((arg-list
	:initarg  :arg-list
	:initform '()
	:reader   arg-list)))

(defmethod c-e-* (&rest arg-list)
  (unless (every #'c-ex-p arg-list)
    (error "c-e-* must have c-ex instances in arg-list"))
  (make-instance 'c-e-*
		 :arg-list arg-list))

(defmethod c-->string ((e c-e-*))
  (let ((al (arg-list e)))
    (cond
      ((null al)         "1")
      ((= 1 (length al)) (c-->string (car al)))
      (t                 (format nil
								 "( ~{~a~^*~} )"
								 (mapcar #'c-->string
										 al))))))


;; c-e-or

;; Note, the vertical bar | is a reserved delimiter in Common
;; Lisp. To avoid that letter, we call the facility "or"
;; instead of "||", which would have been consistent with
;; naming conventions.

(defclass c-e-or (c-ex)
  ((arg-list
	:initarg  :arg-list
	:initform '()
	:reader   arg-list)))

(defmethod c-e-or (&rest arg-list)
  (unless (every #'c-ex-p arg-list)
    (error "c-e-or must have c-ex instances in arg-list"))
  (make-instance 'c-e-or
		 :arg-list arg-list))

(defmethod c-->string ((e c-e-or))
  (with-accessors ((arg-list arg-list))
      e
    (cond
      ((null arg-list)         "false")
      ((= 1 (length arg-list)) (c-->string (car arg-list)))
      (t                       (format
								nil
								"( ~{~a~^ || ~} )"
								(mapcar #'c-->string arg-list))))))

;; c-e-&&

(defclass c-e-&& (c-ex)
  ((arg-list
	:initarg  :arg-list
	:initform '()
	:reader   arg-list)))

(defmethod c-e-&& (&rest arg-list)
  (unless (every #'c-ex-p arg-list)
    (error "c-e-&& must have c-ex instances in arg-list"))
  (make-instance 'c-e-&&
		 :arg-list arg-list))

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

;; c-e-vector

(defclass c-e-vector (c-ex)
  ((arg-list
	:initarg  :arg-list
	:initform '()
	:reader   arg-list)))

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
	
(defmethod c-->string ((arg c-e-vector))
  (let ((arg-list (arg-list arg)))
	(if arg-list
		(format nil
				"{ ~{~a~^, ~} }"
				(mapcar #'c-->string
						arg-list))
		"{}")))


;;------------------------------------------------------------
;; 4.4 L-Value Instances
;;------------------------------------------------------------

;; c-e-subscr

(defclass c-e-subscr (c-e-lval)
  ((lhs
	:initarg  :lhs
	:initform (error "c-e-subscr must have lhs slot")
	:reader   lhs)
   (rhs
	:initarg  :rhs
	:initform (error "c-e-subscr must have rhs slot")
	:reader   rhs)))

(defmethod c-e-subscr ((lhs c-e-lval) (rhs c-ex))
  (make-instance 'c-e-subscr
		 :lhs lhs
		 :rhs rhs))

(defmethod c-->string ((arg c-e-subscr))
  (format nil
		  "~a[~a]"
		  (c-->string (lhs arg))
		  (c-->string (rhs arg))))


;; c-e-deref

(defclass c-e-deref (c-e-lval)
  ((expr
	:initarg  :expr
	:initform (error "c-e-deref must have expr slot")
	:reader   expr)))

(defmethod c-e-deref ((expr c-e-lval))
  (make-instance 'c-e-deref
		 :expr expr))

(defmethod print-object ((obj c-e-deref) stream)
  (with-accessors ((expr expr))
      obj
    (format stream "(c-e-deref ~a)" expr)))

(defmethod c-->string ((arg c-e-deref))
  (format nil
		  "*~a"
		  (c-->string (expr arg))))


;;------------------------------------------------------------
;; Sugar
;;------------------------------------------------------------

(defmethod c-e-true ()
  (c-e-bool t))

(defmethod c-e-false ()
  (c-e-bool nil))

(defmethod c-e-map ()
  (c-e-obj))

(defmethod c-e-list ()
  (c-e-obj))


