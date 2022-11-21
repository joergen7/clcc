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
;; 5 Names
;;------------------------------------------------------------

;; 5.1 Names

(defclass c-name (c-tp c-e-lval) ())

(defmethod c-name-p (x)
  (typep x 'c-name))

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

;; 5.2 Symbols
;;

(defclass c-n-x (c-name)
  ((name
    :initarg  :name
    :initform (error "c-n-x must have name slot")
    :reader   name)))

(defmethod c-n-x ((name string))
  (make-instance 'c-n-x
				 :name name))

(defmethod c-n-x-p (x)
  (typep x 'c-n-x))

(defmethod print-object ((obj c-n-x) stream)
  (with-accessors ((x-name name))
      obj
    (format stream "(c-n-x ~s)" x-name)))

(defmethod c-->string ((c c-n-x))
  (name c))


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

(defmethod c-n-arg-p (x)
  (typep x 'c-n-arg))

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

(defmethod c-->string ((c c-n-arg))
  (with-accessors ((name      name)
				   (arg-list arg-list))
      c
    (format nil "~a<~{~a~^, ~}>" (c-->string name) (mapcar #'c-->string arg-list))))

(defmethod c-find-dep-single ((x c-n-arg))
  (with-accessors ((name     name)
				   (arg-list arg-list))
      x
    (c-find-dep name arg-list)))


;;------------------------------------------------------------
;; Sub-Symbols
;;------------------------------------------------------------

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

(defmethod c-n-sub-p (x)
  (typep x 'c-n-sub))

(defmethod print-object ((obj c-n-sub) stream)
  (with-accessors ((sub-lhs lhs)
				   (sub-rhs rhs))
      obj
    (format stream "(c-n-sub ~a ~a)" sub-lhs sub-rhs)))

(defmethod c-->string ((c c-n-sub))
  (with-accessors ((sub-lhs lhs)
				   (sub-rhs rhs))
      c
    (format nil "~a::~a" (c-->string sub-lhs) (c-->string sub-rhs))))


;;------------------------------------------------------------
;; Additional L-Value Instances
;;------------------------------------------------------------
;;
;; Here, we continue defining L-value instances, adding
;; name-specific L-values to the L-value instances introduced
;; in clcc-ex.lisp.

;; c-e-.

(defclass c-e-. (c-e-lval)
  ((lhs
	:initarg  :lhs
	:initform (error "c-e-. must have lhs slot")
	:reader   lhs)
   (rhs
	:initarg  :rhs
	:initform (error "c-e-. must have rhs slot")
	:reader   rhs)))

(defmethod c-e-. ((lhs c-e-lval) (rhs c-name))
  (make-instance 'c-e-.
				 :lhs lhs
				 :rhs rhs))

(defmethod print-object ((obj c-e-.) stream)
  (with-accessors ((lhs lhs)
				   (rhs rhs))
      obj
    (format stream "(c-e-. ~a ~a)" lhs rhs)))

(defmethod c-->string ((c c-e-.))
  (format nil
		  "~a.~a"
		  (c-->string (lhs c))
		  (c-->string (rhs c))))

(defmethod c-find-dep-single ((x c-e-.))
  (c-find-dep (lhs x) (rhs x)))

;; c-e-->

(defclass c-e--> (c-e-lval)
  ((lhs
	:initarg  :lhs
	:initform (error "c-e--> must have lhs slot")
	:reader   lhs)
   (rhs
	:initarg  :rhs
	:initform (error "c-e--> must have rhs slot")
	:reader   rhs)))

(defmethod c-e--> ((lhs c-e-lval) (rhs c-name))
  (make-instance 'c-e-->
				 :lhs lhs
				 :rhs rhs))

(defmethod print-object ((obj c-e-->) stream)
  (with-accessors ((lhs lhs)
				   (rhs rhs))
      obj
    (format stream "(c-e--> ~a ~a)" lhs rhs)))

(defmethod c-->string ((c c-e-->))
  (format nil
		  "~a->~a"
		  (c-->string (lhs c))
		  (c-->string (rhs c))))

(defmethod c-find-dep-single ((x c-e-->))
  (c-find-dep (lhs x) (rhs x)))

;;------------------------------------------------------------
;; Sugar
;;------------------------------------------------------------

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
;; Utilities
;;------------------------------------------------------------

(defgeneric c-name-eq (a b)
  (:documentation "(c-name-eq A B)

Compares two names A and B and returns T when both compile to the same
C++ representation. A or B might also be plain strings, in which case
the function assumes that the string is a compilation result.

The function is reflexive, transitive, and symmetric.

Examples:
(c-name-eq (c-n-x \"blub\") (c-n-x \"blub\"))                            --> T
(c-name-eq (c-n-sub (c-n-x \"std\") (c-n-x \"string\")) \"std::string\") --> T
"))

(defmethod c-name-eq ((a c-name) (b c-name))
  (string= (c-->string a) (c-->string b)))

(defmethod c-name-eq ((a string) (b c-name))
  (string= a (c-->string b)))

(defmethod c-name-eq ((a c-name) (b string))
  (string= (c-->string a) b))

(defmethod c-name-eq ((a string) (b string))
  (string= a b))

(defgeneric c-simplify-arg (a)
  (:documentation "(c-simplify-arg A)

Simplify the type of argument A.

Examples:
(c-simplify-arg (c-argument-ellipses))        --> (c-argument-ellipses)
(c-simplify-arg (c-argument \"a\" (c-t-int))) --> (c-t-int)
"))

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
	
