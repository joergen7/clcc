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

;;------------------------------------------------------------
;; Statements
;;------------------------------------------------------------

(defclass c-st () ())

(defmethod c-st-p (x)
  (typep x 'c-st))


;;------------------------------------------------------------
;; Statement Instances
;;------------------------------------------------------------

;; c-s-do

(defclass c-s-do (c-st)
  ((expr
	:initarg  :expr
	:initform (error "c-s-do must have expr slot")
	:reader   expr)))

(defmethod c-s-do ((expr c-ex))
  (make-instance 'c-s-do
		 :expr expr))

(defmethod print-object ((obj c-s-do) stream)
  (with-accessors ((expr expr))
      obj
    (format stream "(c-s-do ~a)" expr)))

(defmethod c-->string ((s c-s-do))
  (format nil
		  "~a;"
		  (c-->string (expr s))))

(defmethod c-find-dep-single ((x c-s-do))
  (c-find-dep-single (expr x)))



;; c-s-assign

(defclass c-s-assign (c-st)
  ((lhs
	:initarg  :lhs
	:initform (error "c-s-assign must have lhs slot")
	:reader   lhs)
   (rhs
	:initarg  :rhs
	:initform (error "c-s-assign must have rhs slot")
	:reader   rhs)))

(defmethod c-s-assign ((lhs c-e-lval) (rhs c-ex))
  (make-instance 'c-s-assign
		 :lhs lhs :rhs rhs))

(defmethod print-object ((obj c-s-assign) stream)
  (with-accessors ((lhs lhs)
		   (rhs rhs))
      obj
    (format stream "(c-s-assign ~a ~a)" lhs rhs)))

(defmethod c-->string ((s c-s-assign))
  (let ((arg  (lhs s))
	(expr (rhs s)))
    (format nil
	    "~a = ~a;"
	    (c-->string arg)
	    (c-->string expr))))

(defmethod c-find-dep-single ((x c-s-assign))
  (c-find-dep (lhs x) (rhs x)))

;; c-s-+=

(defclass c-s-+= (c-st)
  ((lhs
	:initarg  :lhs
	:initform (error "c-s-+= must have lhs slot")
	:reader   lhs)
   (rhs
	:initarg  :rhs
	:initform (error "c-s-+= must have rhs slot")
	:reader   rhs)))

(defmethod c-s-+= ((lhs c-e-lval) (rhs c-ex))
  (make-instance 'c-s-+=
		 :lhs lhs
		 :rhs rhs))

(defmethod c-->string ((arg c-s-+=))
  (format nil
		  "~a += ~a;"
		  (c-->string (lhs arg))
		  (c-->string (rhs arg))))

;; c-s-continue

(defclass c-s-continue (c-st) ())

(defmethod c-s-continue ()
  (make-instance 'c-s-continue))

(defmethod c-->string ((arg c-s-continue))
  "continue;")

(defmethod c-find-dep-single ((x c-s-continue))
  '())

;; c-s-break

(defclass c-s-break (c-st) ())

(defmethod c-s-break ()
  (make-instance 'c-s-break))

(defmethod c-->string ((arg c-s-break))
  "break;")

(defmethod c-find-dep-single ((x c-s-break))
  '())

;; c-s-return

(defclass c-s-return (c-st)
  ((expr
	:initarg  :expr
	:initform (error "c-s-return must have expr slot")
	:reader   expr)))

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
      
(defmethod c-->string ((arg c-s-return))
  (with-accessors ((expr expr))
      arg
    (if expr
	(format nil "return ~a;" (c-->string expr))
	"return;")))

(defmethod c-find-dep-single ((x c-s-return))
  (with-accessors ((expr expr))
      x
    (if expr
	(c-find-dep-single expr)
	'())))



;; c-s-define

(defclass c-s-define (c-st)
  ((name
	:initarg  :name
	:initform (error "c-s-define must have name slot")
	:reader   name)
   (param-type
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

(defmethod c-find-dep-single ((c c-s-define))
  (with-accessors ((def-param-type param-type)
		   (def-expr-list  expr-list))
      c
    (c-find-dep def-param-type def-expr-list)))



;; c-s-block

(defclass c-s-block (c-st)
  ((body
	:initarg  :body
	:initform '()
	:reader   body)))

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

(defmethod c-find-dep-single ((x c-s-block))
  (c-find-dep (body x)))

;; c-s-doc

(defclass c-s-doc (c-st)
  ((doc
    :initarg  :doc
    :initform (error "c-s-doc must have doc slot")
    :reader   doc)
   (body
	:initarg  :body
	:initform '()
	:reader   body)))

(defmethod c-s-doc ((doc string) &rest body)
  (let ((body (alexandria:flatten body)))
    (unless (every #'c-st-p body)
      (error "c-s-doc must have c-st instances in body"))
    (make-instance 'c-s-doc :doc doc :body body)))

(defmethod c-->string ((s c-s-doc))
  (format nil "~%~a~%~{~a~^~%~}" (comment (doc s)) (mapcar #'c-->string (body s))))

(defmethod c-find-dep-single ((x c-s-doc))
  (c-find-dep (body x)))

;; c-case

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

(defmethod c-find-dep-single ((x c-case))
  (c-find-dep (expr x) (body x)))

;; c-s-switch

(defclass c-s-switch (c-st)
  ((expr
	:initarg  :expr
	:initform (error "c-s-switch must have expr slot")
	:reader   expr)
   (case-list
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
	
(defmethod c-find-dep-single ((x c-s-switch))
  (c-find-dep (expr x) (case-list x)))

;; c-catch-block

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

(defmethod c-->string ((x c-catch-block))
  (with-accessors ((name        name)
				   (except-name except-name)
				   (body        body))
	  x
	(format nil
			"~%catch( ~a ~a )~%~a"
			(c-->string (c-t-const-ref except-name))
			name
			(indent (c-->string (apply #'c-s-block body))))))

(defmethod c-find-dep-single ((x c-catch-block))
  (c-find-dep (body x)))

;; c-s-try

(defclass c-s-try (c-st)
  ((body
	:initarg  :body
	:initform '()
	:reader   body)
   (catch-block-list
    :initarg  :catch-block-list
    :initform '()
    :reader   catch-block-list)))

(defmethod c-s-try ((body c-s-block) &rest catch-block-list)
  (unless (every #'c-catch-block-p catch-block-list)
    (error "c-s-try catch-block-list must have c-catch-block elements"))
  (make-instance 'c-s-try
		 :body             body
		 :catch-block-list catch-block-list))

(defmethod c-->string ((x c-s-try))
  (with-accessors ((body             body)
				   (catch-block-list catch-block-list))
	  x
	(format nil
			"try~%~a~{~a~}"
			(indent (c-->string body))
			(mapcar #'c-->string catch-block-list))))

(defmethod c-find-dep-single ((x c-s-try))
  (c-find-dep (catch-block-list x)))

;; c-s-foreach

(defclass c-s-foreach (c-st)
  ((name
	:initarg  :name
	:initform (error "c-s-foreach must have name slot")
	:reader   name)
   (param-type
    :initarg  :param-type
    :initform (error "c-s-foreach must have param-type slot")
    :reader   param-type)
   (expr
	:initarg  :expr
	:initform (error "c-s-foreach must have expr slot")
	:reader   expr)
   (body
	:initarg  :body
	:initform '()
	:reader   body)))

(defmethod c-s-foreach ((name string) (param-type c-tp) (expr c-ex) &rest body)
  (let ((body (alexandria:flatten body)))
    (unless (every #'c-st-p body)
      (error "c-s-foreach must have c-st instances in body"))
    (make-instance 'c-s-foreach
		   :name       name
		   :param-type param-type
		   :expr       expr
		   :body       body)))

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

(defmethod c-find-dep-single ((x c-s-foreach))
  (c-find-dep (param-type x) (expr x) (body x)))



;; c-s-if

(defclass c-s-if (c-st)
  ((cnd
	:initarg  :cnd
	:initform (error "c-s-if must have cnd slot")
	:reader   cnd)
   (imp
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

(defmethod c-find-dep-single ((x c-s-if))
  (c-find-dep (cnd x) (imp x) (alt x)))

;; c-s-for

(defclass c-s-for (c-st)
  ((name
	:initarg  :name
	:initform (error "c-s-for must have name slot")
	:reader   name)
   (cnd
	:initarg  :cnd
	:initform (error "c-s-for must have cnd slot")
	:reader   cnd)
   (body
	:initarg  :body
	:initform '()
	:reader   body)
   (n-init
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

(defmethod c-find-dep-single ((x c-s-for))
  (c-find-dep (cnd x) (body x)))

;; c-s-<<

(defclass c-s-<< (c-st)
  ((expr
	:initarg  :expr
	:initform (error "c-s-<< must have expr slot")
	:reader   expr)
   (arg-list
	:initarg  :arg-list
	:initform '()
	:reader   arg-list)))

(defmethod c-s-<< (expr &rest arg-list)
  (unless (every #'c-ex-p arg-list)
    (error "c-s-<< must have c-ex instances in arg-list"))
  (make-instance 'c-s-<< :expr expr :arg-list arg-list))

(defmethod c-->string ((arg c-s-<<))
  (format nil
		  "~{~a~^ << ~};"
		  (mapcar #'c-->string (cons (expr arg) (arg-list arg)))))

(defmethod c-find-dep-single ((x c-s-<<))
  (with-accessors ((expr     expr)
		   (arg-list arg-list))
      x
    (c-find-dep expr arg-list)))

;; c-s->>

(defclass c-s->> (c-st)
  ((expr
	:initarg  :expr
	:initform (error "c-s->> must have expr slot")
	:reader   expr)
   (arg-list
	:initarg  :arg-list
	:initform '()
	:reader   arg-list)))

(defmethod c-s->> (expr &rest arg-list)
  (unless (every #'c-ex-p arg-list)
    (error "c-s->> must have c-ex instances in arg-list"))
  (make-instance 'c-s->> :expr expr :arg-list arg-list))

(defmethod c-->string ((arg c-s->>))
  (format nil
		  "~{~a~^ >> ~};"
		  (mapcar #'c-->string (cons (expr arg) (arg-list arg)))))

(defmethod c-find-dep-single ((x c-s->>))
  (with-accessors ((expr     expr)
		   (arg-list arg-list))
      x
    (c-find-dep expr arg-list)))

;; c-s-while

(defclass c-s-while (c-st)
  ((cnd
	:initarg  :cnd
	:initform (error "c-s-while must have cnd slot")
	:reader   cnd)
   (body
	:initarg  :body
	:initform '()
	:reader   body)))

(defmethod c-s-while ((cnd c-ex) (body c-st))
  (make-instance 'c-s-while
		 :cnd cnd
		 :body body))

(defmethod c-->string ((arg c-s-while))
  (format nil
	  "while( ~a )~%~a"
	  (c-->string (cnd arg))
	  (indent (c-->string (body arg)))))

(defmethod c-find-dep-single ((x c-s-while))
  (c-find-dep (cnd x) (body x)))


;; c-s-rethrow

(defclass c-s-rethrow (c-st) ())

(defmethod c-s-rethrow ()
  (make-instance 'c-s-rethrow))

(defmethod c-->string ((arg c-s-rethrow))
  "throw;")

(defmethod c-find-dep-single ((x c-s-rethrow))
  '())

;; c-s-throw

(defclass c-s-throw (c-st)
  ((name
	:initarg  :name
	:initform (error "c-s-throw must have name slot")
	:reader   name)
   (arg-list
	:initarg  :arg-list
	:initform '()
	:reader   arg-list)))

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

(defmethod c-->string ((arg c-s-throw))
  (format nil
		  "throw ~a ~a;"
		  (c-->string (name arg))
		  (let ((al (arg-list arg)))
			(if al
				(format nil "{ ~{~a~^, ~} }" (mapcar #'c-->string al))
				"{}"))))

(defmethod c-find-dep-single ((x c-s-throw))
  (c-find-dep (name x) (arg-list x)))











