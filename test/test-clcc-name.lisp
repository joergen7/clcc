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

(in-package :clcc/test)
(in-suite clcc-suite)

(test clcc-name-predicate
  (is (c-n-x-p (c-n-x "x"))))

(test clcc-name-construct-format

  (let ((t1 (c-t-vector (c-n-x "blub"))))
    (is (equal "blub" (name (car (arg-list t1)))))
    (is (c-name-eq "std::vector" (name t1)))
    (is (c-name-eq "std::vector<blub>" t1)))

  (is
   (string-equal
	"t<>"
	(c-->string (c-n-arg (c-n-x "t")))))

  (is
   (string-equal
	"t<a>"
	(c-->string (c-n-arg (c-n-x "t") (c-n-x "a")))))

  (is
   (string-equal
	"t<a, b>"
	(c-->string (c-n-arg (c-n-x "t") (c-n-x "a") (c-n-x "b")))))

  (signals
	  (error "c-n-arg failed to signal error on non-type argument")
	(c-n-arg (c-n-x "x") 5))

  (signals
	  (error "c-n-sub failed to signal error on non-name argument")
	(c-n-sub (c-n-x "a") (c-t-const (c-n-x "a"))))

  (is
   (string-equal
	"t"
	(c-->string (c-n-sub (c-n-x "t")))))

  (is
   (string-equal
	"a::b"
	(c-->string (c-n-sub (c-n-x "a") (c-n-x "b")))))

  (is
   (string-equal
	"a::b::c"
	(c-->string (c-n-sub (c-n-x "a") (c-n-x "b") (c-n-x "c")))))

  (signals
	  (error "c-n-sub failed to signal error on sub argument")
	(c-n-sub (c-n-x "a") (c-n-sub (c-n-x "b") (c-n-x "c")))))


(test clcc-name-print
  (is
   (string-equal
	"(c-n-arg (c-n-x \"t\"))"
	(format nil "~a" (c-n-arg (c-n-x "t")))))

  (is
   (string-equal
	"(c-n-arg (c-n-x \"t\") (c-n-x \"a\"))"
	(format nil "~a" (c-n-arg (c-n-x "t") (c-n-x "a")))))

  (is
   (string-equal
	"(c-n-arg (c-n-x \"t\") (c-n-x \"a\") (c-n-x \"b\"))"
	(format nil "~a" (c-n-arg (c-n-x "t") (c-n-x "a") (c-n-x "b")))))

  (is
   (string-equal
	"(c-n-sub (c-n-x \"a\") (c-n-x \"b\"))"
	(format nil "~a" (c-n-sub (c-n-x "a") (c-n-x "b")))))

  (is
   (string-equal
	"(c-e-. (c-n-x \"a\") (c-n-x \"b\"))"
	(format nil "~a" (c-e-. (c-n-x "a") (c-n-x "b")))))

  (is
   (string-equal
	"(c-e--> (c-n-x \"a\") (c-n-x \"b\"))"
	(format nil "~a" (c-e--> (c-n-x "a") (c-n-x "b"))))))

(test clcc-name-c-find-dep-single

  (is (null (c-find-dep-single (c-n-x "int"))))

  (is (equal '("vector") (c-find-dep-single (c-t-vector (c-t-bool)))))
  (is (equal '("ctime" "vector") (c-find-dep-single (c-t-vector (c-t-time)))))

  (is (equal '("utility") (c-find-dep-single (c-t-pair (c-t-bool) (c-t-bool)))))
  (is (equal '("ctime" "utility") (c-find-dep-single (c-t-pair (c-t-time) (c-t-bool)))))
  (is (equal '("ctime" "utility") (c-find-dep-single (c-t-pair (c-t-bool) (c-t-time)))))

  (is (equal '("experimental/optional") (c-find-dep-single (c-t-optional (c-t-bool)))))
  (is (equal '("ctime" "experimental/optional") (c-find-dep-single (c-t-optional (c-t-time)))))


  (is (equal '("map") (c-find-dep-single (c-t-map (c-t-bool) (c-t-bool)))))
  (is (equal '("map" "string") (c-find-dep-single (c-t-map (c-t-string) (c-t-bool)))))
  (is (equal '("map" "string") (c-find-dep-single (c-t-map (c-t-bool) (c-t-string)))))

  (is (null (c-find-dep-single (c-t-bool))))
  (is (equal '("string") (c-find-dep-single (c-t-string))))
  (is (equal '() (c-find-dep-single (c-t-int))))
  (is (equal '() (c-find-dep-single (c-t-nat))))
  (is (equal '("ctime") (c-find-dep-single (c-t-time))))
  (is (null (c-find-dep-single (c-t-struct-tm))))
  (is (null (c-find-dep-single (c-t-void))))
  (is (equal '("cstddef") (c-find-dep-single (c-t-size))))
  (is (null (c-find-dep-single (c-t-char))))

  (is (null (c-find-dep-single (c-t-function '() (c-t-void)))))

  (is (equal '("memory") (c-find-dep-single (c-t-unique-ptr (c-t-bool)))))
  (is (equal '("ctime" "memory") (c-find-dep-single (c-t-unique-ptr (c-t-time)))))
  (is (equal '("sstream") (c-find-dep-single (c-n-sub (c-n-x "std") (c-n-x "stringstream")))))
  (is (equal '("sstream") (c-find-dep-single (c-static-func "f" (list (c-argument "x" (c-n-sub (c-n-x "std") (c-n-x "stringstream")))) (c-t-void)))))

  (is (null (c-find-dep-single (c-e-int 5))))
  (is (null (c-find-dep-single (c-n-x "x"))))
  (is (equal '("cstring") (c-find-dep-single (c-n-x "memset"))))
  (is (equal '("cstring") (c-find-dep-single (c-e-call (c-n-x "memset")))))


  (is (null (c-find-dep-single (c-field :public (c-argument "x" (c-t-bool))))))
  (is (equal '("ctime") (c-find-dep-single (c-field :public (c-argument "x" (c-t-time))))))

  (is (null (c-find-dep-single (c-meth :public nil (c-info "m") '() (c-t-void)))))
  (is (equal '("ctime") (c-find-dep-single (c-meth :public nil (c-info "m") (list (c-argument "x" (c-t-time))) (c-t-void)))))
  (is (equal '("ctime") (c-find-dep-single (c-abstract-meth :public nil (c-info "m") (list (c-argument "x" (c-t-time))) (c-t-void)))))
  
  (is (null (c-find-dep-single (c-cls (c-info "c") '()))))
  (is (equal '("ctime")
	     (c-find-dep-single (c-cls (c-info "c")
				       '()
				       (c-field :public (c-argument "x" (c-t-time)))))))
  
  (is (equal '("stdexcept" "string") (c-find-dep-single (c-enum (c-info "e")))))

  (is-false (c-find-dep-single (c-func (c-info "f") '() (c-t-void))))
  (is (equal '("ctime") (c-find-dep-single (c-func (c-info "f") '() (c-t-time)))))
  (is (equal '("ctime") (c-find-dep-single (c-func (c-info "f") (list (c-argument "x" (c-t-time))) (c-t-void)))))
  (is (equal '("libxml/parser.h") (c-find-dep-single (c-n-x "xmlSAXHandler"))))
  (is (equal '("cstring") (c-find-dep-single (c-destructor (c-s-do (c-n-x "memset"))))))
  (is (equal '("cstring") (c-find-dep-single (c-constructor '() '() (c-s-do (c-n-x "memset"))))))
  (is (equal '("ctime") (c-find-dep-single (c-constructor (list (c-argument "x" (c-t-time))) '()))))
  (is (equal '() (c-find-dep-single (c-s-return))))

  (is
   (string-equal
	"iostream"
	(car (c-find-dep-single (c-e-cin)))))

  (is
   (string-equal
	"iostream"
	(car (c-find-dep-single (c-e-cout)))))

  (is
   (string-equal
	"stdexcept"
	(car (c-find-dep-single (c-t-runtime-error)))))

  (is
   (string-equal
	"stdexcept"
	(car (c-find-dep-single (c-t-exception)))))

  (is
   (string-equal
	"functional"
	(car (c-find-dep-single (c-n-sub (c-n-x "std") (c-n-x "function"))))))

  (is
   (string-equal
	"cstdint"
	(car (c-find-dep-single (c-n-x "uint64_t")))))

  (is
   (string-equal
	"cstdint"
	(car (c-find-dep-single (c-n-x "int64_t")))))

  (is
   (string-equal
	"libxml/xmlWriter.h"
	(car (c-find-dep-single (c-n-x "xmlTextWriterPtr")))))

  (is
   (string-equal
	"jansson.h"
	(car (c-find-dep-single (c-n-x "json_t")))))

  (is
   (string-equal
	"ctime"
	(car (c-find-dep-single (c-e-. (c-t-time) (c-n-x "a"))))))

  (is
   (string-equal
	"ctime"
	(car (c-find-dep-single (c-e-. (c-n-x "a") (c-t-time))))))

  (is
   (string-equal
	"ctime"
	(car (c-find-dep-single (c-e--> (c-t-time) (c-n-x "a"))))))

  (is
   (string-equal
	"ctime"
	(car (c-find-dep-single (c-e--> (c-n-x "a") (c-t-time)))))))


(test clcc-name-sugar

  (is
   (c-name-eq
	"std::stringstream"
	(c-t-stringstream)))

  (is
   (c-name-eq
	"std::runtime_error"
	(c-t-runtime-error)))

  (is
   (c-name-eq
	"std::exception"
	(c-t-exception)))

  (is
   (c-name-eq
	"double"
	(c-t-double))))

(test clcc-name-c-name-eq
  (is (c-name-eq (c-n-x "x") (c-n-x "x")))
  (is (c-name-eq (c-n-x "x") "x"))
  (is (c-name-eq "x" (c-n-x "x")))
  (is (c-name-eq "x" "x")))

(test clcc-name-simplify-arg

  (is
   (c-argument-ellipses-p (c-simplify-arg (c-argument-ellipses))))

  (let ((a (c-simplify-arg (c-argument "a" (c-n-x "t")))))
	(with-accessors ((name       name)
					 (param-type param-type))
		a
	  (is (string-equal "a" name))
	  (is (c-name-eq "t" param-type)))))

(test clcc-name-simplify-type

  (is
   (c-name-eq "t" (c-simplify-type (c-n-x "t"))))

  (let ((t1 (c-simplify-type (c-t-ref (c-n-x "t")))))
	(is (string-equal "t&" (c-->string t1))))

  (let ((t1 (c-simplify-type (c-t-ref (c-t-const (c-t-pointer (c-n-x "t")))))))
	(is (string-equal "t* const&" (c-->string t1))))

  (let ((t1 (c-simplify-type (c-t-ref (c-t-const (c-n-x "t"))))))
	(is (string-equal "t const&" (c-->string t1))))

  (let ((t1 (c-simplify-type (c-t-ref (c-t-const (c-n-x "bool"))))))
	(is (string-equal "bool" (c-->string t1))))

  (let ((t1 (c-simplify-type (c-t-ref (c-t-const (c-n-x "int"))))))
	(is (string-equal "int" (c-->string t1))))

  (let ((t1 (c-simplify-type (c-t-ref (c-t-const (c-n-x "unsigned int"))))))
	(is (string-equal "unsigned int" (c-->string t1))))

  (let ((t1 (c-simplify-type (c-t-ref (c-t-const (c-n-x "time_t"))))))
	(is (string-equal "time_t" (c-->string t1))))

  (let ((t1 (c-simplify-type (c-t-ref (c-t-const (c-n-x "size_t"))))))
	(is (string-equal "size_t" (c-->string t1))))

  (let ((t1 (c-simplify-type (c-t-ref (c-t-const (c-n-x "char"))))))
	(is (string-equal "char" (c-->string t1))))

  (let ((t1 (c-simplify-type (c-t-ref (c-t-const (c-n-x "double"))))))
	(is (string-equal "double" (c-->string t1)))))


