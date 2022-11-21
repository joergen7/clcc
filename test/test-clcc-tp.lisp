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

(test clcc-tp-predicate

  (is (c-argument-p (c-argument "x" (c-n-x "int"))))
  (is (c-tp-p (c-n-x "blub")))
  (is (c-t-function-p (c-t-function '() (c-t-string))))
  (is (c-t-const-p (c-t-const (c-t-string)))))

(test clcc-tp-construct-format

  (let ((arg (c-argument-ellipses)))
    (is
	 (string-equal
	  "..."
	  (c-->string arg))))

  (is
   (string-equal
	"..."
	(c-->string (c-argument-ellipses "documentation"))))

  (signals
	  (error "c-argument-ellipses failed to signal error on bad doc string")
	(c-argument-ellipses 5))

  (let ((arg (c-argument "a" (c-t-void))))
    (is
	 (string-equal
	  "void a"
	  (c-->string arg))))

  (let ((arg (c-argument "f" (c-t-function (list (c-argument "x" (c-t-nat))) (c-t-void)))))
    (is
	 (string-equal
	  "void (*f)( unsigned int x )"
	  (c-->string arg))))

  (let ((arg (c-argument "a" (c-t-int) "this is a, an integer")))
	(is
	 (string-equal
	  "int a"
	  (c-->string arg))))

  (signals
	  (error "c-argument failed to signal error on bad doc string")
	(c-argument "a" (c-t-int) 5))

  (is
   (string-equal
	"std::string const&"
	(c-->string (c-t-const-ref (c-t-string)))))

  (is
   (string-equal
	"std::string const*"
	(c-->string (c-t-const-pointer (c-t-string)))))

  (is
   (string-equal
	"std::string const& s"
	(c-->string (c-const-ref-argument "s" (c-t-string))))))

(test clcc-tp-find-dep

  (is (equal '("ctime") (c-find-dep-single (c-t-ref (c-t-time)))))
  (is (equal '("ctime") (c-find-dep-single (c-t-pointer (c-t-time)))))
  (is (equal '("ctime") (c-find-dep-single (c-t-function '() (c-t-time)))))
  (is (equal '("ctime") (c-find-dep-single (c-t-function (list (c-argument "n" (c-t-time))) (c-t-void)))))
  (is (null (c-find-dep-single (c-argument-ellipses)))))

(test clcc-tp-print

  (is
   (string-equal
	"(c-argument \"a\" (c-n-x \"int\"))"
	(format nil "~a" (c-argument "a" (c-n-x "int")))))

  (is
   (string-equal
	"(c-argument \"a\" (c-n-x \"int\") \"this is an info\")"
	(format nil "~a" (c-argument "a" (c-n-x "int") "this is an info"))))

  (is
   (string-equal
	"(c-t-const (c-n-x \"int\"))"
	(format nil "~a" (c-t-const (c-n-x "int")))))

  (is
   (string-equal
	"(c-t-ref (c-n-x \"int\"))"
	(format nil "~a" (c-t-ref (c-n-x "int")))))

  (is
   (string-equal
	"(c-t-pointer (c-n-x \"int\"))"
	(format nil "~a" (c-t-pointer (c-n-x "int"))))))
