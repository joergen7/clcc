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

(test clcc-ex-predicate

  (is (string-c-ex-p (cons "a" (c-e-int 5)))))

(test clcc-ex-sugar

  (is (string-equal "true" (c-->string (c-e-true))))
  (is (string-equal "false" (c-->string (c-e-false))))
  (is (string-equal "{}" (c-->string (c-e-map))))
  (is (string-equal "{}" (c-->string (c-e-list)))))

(test clcc-ex-print

  (is
   (string-equal
	"(c-e-string \"blub\")"
	(format nil "~a" (c-e-string "blub"))))

  (is
   (string-equal
	"(c-e-int 5)"
	(format nil "~a" (c-e-int 5))))

  (is
   (string-equal
	"(c-e-!= (c-e-int 4) (c-e-int 3))"
	(format nil "~a" (c-e-!= (c-e-int 4) (c-e-int 3)))))

  (is
   (string-equal
	"(c-e-call (c-n-x \"f\"))"
	(format nil "~a" (c-e-call (c-n-x "f")))))

  (is
   (string-equal
	"(c-e-call (c-n-x \"f\") (c-n-x \"x\"))"
	(format nil "~a" (c-e-call (c-n-x "f") (c-n-x "x")))))

  (is
   (string-equal
	"(c-e-+ (c-n-x \"x\") (c-n-x \"y\"))"
	(format nil "~a" (c-e-+ (c-n-x "x") (c-n-x "y")))))

  (is
   (string-equal
	"(c-e-vector)"
	(format nil "~a" (c-e-vector))))

  (is
   (string-equal
	"(c-e-vector (c-n-x \"a\"))"
	(format nil "~a" (c-e-vector (c-n-x "a")))))

  (is
   (string-equal
	"(c-e-vector (c-n-x \"a\") (c-n-x \"b\"))"
	(format nil "~a" (c-e-vector (c-n-x "a") (c-n-x "b")))))

  (is
   (string-equal
	"(c-e-deref (c-n-x \"p\"))"
	(format nil "~a" (c-e-deref (c-n-x "p")))))

  (is
   (string-equal
	"(c-e-bool t)"
	(format nil "~a" (c-e-bool t))))

  (is
   (string-equal
	"(c-e-bool nil)"
	(format nil "~a" (c-e-bool nil))))
  )

(test clcc-ex-construct-format

    (let ((e (c-e-++ (c-n-x "x"))))
    (is (equal "x" (name (expr e))))
    (is (equal "++x" (c-->string e))))

  (let ((e (c-e--- (c-n-x "x"))))
    (is (equal "x" (name (expr e))))
    (is (equal "--x" (c-->string e))))

  (let ((e (c-e-! (c-e-bool nil))))
    (is-false (value (expr e)))
    (is (equal "!( false )" (c-->string e))))
  
  (let ((e (c-e-string "blub")))
    (is (equal "blub" (value e)))
    (is (equal "\"blub\"" (c-->string e))))

  (let ((e (c-e-int 5)))
    (is (equal 5 (value e)))
    (is (equal "5" (c-->string e))))

  (let ((e (c-e-bool t)))
    (is-true (value e))
    (is (equal "true" (c-->string e))))

  (let ((e (c-e-bool nil)))
    (is-false (value e))
    (is (equal "false" (c-->string e))))

  (let ((e (c-e-char #\b)))
    (is (equal #\b (value e)))
    (is (equal "'b'" (c-->string e))))

  (let ((e (c-e-char #\newline)))
    (is (equal #\newline (value e)))
    (is (equal (format nil "'\\n'") (c-->string e))))

  (let ((e (c-e-char #\return)))
    (is (equal #\return (value e)))
    (is (equal (format nil "'\\r'") (c-->string e))))

  (let ((e (c-e-char #\tab)))
    (is (equal #\tab (value e)))
    (is (equal (format nil "'\\t'") (c-->string e))))

  (let ((e (c-e-char #\')))
    (is (equal #\' (value e)))
    (is (equal (format nil "'\\''") (c-->string e))))

  (let ((e (c-e-- (c-e-int 3) (c-e-int 4))))
    (is (equal 3 (value (lhs e))))
    (is (equal 4 (value (rhs e))))
    (is (equal "( 3-4 )" (c-->string e))))
  
  (let ((e (c-e-/ (c-e-int 3) (c-e-int 4))))
    (is (equal 3 (value (lhs e))))
    (is (equal 4 (value (rhs e))))
    (is (equal "( 3/4 )" (c-->string e))))

  (let ((e (c-e-== (c-e-int 3) (c-e-int 4))))
    (is (equal 3 (value (lhs e))))
    (is (equal 4 (value (rhs e))))
    (is (equal "3 == 4" (c-->string e))))

  (let ((e (c-e-!= (c-e-int 3) (c-e-int 4))))
    (is (equal 3 (value (lhs e))))
    (is (equal 4 (value (rhs e))))
    (is (equal "3 != 4" (c-->string e))))

  (let ((e (c-e->= (c-e-int 3) (c-e-int 4))))
    (is (equal 3 (value (lhs e))))
    (is (equal 4 (value (rhs e))))
    (is (equal "3 >= 4" (c-->string e))))

  (let ((e (c-e-< (c-e-int 3) (c-e-int 4))))
    (is (equal 3 (value (lhs e))))
    (is (equal 4 (value (rhs e))))
    (is (equal "3 < 4" (c-->string e))))

  (let ((e (c-e-> (c-e-int 3) (c-e-int 4))))
    (is (equal 3 (value (lhs e))))
    (is (equal 4 (value (rhs e))))
    (is (equal "3 > 4" (c-->string e))))

  (let ((e (c-e-! (c-e-bool t))))
    (is-true (value (expr e)))
    (is (equal "!( true )" (c-->string e))))

  (let ((e (c-e-pair (c-e-int 3) (c-e-int 4))))
    (is (equal 3 (value (lhs e))))
    (is (equal 4 (value (rhs e))))
    (is (equal "{ 3, 4 }" (c-->string e))))

  (let ((e (c-e-pointer (c-e-int 3))))
    (is (equal 3 (value (expr e))))
    (is (equal "&3" (c-->string e))))

  (let ((e (c-e-obj)))
    (is (equal "{}" (c-->string e))))

  (let ((e (c-e-plain-cast (c-n-x "a") (c-n-x "x"))))
    (is (equal "a" (name (param-type e))))
    (is (equal "x" (name (expr e))))
    (is (equal "(a)( x )" (c-->string e))))

  (let ((e (c-e-static-cast (c-n-x "a") (c-n-x "x"))))
    (is (equal "a" (name (param-type e))))
    (is (equal "x" (name (expr e))))
    (is (equal "static_cast<a>( x )" (c-->string e))))

  (let ((e (c-e-dynamic-cast (c-n-x "a") (c-n-x "x"))))
    (is (equal "a" (name (param-type e))))
    (is (equal "x" (name (expr e))))
    (is (equal "dynamic_cast<a>( x )" (c-->string e))))

  (let ((e (c-e-call (c-n-x "f") (c-n-x "x"))))
    (is (equal "f" (name (expr e))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "f( x )" (c-->string e))))
  
  (signals
      (error "c-e-call failed to signal error on bad argument")
    (c-e-call (c-n-x "f") 5))

  (let ((e (c-e-call (c-n-x "f"))))
    (is (equal "f" (name (expr e))))
    (is-false (arg-list e))
    (is (equal "f()" (c-->string e))))

  (let ((e (c-e-call (c-n-x "f") (list (c-n-x "x")))))
    (is (equal "f" (name (expr e))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "f( x )" (c-->string e))))

  (let ((e (c-e-call (c-n-x "f") (list (c-n-x "x") (c-n-x "y")))))
    (is (equal "f" (name (expr e))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "y" (name (cadr (arg-list e)))))
    (is (equal "f( x, y )" (c-->string e))))

  (let ((e (c-e-? (c-n-x "x") (c-n-x "y") (c-n-x "z"))))
    (is (equal "x" (name (cnd e))))
    (is (equal "y" (name (imp e))))
    (is (equal "z" (name (alt e))))
    (is (equal "( x ? y : z )" (c-->string e))))

  (signals
      (error "c-e-optional failed to signal error on bad operand")
    (c-e-optional 5))

  (let ((e (c-e-optional)))
    (is-false (expr e))
    (is (equal "std::experimental::nullopt" (c-->string e))))

  (let ((e (c-e-optional (c-n-x "x"))))
    (is (equal "x" (name (expr e))))
    (is (equal "{ x }" (c-->string e))))

  (let ((e (c-e-+)))
    (is-false (arg-list e))
    (is (equal "0" (c-->string e))))

  (let ((e (c-e-+ (c-n-x "x"))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "x" (c-->string e))))

  (let ((e (c-e-+ (c-n-x "x") (c-n-x "y"))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "y" (name (cadr (arg-list e)))))
    (is (equal "( x+y )" (c-->string e))))

  (signals
      (error "c-e-+ failed to signal error on bad argument")
    (c-e-+ 5 2))

  (let ((e (c-e-*)))
    (is-false (arg-list e))
    (is (equal "1" (c-->string e))))

  (let ((e (c-e-* (c-n-x "x"))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "x" (c-->string e))))

  (let ((e (c-e-* (c-n-x "x") (c-n-x "y"))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "y" (name (cadr (arg-list e)))))
    (is (equal "( x*y )" (c-->string e))))

  (let ((e (c-e-* (c-n-x "x") (c-n-x "y") (c-n-x "z"))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "y" (name (cadr (arg-list e)))))
    (is (equal "z" (name (caddr (arg-list e)))))
    (is (equal "( x*y*z )" (c-->string e))))

  (signals
      (error "c-e-* failed to signal error on bad argument")
    (c-e-* 5 2))

  (let ((e (c-e-&&)))
    (is-false (arg-list e))
    (is (equal "true" (c-->string e))))

  (let ((e (c-e-&& (c-n-x "x"))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "x" (c-->string e))))

  (let ((e (c-e-&& (c-n-x "x") (c-n-x "y"))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "y" (name (cadr (arg-list e)))))
    (is (equal "( x && y )" (c-->string e))))

  (let ((e (c-e-&& (c-n-x "x") (c-n-x "y") (c-n-x "z"))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "y" (name (cadr (arg-list e)))))
    (is (equal "z" (name (caddr (arg-list e)))))
    (is (equal "( x && y && z )" (c-->string e))))

  (signals
      (error "c-e-&& failed to signal error on bad argument")
    (c-e-&& "x" "y"))

  (let ((e (c-e-or)))
    (is-false (arg-list e))
    (is (equal "false" (c-->string e))))

  (let ((e (c-e-or (c-n-x "x"))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "x" (c-->string e))))

  (let ((e (c-e-or (c-n-x "x") (c-n-x "y"))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "y" (name (cadr (arg-list e)))))
    (is (equal "( x || y )" (c-->string e))))

  (let ((e (c-e-or (c-n-x "x") (c-n-x "y") (c-n-x "z"))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "y" (name (cadr (arg-list e)))))
    (is (equal "z" (name (caddr (arg-list e)))))
    (is (equal "( x || y || z )" (c-->string e))))

  (signals
      (error "c-e-or failed to signal error on bad argument")
    (c-e-or "x" "y"))

  (signals
      (error "c-e-vector failed to signal error on bad argument")
    (c-e-vector 5))

  (let ((e (c-e-vector)))
    (is-false (arg-list e))
    (is (equal "{}" (c-->string e))))

  (let ((e (c-e-vector (c-n-x "x"))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "{ x }" (c-->string e))))

  (let ((e (c-e-vector (c-n-x "x") (c-n-x "y"))))
    (is (equal "x" (name (car (arg-list e)))))
    (is (equal "y" (name (cadr (arg-list e)))))
    (is (equal "{ x, y }" (c-->string e))))

  (is
   (string-equal
	"5.5"
	(c-->string (c-e-double 5.5))))
  )
