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

(test cc-predicate
  (is-true  (c-ex-p          (c-n-x "x")))
  (is-true  (c-st-p          (c-s-do (c-n-x "x"))))
  (is-true  (c-catch-block-p (c-catch-block "a" (c-n-x "b"))))
  (let ((tl (c-using (c-info "x_t") (c-t-string))))
    (is-true  (c-toplevel-p tl))
    (is-true  (c-using-p tl)))
  (is-true  (c-protect-p :public))
  (is-true  (c-protect-p :protected))
  (is-true  (c-protect-p :private))
  (is-false (c-protect-p 'private))
  (is-false (c-protect-p :bruviat))
  (let ((memb (c-field :private (c-argument "x" (c-t-string)))))
    (is-true (c-memb-p memb))
    (is-true (c-field-p memb)))
  (is-true  (c-meth-p (c-meth :private nil (c-info "f") '() (c-t-void))))
  (is-false (c-meth-p (c-abstract-meth :private nil (c-info "f") '() (c-t-void))))
  (let ((cs (c-constructor '() '())))
    (is-true  (c-constructor-p cs))
    (is-false (c-destructor-p cs)))
  (let ((ds (c-destructor '())))
    (is-false (c-constructor-p ds))
    (is-true  (c-destructor-p ds)))
  (is-true  (c-static-func-p (c-static-func "f" '() (c-t-void))))
  (is-false (c-static-func-p (c-func (c-info "f") '() (c-t-void) '())))
  (is-false (c-static-func-p 5))
  (is-true (c-enum-p (c-enum (c-info "blub"))))
  (is-true (c-case-p (c-case (c-n-x "x"))))
  )


(test cc-construct-access-compile

  (let ((t1 (c-n-x "blub")))
    (is (equal "blub" (name t1)))
    (is (equal "blub" (c-->string t1))))

  (let ((t1 (c-t-const (c-n-x "blub"))))
    (is (equal "blub" (name (param-type t1))))
    (is (equal "blub const" (c-->string t1))))


  (let ((t1 (c-t-pair (c-n-x "a") (c-n-x "b"))))
    (is (c-name-eq "std::pair" (name t1)))
    (is (equal "a" (name (car (arg-list t1)))))
    (is (equal "b" (name (cadr (arg-list t1)))))
    (is (equal "std::pair<a, b>" (c-->string t1))))

  (let ((t1 (c-t-optional (c-n-x "a"))))
    (is (c-name-eq "std::experimental::optional" (name t1)))
    (is (equal "a" (name (car (arg-list t1)))))
    (is (equal "std::experimental::optional<a>" (c-->string t1))))

  (let ((t1 (c-t-ref (c-n-x "a"))))
    (is (equal "a" (name (param-type t1))))
    (is (equal "a&" (c-->string t1))))

  (let ((t1 (c-t-pointer (c-n-x "a"))))
    (is (equal "a" (name (param-type t1))))
    (is (equal "a*" (c-->string t1))))

  (let ((t1 (c-t-map (c-n-x "a") (c-n-x "b"))))
    (is (c-name-eq "std::map" (name t1)))
    (is (equal "a" (name (car (arg-list t1)))))
    (is (equal "b" (name (cadr (arg-list t1)))))
    (is (equal "std::map<a, b>" (c-->string t1))))

  (is (equal "bool"        (c-->string (c-t-bool))))
  (is (equal "std::string" (c-->string (c-t-string))))
  (is (equal "int"         (c-->string (c-t-int))))
  (is (equal "unsigned int"(c-->string (c-t-nat))))
  (is (equal "time_t"      (c-->string (c-t-time))))
  (is (equal "struct tm"   (c-->string (c-t-struct-tm))))
  (is (equal "void"        (c-->string (c-t-void))))
  (is (equal "size_t"      (c-->string (c-t-size))))
  (is (equal "char"        (c-->string (c-t-char))))

  (signals
      (error "c-t-function failed to signal error on bad argument")
    (c-t-function (list (c-t-string)) (c-n-x "r")))

  (let ((t1 (c-t-function '() (c-n-x "r"))))
    (is (equal "r" (name (ret-type t1))))
    (is (equal "std::function<r()>" (c-->string t1))))

  (let ((t1 (c-t-function (list (c-argument "x" (c-n-x "a"))) (c-n-x "r"))))
    (is (equal "x" (name (car (arg-list t1)))))
    (is (equal "a" (name (param-type (car (arg-list t1))))))
    (is (equal "r" (name (ret-type t1))))
    (is (equal "std::function<r( a )>" (c-->string t1))))

  (let ((t1 (c-t-function (list (c-argument "x" (c-n-x "a"))
				(c-argument "y" (c-n-x "b"))) (c-n-x "r"))))
    (is (equal "x" (name (car (arg-list t1)))))
    (is (equal "a" (name (param-type (car (arg-list t1))))))
    (is (equal "y" (name (cadr (arg-list t1)))))
    (is (equal "b" (name (param-type (cadr (arg-list t1))))))
    (is (equal "r" (name (ret-type t1))))
    (is (equal "std::function<r( a, b )>" (c-->string t1))))

  (let ((t1 (c-t-unique-ptr (c-n-x "a"))))
    (is (c-name-eq "std::unique_ptr" (name t1)))
    (is (equal "a" (name (car (arg-list t1)))))
    (is (equal "std::unique_ptr<a>" (c-->string t1))))

  (let ((e (c-n-x "x")))
    (is (equal "x" (name e)))
    (is (equal "x" (c-->string e)))
    (is (equal "(c-n-x \"x\")" (format nil "~a" e))))

  (let ((e (c-e-. (c-n-x "x") (c-n-x "y"))))
    (is (c-name-eq "x" (lhs e)))
    (is (c-name-eq "y" (rhs e)))
    (is (equal "x.y" (c-->string e))))

  (let ((a (c-e-call (c-n-sub (c-n-x "std") (c-n-x "to_string")) (c-e-call (c-e-. (c-n-x "ds") (c-n-x "get_x")))))
		(b (c-n-x "c_str")))
	(is (equal "std::to_string( ds.get_x() ).c_str"
			   (c-->string (c-e-. a b)))))
  
  (let ((e (c-e--> (c-n-x "x") (c-n-x "y"))))
    (is (c-name-eq "x" (lhs e)))
    (is (c-name-eq "y" (rhs e)))
    (is (equal "x->y" (c-->string e))))
  
  (let ((e (c-e-subscr (c-n-x "x") (c-n-x "y"))))
    (is (equal "x" (name (lhs e))))
    (is (equal "y" (name (rhs e))))
    (is (equal "x[y]" (c-->string e))))
  
  (let ((e (c-e-deref (c-n-x "x"))))
    (is (equal "x" (name (expr e))))
    (is (equal "*x" (c-->string e))))
  
  (let ((s (c-s-do (c-n-x "x"))))
    (is (equal "x" (name (expr s))))
    (is (equal "x;" (c-->string s))))

  (let ((s (c-s-assign (c-n-x "x") (c-n-x "y"))))
    (is (equal "x" (name (lhs s))))
    (is (equal "y" (name (rhs s))))
    (is (equal "x = y;" (c-->string s))))

  (let ((s (c-s-+= (c-n-x "x") (c-n-x "y"))))
    (is (equal "x" (name (lhs s))))
    (is (equal "y" (name (rhs s))))
    (is (equal "x += y;" (c-->string s))))

  (let ((s (c-s-return (c-n-x "x"))))
    (is (equal "x" (name (expr s))))
    (is (equal "return x;" (c-->string s))))

  (signals
      (error "c-case failed to signal error on bad expression")
    (c-case 5))
  
  (signals
      (error "c-case failed to signal error on bad body")
    (c-case (c-e-int 5) (c-e-int 5)))

  (let ((c (c-case (c-n-x "x"))))
    (is (equal (format nil "case x:~%") (c-->string c))))

  (let ((c (c-case (c-n-x "x") (c-s-break))))
    (is (equal (format nil "case x:~%  break;") (c-->string c))))

  (let ((c (c-case (c-n-x "x") (c-s-do (c-e-call (c-n-x "f"))) (c-s-break))))
    (is (equal (format nil "case x:~%  f();~%  break;") (c-->string c))))

  (let ((c (c-case nil (c-s-break))))
    (is (equal (format nil "default:~%  break;") (c-->string c))))

  (let ((s (c-s-switch (c-n-x "a"))))
    (is (equal (format nil "switch( a )~%  {}") (c-->string s))))
  
  (let ((s (c-s-switch
	    (c-n-x "a")
	    (c-case (c-n-x "x") (c-s-break)))))
    (is (equal (format nil "switch( a )~%  {~%  case x:~%    break;~%  }") (c-->string s))))

  (let ((s (c-s-switch
	    (c-n-x "a")
	    (c-case (c-e-bool t) (c-s-break))
	    (c-case (c-e-bool nil) (c-s-break))
	    )))
    (is
     (equal
      (format nil "switch( a )~%  {~%  case true:~%    break;~%  case false:~%    break;~%  }")
      (c-->string s))))

  (signals
      (error "c-s-switch failed to signal error on bad body")
    (c-s-switch (c-n-x "a") 5))



  (is (equal "return;" (c-->string (c-s-return))))
  (signals
      (error "c-s-return failed to signal error on bad operand")
    (c-s-return 5))

  (is (equal "continue;" (c-->string (c-s-continue))))
  (is (equal "break;"    (c-->string (c-s-break))))
  
  (signals
      (error "c-s-define failed to signal error on bad argument")
    (c-s-define "x" (c-t-string) 5))

  (let ((s (c-s-define "x" (c-t-string))))
    (is (equal "x" (name s)))
    (is-false (expr-list s))
    (is (equal "std::string x {};" (c-->string s))))

  (let ((s (c-s-define "x" (c-t-const (c-t-string)))))
    (is (equal "x" (name s)))
    (is-false (expr-list s))
    (is (equal "std::string const x {};" (c-->string s))))

  (let ((s (c-s-define "x" (c-t-string) (c-e-string "blub"))))
    (is (equal "x" (name s)))
    (is (equal "blub" (value (car (expr-list s)))))
    (is (equal "std::string x { \"blub\" };" (c-->string s))))

  (let ((s (c-s-block)))
    (is-false (body s))
    (is (equal "{}" (c-->string s))))

  (let ((s (c-s-block (c-s-do (c-n-x "x")))))
    (is (equal "x" (name (expr (car (body s))))))
    (is (equal (format nil "{~%  x;~%}") (c-->string s))))

  (let ((s (c-s-block (c-s-do (c-n-x "x")) (c-s-do (c-n-x "y")))))
    (is (equal "x" (name (expr (car (body s))))))
    (is (equal "y" (name (expr (cadr (body s))))))
    (is (equal (format nil "{~%  x;~%  y;~%}") (c-->string s))))

  (signals
      (error "c-s-block failed to signal error on bad body")
    (c-s-block (c-n-x "x")))

  (let ((s (c-s-doc "cOmMeNt")))
    (is-false (body s))
    (is (equal (format nil "~%// cOmMeNt~%") (c-->string s))))

  (let ((s (c-s-doc "cOmMeNt"(c-s-do (c-n-x "x")))))
    (is (equal "x" (name (expr (car (body s))))))
    (is (equal (format nil "~%// cOmMeNt~%x;") (c-->string s))))

  (let ((s (c-s-doc "cOmMeNt" (c-s-do (c-n-x "x")) (c-s-do (c-n-x "y")))))
    (is (equal "x" (name (expr (car (body s))))))
    (is (equal "y" (name (expr (cadr (body s))))))
    (is (equal (format nil "~%// cOmMeNt~%x;~%y;") (c-->string s))))

  (signals
      (error "c-s-doc failed to signal error on bad body")
    (c-s-doc "comment" (c-n-x "x")))

  (let ((s (c-s-try (c-s-block))))
    (is-false (body (body s)))
    (is-false (catch-block-list s))
    (is (equal (format nil "try~%  {}") (c-->string s))))

  (let ((s (c-s-try (c-s-block (c-s-do (c-n-x "x"))))))
    (is (equal "x" (name (expr (car (body (body s)))))))
    (is-false (catch-block-list s))
    (is (equal (format nil "try~%  {~%    x;~%  }") (c-->string s))))

  (let ((s (c-s-try (c-s-block) (c-catch-block "a" (c-n-x "b")))))
    (is (equal "a" (name (car (catch-block-list s)))))
    (is (c-name-eq "b" (except-name (car (catch-block-list s)))))
    (is (equal (format nil "try~%  {}~%catch( b const& a )~%  {}") (c-->string s))))

  (let ((s (c-s-try (c-s-block) (c-catch-block "a" (c-n-x "b") (c-s-do (c-n-x "x"))))))
    (is (equal "a" (name (car (catch-block-list s)))))
    (is (c-name-eq "b" (except-name (car (catch-block-list s)))))
    (is (equal (format nil "try~%  {}~%catch( b const& a )~%  {~%    x;~%  }") (c-->string s))))

  (let ((s (c-s-try (c-s-block) (c-catch-block "a" (c-n-x "b")) (c-catch-block "c" (c-n-x "d")))))
    (is (equal "a" (name (car (catch-block-list s)))))
    (is (c-name-eq "b" (except-name (car (catch-block-list s)))))
    (is (equal (format nil "try~%  {}~%catch( b const& a )~%  {}~%catch( d const& c )~%  {}") (c-->string s))))


  (signals
      (error "c-catch-block failed to signal error on bad body")
    (c-catch-block "a" (c-n-x "b") (c-n-x "x")))

  (signals
      (error "c-s-try failed to signal error on bad body")
    (c-s-try 5))
  
  (signals
      (error "c-s-try failed to signal error on bad catch block")
    (c-s-try (c-s-block) 5))

  (let ((s (c-s-foreach "x" (c-t-string) (c-n-x "xs"))))
    (is (equal "x" (name s)))
    (is (equal "xs" (name (expr s))))
    (is-false (body s))
    (is (equal (format nil "for( std::string x : xs )~%  {}") (c-->string s))))

  (let ((s (c-s-foreach "x" (c-t-string) (c-n-x "xs") (c-s-do (c-n-x "y")))))
    (is (equal "x" (name s)))
    (is (equal "xs" (name (expr s))))
    (is (equal "y" (name (expr (car (body s))))))
    (is (equal (format nil "for( std::string x : xs )~%  {~%    y;~%  }") (c-->string s))))

  (signals
      (error "c-s-foreach failed to signal error on bad body")
    (c-s-foreach "x" (c-t-string) (c-n-x "xs") (c-n-x "x")))

  (let ((s (c-s-if (c-n-x "x") (c-s-do (c-n-x "y")) (c-s-do (c-n-x "z")))))
    (is (equal "x" (name (cnd s))))
    (is (equal "y" (name (expr (imp s)))))
    (is (equal "z" (name (expr (alt s)))))
    (is (equal (format nil "if( x )~%  y;~%else~%  z;") (c-->string s))))

  (signals
      (error "c-s-if c-s-if failed to signal error on bad else-branch (alternative)")
    (c-s-if (c-n-x "x") (c-s-do (c-n-x "y")) (c-n-x "z")))


  (signals
      (error "c-s-for failed to signal error on bad body")
    (c-s-for "i" (c-e-int 10) (c-t-string)))

  (signals
      (error "c-s-for failed to signal error on nil n-init")
    (c-s-for "i" (c-e-bool t) (c-s-block) :n-init nil))

  (signals
      (error "c-s-for failed to signal error on string n-init")
    (c-s-for "i" (c-e-bool t) (c-s-block) :n-init "0"))

  (signals
      (error "c-s-for failed to signal error on string delta-n")
    (c-s-for "i" (c-e-bool t) (c-s-block) :delta-n "1"))

  (signals
      (error "c-s-for failed to signal error on zero delta-n")
    (c-s-for "i" (c-e-bool t) (c-s-block) :delta-n 0))

  (signals
      (error "c-s-for failed to signal error on nil delta-n")
    (c-s-for "i" (c-e-bool t) (c-s-block) :delta-n nil))

  (let ((f (c-s-for "i" (c-e-< (c-n-x "i") (c-n-x "len")) (c-s-block) :n-init 0 :delta-n 1)))
    (is
     (equal
      (format nil "for( size_t i { 0 }; i < len; ++i )~%  {}")
      (c-->string f))))

  (let ((f (c-s-for "i" (c-e-< (c-n-x "i") (c-n-x "len")) (c-s-block) :n-init 3 :delta-n -1)))
    (is
     (equal
      (format nil "for( size_t i { 3 }; i < len; --i )~%  {}")
      (c-->string f))))

  (let ((f (c-s-for "i" (c-e-< (c-n-x "i") (c-n-x "len")) (c-s-block) :n-init 3)))
    (is
     (equal
      (format nil "for( size_t i { 3 }; i < len; ++i )~%  {}")
      (c-->string f))))
  
  (let ((f (c-s-for "i" (c-e-< (c-n-x "i") (c-n-x "len")) (c-s-block) :delta-n 2)))
    (is
     (equal
      (format nil "for( size_t i { 0 }; i < len; i += 2 )~%  {}")
      (c-->string f))))
  
  (let ((f (c-s-for "i" (c-e-< (c-n-x "i") (c-n-x "len")) (c-s-block) :delta-n -1)))
    (is
     (equal
      (format nil "for( size_t i { 0 }; i < len; --i )~%  {}")
      (c-->string f))))
  
  (let ((f (c-s-for "i" (c-e-< (c-n-x "i") (c-n-x "len")) (c-s-block) :delta-n -2)))
    (is
     (equal
      (format nil "for( size_t i { 0 }; i < len; i -= 2 )~%  {}")
      (c-->string f))))
  
  

  (let ((s (c-s-for "i" (c-e-< (c-n-x "i") (c-e-int 10)) (c-s-block))))
    (is (equal "i" (name s)))
    (is (equal 0 (n-init s)))
    (is (equal 1 (delta-n s)))
    (is-false (body (body s)))
    (is (equal (format nil "for( size_t i { 0 }; i < 10; ++i )~%  {}") (c-->string s))))

  (let ((s (c-s-for "i" (c-e-< (c-n-x "i") (c-e-int 10)) (c-s-block (c-s-do (c-n-x "x"))))))
    (is (equal "i" (name s)))
    (is (equal 0 (n-init s)))
    (is (equal 1 (delta-n s)))
    (is (equal "x" (name (expr (car (body (body s)))))))
    (is (equal (format nil "for( size_t i { 0 }; i < 10; ++i )~%  {~%    x;~%  }") (c-->string s))))

  (signals
      (error "c-s-<< failed to signal error on bad argument")
    (c-s-<< (c-n-x "x") 5))

  (let ((s (c-s-<< (c-n-x "x"))))
    (is (equal "x" (name (expr s))))
    (is-false (arg-list s))
    (is (equal "x;" (c-->string s))))

  (let ((s (c-s-<< (c-n-x "x") (c-n-x "y"))))
    (is (equal "x" (name (expr s))))
    (is (equal "y" (name (car (arg-list s)))))
    (is (equal "x << y;" (c-->string s))))

  (signals
      (error "c-s-while failed to signal error on bad body")
    (c-s-while (c-n-x "x") (c-n-x "y")))

  (let ((s (c-s-while (c-n-x "x") (c-s-block (c-s-do (c-n-x "y"))))))
    (is (equal "x" (name (cnd s))))
    (is (equal "y" (name (expr (car (body (body s)))))))
    (is (equal (format nil "while( x )~%  {~%    y;~%  }") (c-->string s))))

  (signals
      (error "c-s-throw failed to signal error on bad argument")
    (c-s-throw (c-n-x "except") 5))

  (let ((s (c-s-throw (c-n-x "except"))))
    (is (c-name-eq "except" (name s)))
    (is-false (arg-list s))
    (is (equal "throw except {};" (c-->string s))))

  (let ((s (c-s-throw (c-n-x "except") (c-n-x "x"))))
    (is (c-name-eq "except" (name s)))
    (is (equal "x" (name (car (arg-list s)))))
    (is (equal "throw except { x };" (c-->string s))))

  (let ((s (c-s-throw (c-n-x "except") (c-n-x "x") (c-n-x "y"))))
    (is (c-name-eq "except" (name s)))
    (is (equal "x" (name (car (arg-list s)))))
    (is (equal "y" (name (cadr (arg-list s)))))
    (is (equal "throw except { x, y };" (c-->string s))))


  (signals
      (error "c-field failed to signal error on bad protection")
    (c-field :blub (c-argument "x" (c-t-string))))
  
  (signals
      (error "c-field failed to signal error on bad init argument")
    (c-field :public (c-argument "x" (c-t-string)) 5))

  (let ((field (c-field :public (c-argument "x" (c-t-string)))))
    (is (eq :public (protect field)))
    (is (equal "x" (name (arg field))))
    (is-false (expr-list field))
    (is (equal "std::string x;" (c-memb->hh field "c"))))

  (let ((field (c-field :protected (c-argument "x" (c-t-string)))))
    (is (eq :protected (protect field)))
    (is (equal "x" (name (arg field))))
    (is-false (expr-list field))
    (is (equal "std::string x;" (c-memb->hh field "c"))))

  (let ((field (c-field :private (c-argument "x" (c-t-string)))))
    (is (eq :private (protect field)))
    (is (equal "x" (name (arg field))))
    (is-false (expr-list field))
    (is (equal "std::string x;" (c-memb->hh field "c"))))

  (let ((field (c-field :private (c-argument "x" (c-t-string)) (c-n-x "y"))))
    (is (eq :private (protect field)))
    (is (equal "x" (name (arg field))))
    (is (equal "y" (name (car (expr-list field)))))
    (is (equal "std::string x;" (c-memb->hh field "c"))))

  (let ((m (c-abstract-meth :public nil (c-info "f") '() (c-t-void))))
    (is (eq :public (protect m)))
    (is (equal "f" (name (info m))))
    (is-false (arg-list m))
    (is-true (ret-type m))
    (is (equal "virtual void f( void ) = 0;" (c-memb->hh m "c"))))

  (signals
      (error "c-abstract-meth failed to signal error on bad protection")
    (c-abstract-meth :pubic nil (c-info "f") '() (c-t-void)))

  (signals
      (error "c-abstract-meth failed to signal error on bad argument")
    (c-abstract-meth :plubic nil (c-info "f") '(5) (c-t-void)))

  (signals
      (error "c-meth failed to signal error on bad protection")
    (c-meth :blub nil (c-info "f") '() (c-t-void)))
  
  (signals
      (error "c-meth failed to signal error on bad argument")
    (c-meth :public nil (c-info "f") '(5) (c-t-void)))
  
  (signals
      (error "c-meth failed to signal error on bad body")
    (c-meth :public nil (c-info "f") '() (c-t-void) (c-n-x "x")))
  
  (let ((m (c-meth :public nil (c-info "f") '() (c-t-void))))
    (is (eq :public (protect m)))
    (is (equal "f" (name (info m))))
    (is-true (ret-type m))
    (is-false (body m))
    (is (equal "void f( void );" (c-memb->hh m "c")))
    (is (equal (format nil "void c::f( void )~%{}") (c-memb->cc m "c" '() (c-n-x "c") '()))))

  (signals
      (error "c-constructor failed to sginal error on bad argument")
    (c-constructor (list (c-n-x "x")) '()))
  
  (signals
      (error "c-constructor failed to signal error on bad set-list entry")
    (c-constructor '() (list (c-n-x "x"))))

  (signals
      (error "c-constructor failed to signal error on bad body")
    (c-constructor '() '() (c-n-x "x")))
  
  (let ((c (c-constructor '() '())))
    (is-false (arg-list c))
    (is-false (body c))
    (is (equal "c( void );" (c-memb->hh c "c")))
    (is (equal (format nil "c::c( void )~%{}") (c-memb->cc c "c" '() (c-n-x "c") '()))))

  (let ((c (c-constructor '() '() (c-s-do (c-n-x "x")))))
    (is-false (arg-list c))
    (is (equal "x" (name (expr (car (body c))))))
    (is (equal "c( void );" (c-memb->hh c "c")))
    (is (equal (format nil "c::c( void )~%{~%  x;~%}") (c-memb->cc c "c" '() (c-n-x "c") '()))))

  (let* ((c
	   (c-default-constructor))
	 (cls-body
	   (list
	    (c-field :private (c-argument "x" (c-t-nat)) (c-e-int 5))
	    (c-abstract-meth :private nil (c-info "f") '() (c-t-void)))))
    (is
     (equal
      (format nil "c::c( void ) :~%  x ( 5 )~%{}")
      (c-memb->cc c "c" cls-body (c-n-x "c") '()))))

  (let* ((c
	   (c-constructor '() '()))
	 (cls-body
	   (list
	    (c-field :private (c-argument "x" (c-t-nat)) (c-e-int 5))
	    (c-abstract-meth :private nil (c-info "f") '() (c-t-void)))))
    (is
     (equal
      (format nil "c::c( void )~%{}")
      (c-memb->cc c "c" cls-body (c-n-x "c") '()))))

  (let* ((a   (c-field :public (c-argument "a" (c-t-nat))))
	 (b   (c-field :protected (c-argument "b" (c-t-nat))))
	 (c   (c-field :private (c-argument "c" (c-t-nat))))
	 (cls (c-cls (c-info "cls") '() a b c)))
    (is
     (equal
      (format nil "class cls~%{~%public:~%  unsigned int a;~%~%protected:~%  unsigned int b;~%~%private:~%  unsigned int c;~%};")
      (c-toplevel->hh cls "my_module")
      ))
    (is
     (equal
      ""
      (c-toplevel->cc cls "my_module" nil '())))
    )
  

  (let ((d (c-destructor '())))
    (is-false (body d))
    (is (equal "~c( void );" (c-memb->hh d "c")))
    (is (equal (format nil "c::~~c( void )~%{}") (c-memb->cc d "c" '() (c-n-x "c") '()))))

  (let ((d (c-destructor (list (c-s-do (c-n-x "x"))))))
    (is (equal "x" (name (expr (car (body d))))))
    (is (equal "~c( void );" (c-memb->hh d "c")))
    (is (equal (format nil "c::~~c( void )~%{~%  x;~%}") (c-memb->cc d "c" '() (c-n-x "c") '()))))

  (signals
      (error "c-destructor failed to signal error on bad body")
    (c-destructor (c-n-x "x")))

  (signals
      (error "c-cls failed to signal error on bad body")
    (c-cls (c-info "c") '() (c-t-string)))
  
  (let ((c (c-cls (c-info "c") '())))
    (is (equal "c" (name (info c))))
    (is-false (body c))
    (is (equal (format nil "class c~%{};") (c-toplevel->hh c "my_module")))
    (is (equal (format nil "") (c-toplevel->cc c "my_module" nil '()))))
  

  (let* ((constr
	   (c-constructor (list (c-argument "x" (c-t-string))) '() (c-s-do (c-n-x "y"))))
	 (c
	   (c-cls (c-info "c") '() constr)))
    (is (equal "c" (name (info c))))
    (is-true (body c))
    (is (equal (format nil "class c~%{~%public:~%  c( std::string x );~%};") (c-toplevel->hh c "my_module")))
    (is (equal (format nil "c::c( std::string x )~%{~%  y;~%}") (c-toplevel->cc c "my_module" nil '()))))

  (let ((c (c-cls (c-info "c") '() (c-destructor '()))))
    (is (equal "c" (name (info c))))
    (is-true (body c))
    (is (equal (format nil "class c~%{~%public:~%  ~~c( void );~%};") (c-toplevel->hh c "my_module")))
    (is (equal (format nil "c::~~c( void )~%{}") (c-toplevel->cc c "my_module" nil '()))))

  (let ((c (c-cls (c-info "c") '() (c-field :public (c-argument "x" (c-t-string))))))
    (is (equal "c" (name (info c))))
    (is-true (body c))
    (is
     (equal
      (format nil "class c~%{~%public:~%  std::string x;~%};")
      (c-toplevel->hh c "my_module")))
    (is
     (equal
      ""
      (c-toplevel->cc c "my_module" nil '()))))

  (let* ((m (c-meth :public nil (c-info "f1") '() (c-t-void)))
	 (c (c-cls (c-info "c") '() m)))
    (is
     (equal
      (format nil "class c~%{~%public:~%  void f1( void );~%};")
      (c-toplevel->hh c "my_module")))
    (is
     (equal
      (format nil "void c::f1( void )~%{}")
      (c-toplevel->cc c "my_module" nil '()))))

  (let ((u (c-using (c-info "x_t") (c-t-string))))
    (is (equal "x_t" (name (info u))))
    (is-true (param-type u))
    (is (equal "using x_t = std::string;" (c-toplevel->hh u "my_module"))))

  (let ((en (c-enum (c-info "en") "en_a" "en_b")))
    (is (equal "en" (name (info en))))
    (is (equal '("en_a" "en_b") (arg-list en)))
    (is (stringp (c-toplevel->hh en "my_module")))
    (is (stringp (c-toplevel->cc en "my_module" nil '()))))
  
  (signals
      (error "c-enum failed to signal error on bad argument")
    (c-enum (c-info "en") 4 5))

  (let ((f (c-static-func "f" '() (c-t-string))))
    (is (equal "f" (name f)))
    (is-false (arg-list f))
    (is-true (ret-type f))
    (is-false (body f))
    (is (equal "static std::string f( void );" (c-toplevel->hh f "my_module")))
    (is (equal (format nil "static std::string f( void )~%{}") (c-toplevel->cc f "my_module" nil '()))))

  (signals
      (error "c-static-func failed to signal error on bad body")
    (c-static-func "f" '() (c-t-void) 5))

  (let ((f (c-func (c-info "f") '() (c-t-string))))
    (is (equal "f" (name (info f))))
    (is-false (arg-list f))
    (is-true (ret-type f))
    (is-false (body f))
    (is (equal "std::string f( void );" (c-toplevel->hh f "my_module")))
    (is (equal (format nil "std::string f( void )~%{}") (c-toplevel->cc f "my_module" nil '()))))

  (signals
      (error "c-func failed to signal error on bad body")
    (c-func (c-info "f") '() (c-t-void) 5))

  (let ((cls (c-cls (c-info "c0") '())))
    (let ((tmp (c-template (list "T") cls)))
      (is
       (equal
	(format nil "template<typename T>~%class c0~%{};")
	(c-toplevel->hh tmp "my_module")))))

  (signals
      (error "c-src failed to signal error on bad system-list entry")
    (c-src "blub" '(5) '() "namespace"))
  
  (signals
      (error "c-src failed to signal error on bad user-list entry")
    (c-src "blub" '() '(5) "namespace"))
  
  (signals
      (error "c-src failed to signal error on bad body")
    (c-src "blub" '() '() "namespace" 5))

  (let* ((src-body (list (c-func (c-info "f") '() (c-t-void))
			 (c-static-func "g" '() (c-t-void))))
	 (src      (c-src "module_name" '("iostream") '("other_module_name") "my_favorite_namespace" src-body)))
    (is (equal "module_name" (name src)))
    (is (equal '("iostream") (system-list src)))
    (is (equal '("other_module_name") (user-list src)))
    (is (equal "my_favorite_namespace" (namespace src)))
    (is (equal 2 (length (body src))))
    (finishes (c-src->hash src))
    )

  (signals
      (error "c-->string failed to signal error on bad operand")
    (c-->string 5))

  (let* ((c1
	   (c-cls
	    (c-info "c1")
	    '()))
	 (c2
	   (c-cls
	    (c-info "c2")
	    '()
	    c1)))
    (is
     (equal
      (format nil "class c2~%{~%public:~%  class c1~%  {};~%};")
      (c-toplevel->hh c2 "my_module")))
    (is
     (equal
      ""
      (c-toplevel->cc c2 "my_module" nil '()))))

  (let* ((f1
	   (c-field
	     :private
	     (c-const-ref-argument
	      "s"
	      (c-t-string))))
	 (c1
	   (c-cls
	    (c-info "c1")
	    '()
	    f1))
	 (c2
	   (c-cls
	    (c-info "c2")
	    '()
	    c1)))

    (is
     (string-equal
      (format nil "class c1~%{~%private:~%  std::string const& s;~%};")
      (c-toplevel->hh c1 "my_module")))
    (is
     (string-equal
      ""
      (c-toplevel->cc c1 "my_module" nil '())))
    (is
     (string-equal
      (format nil "class c2~%{~%public:~%  class c1~%  {~%  private:~%    std::string const& s;~%  };~%};")
      (c-toplevel->hh c2 "my_module")))
    (is
     (string-equal
      ""
      (c-toplevel->cc c2 "my_module" nil '())))
    )

  (let ((e (c-e-!
	    (c-e-==
	     (c-e-deref
	      (c-n-x "this"))
	     (c-n-x "b")))))
    (is
     (string-equal
      "!( *this == b )"
      (c-->string e))))

  (let ((e (c-e-! (c-n-x "x"))))
    (is
     (string-equal
      "!( x )"
      (c-->string e))))
	  
  )

(test cc-c-find-dep-single

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
  )

(test cc-c-find-dep

  (is (equal '("ctime" "string") (c-find-dep (c-t-string) (c-t-time)))))



    
