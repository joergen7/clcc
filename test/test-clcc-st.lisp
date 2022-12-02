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

(test clcc-st-construct-format

  (signals
	  (error "c-s-<< failed to raise condition on bad expression")
	(c-s-<< 5))
  
  (signals
	  (error "c-s-<< failed to raise condition on bad expression")
	(c-s-<< (c-n-x "x") 5))
  
  (is
   (string-equal
	"a >> b;"
	(c-->string (c-s->> (c-n-x "a") (c-n-x "b")))))

  (signals
	  (error "c-s->> failed to raise condition on bad expression")
	(c-s->> 5))

  (signals
	  (error "c-s->> failed to raise condition on bad expression")
	(c-s->> (c-n-x "x") 5))

  (signals
      (error "c-s-while failed to signal error on bad body")
    (c-s-while (c-n-x "x") (c-n-x "y")))

  (let ((s (c-s-while (c-n-x "x") (c-s-do (c-n-x "y")))))
    (is (equal "x" (name (cnd s))))
    (is (equal "y" (name (expr (car (body s))))))
    (is (equal (format nil "while( x )~%  {~%    y;~%  }") (c-->string s))))

  (is
   (string-equal
	"throw;"
	(c-->string (c-s-rethrow)))))


(test clcc-st-print
  (is
   (string-equal
	"(c-s-do (c-n-x \"x\"))"
	(format nil "~a" (c-s-do (c-n-x "x")))))
  
  (is
   (string-equal
	"(c-s-assign (c-n-x \"x\") (c-n-x \"y\"))"
	(format nil "~a" (c-s-assign (c-n-x "x") (c-n-x "y")))))

  (is
   (string-equal
	"(c-s-return)"
	(format nil "~a" (c-s-return))))

  (is
   (string-equal
	"(c-s-return (c-n-x \"x\"))"
	(format nil "~a" (c-s-return (c-n-x "x")))))

  (is
   (string-equal
	"(c-s-define \"x\" (c-n-x \"t\"))"
	(format nil "~a" (c-s-define "x" (c-n-x "t")))))

  (is
   (string-equal
	"(c-s-define \"x\" (c-n-x \"t\") (c-n-x \"y\"))"
	(format nil "~a" (c-s-define "x" (c-n-x "t") (c-n-x "y")))))

  (is
   (string-equal
	"(c-s-define \"x\" (c-n-x \"t\") (c-n-x \"y\") (c-n-x \"z\"))"
	(format nil "~a" (c-s-define "x" (c-n-x "t") (c-n-x "y") (c-n-x "z")))))

  (is
   (string-equal
	"(c-s-block)"
	(format nil "~a" (c-s-block))))

  (is
   (string-equal
	"(c-s-block (c-s-do (c-n-x \"x\")))"
	(format nil "~a" (c-s-block (c-s-do (c-n-x "x"))))))

  (is
   (string-equal
	"(c-s-if (c-e-bool nil) (c-s-do (c-e-bool t)) (c-s-do (c-e-bool nil)))"
	(format nil "~a" (c-s-if (c-e-false) (c-s-do (c-e-true)) (c-s-do (c-e-false))))))

  (is
   (string-equal
	"(c-s-throw (c-n-x \"e\"))"
	(format nil "~a" (c-s-throw (c-n-x "e")))))

  (is
   (string-equal
	"(c-s-throw (c-n-x \"e\") (c-n-x \"x\"))"
	(format nil "~a" (c-s-throw (c-n-x "e") (c-n-x "x")))))

  (is
   (string-equal
	"(c-s-throw (c-n-x \"e\") (c-n-x \"x\") (c-n-x \"y\"))"
	(format nil "~a" (c-s-throw (c-n-x "e") (c-n-x "x") (c-n-x "y")))))
  )




(test clcc-st-c-find-dep-single

  (is
   (string-equal
	"iostream"
	(car (c-find-dep-single (c-s-assign (c-e-cin) (c-n-x "y"))))))

  (is
   (string-equal
	"iostream"
	(car (c-find-dep-single (c-s-assign (c-n-x "x") (c-e-cin))))))

  (is (null (c-find-dep-single (c-s-continue))))
  (is (null (c-find-dep-single (c-s-break))))
  (is (null (c-find-dep-single (c-s-return))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-return (c-e-cin)))))

  (is
   (equal
	'("ctime")
	(c-find-dep-single (c-s-define "x" (c-t-time) (c-n-x "y")))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-define "x" (c-n-x "t") (c-e-cin)))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-block (c-s-do (c-e-cin))))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-doc "blub" (c-s-do (c-e-cin))))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-case (c-e-cin)))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-case (c-e-true) (c-s-do (c-e-cin))))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-switch (c-e-cin)))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-switch (c-e-true) (c-case (c-e-cin))))))

  (is
   (equal
	'("stdexcept")
	(c-find-dep-single (c-catch-block "e" (c-t-runtime-error)))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-catch-block "e" (c-n-x "t") (c-s-do (c-e-cin))))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-try (c-s-block (c-s-do (c-e-cin)))))))

  (is
   (equal
	'("stdexcept")
	(c-find-dep-single (c-s-try (c-s-block) (c-catch-block "e" (c-t-runtime-error))))))

  (is
   (equal
	'("stdexcept")
	(c-find-dep-single (c-s-foreach "i" (c-t-runtime-error) (c-e-int 0)))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-foreach "i" (c-t-int) (c-e-cin)))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-foreach "i" (c-t-int) (c-e-int 0) (c-s-do (c-e-cin))))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-if (c-e-cin) (c-s-block) (c-s-block)))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-if (c-e-true) (c-s-block (c-s-do (c-e-cin))) (c-s-block)))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-if (c-e-true) (c-s-block) (c-s-block (c-s-do (c-e-cin)))))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-for "i" (c-e-cin) (c-s-do (c-e-int 0))))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-for "i" (c-e-int 0) (c-s-do (c-e-cin))))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-<< (c-e-cin)))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-<< (c-e-int 0) (c-e-cin)))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s->> (c-e-cin)))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s->> (c-e-int 0) (c-e-cin)))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-while (c-e-cin)))))

  (is
   (null (c-find-dep-single (c-s-rethrow))))

  (is
   (equal
	'("stdexcept")
	(c-find-dep-single (c-s-throw (c-t-runtime-error)))))

  (is
   (equal
	'("iostream")
	(c-find-dep-single (c-s-throw (c-n-x "e") (c-e-cin)))))
  )

  
  
