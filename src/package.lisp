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

(defpackage :clcc
  (:use :cl)
  (:nicknames :cc)
  (:export

   ;; accessors
   #:alt
   #:arg
   #:arg-list
   #:att-list
   #:body
   #:catch-block-list
   #:cnd
   #:const-p
   #:doc
   #:except-name
   #:expr
   #:expr-list
   #:imp
   #:info
   #:n-init
   #:delta-n
   #:lhs
   #:name
   #:namespace
   #:param-type
   #:protect
   #:ret-type
   #:rhs
   #:system-list
   #:user-list
   #:value

   ;; constructors
   #:c-info
   #:c-name-eq
   #:c-sym
   #:c-tp
   #:c-tp-p
   #:c-argument-ellipses
   #:c-argument-ellipses-p
   #:c-argument
   #:c-argument-p
   #:c-const-ref-argument
   #:c-n-x
   #:c-n-sub
   #:c-n-arg
   #:c-t-const
   #:c-t-const-ref
   #:c-t-const-pointer
   #:c-t-quote
   #:c-t-vector
   #:c-t-pair
   #:c-t-optional
   #:c-t-ref
   #:c-t-pointer
   #:c-t-map
   #:c-t-stringstream
   #:c-t-runtime-error
   #:c-t-exception
   #:c-t-bool
   #:c-t-string
   #:c-t-int
   #:c-t-nat
   #:c-t-time
   #:c-t-time
   #:c-t-struct-tm
   #:c-t-void
   #:c-t-size
   #:c-t-char
   #:c-t-function
   #:c-t-function-p
   #:c-t-unique-ptr
   #:c-t-generic
   #:c-t-member
   #:c-t-double
   #:c-ex
   #:c-ex-p
   #:c-e-lval
   #:c-e-value
   #:c-e-expr
   #:c-e-param-type
   #:c-e-binary
   #:c-e-arg-list
   #:c-e-name
   #:c-e-obj
   #:c-e-++
   #:c-e---
   #:c-e-!
   #:c-e-string
   #:c-e-int
   #:c-e-double
   #:c-e-bool
   #:c-e-true
   #:c-e-false
   #:c-e-char
   #:c-e--
   #:c-e-/
   #:c-e-==
   #:c-e-!=
   #:c-e->=
   #:c-e-<
   #:c-e->
   #:c-e-not
   #:c-e-pair
   #:c-e-pointer
   #:c-e-map
   #:c-e-plain-cast
   #:c-e-dynamic-cast
   #:c-e-static-cast
   #:c-e-call
   #:c-e-?
   #:c-e-optional
   #:c-e-+
   #:c-e-*
   #:c-e-or
   #:c-e-&&
   #:c-e-vector
   #:c-e-.
   #:c-e-->
   #:c-e-subscr
   #:c-e-deref
   #:c-st
   #:c-st-p
   #:c-s-arg
   #:c-s-expr
   #:c-s-name
   #:c-s-binary
   #:c-s-body
   #:c-s-arg-list
   #:c-case
   #:c-case-p
   #:c-s-switch
   #:c-s-cnd
   #:c-s-do
   #:c-s-assign
   #:c-s-+=
   #:c-s-return
   #:c-s-continue
   #:c-s-break
   #:c-s-define
   #:c-s-block
   #:c-s-doc
   #:c-catch-block
   #:c-catch-block-p
   #:c-s-try
   #:c-s-foreach
   #:c-s-if
   #:c-s-for
   #:c-s-<<
   #:c-s->>
   #:c-s-while
   #:c-s-rethrow
   #:c-s-throw
   #:c-toplevel
   #:c-toplevel-p
   #:c-protect-p
   #:c-memb
   #:c-memb-p
   #:c-field
   #:c-field-p
   #:c-abstract-meth
   #:c-meth
   #:c-meth-p
   #:c-structor
   #:c-structor-p
   #:c-constructor
   #:c-constructor-p
   #:c-default-constructor
   #:c-destructor
   #:c-destructor-p
   #:c-template
   #:c-cls
   #:c-cls-p
   #:c-using
   #:c-using-p
   #:c-enum
   #:c-enum-p
   #:c-func
   #:c-static-func
   #:c-static-func-p
   #:c-src
   #:c-->string
   #:c-memb->hh
   #:c-memb->cc
   #:c-toplevel->hh
   #:c-toplevel->cc
   #:c-src->hash
   #:c-find-dep
   #:c-find-dep-single

   ;; others
   #:indent
   #:comment
   #:full-name
   ))

