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
;; Toplevel
;;------------------------------------------------------------

(defclass c-toplevel () ())

(defmethod c-toplevel-p (x)
  (typep x 'c-toplevel))

;;------------------------------------------------------------
;; Templatable
;;------------------------------------------------------------

(defclass c-templatable (c-toplevel) ())

(defmethod c-templatable-p (x)
  (typep x 'c-templatable))
