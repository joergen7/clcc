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
;; Comments and Indentation
;;------------------------------------------------------------

(defgeneric indent (s)
  (:documentation "indent argument string"))

(defmethod indent ((s string))
  (line-pad s "  " :unless-starts-with #\#))

(defgeneric comment (s)
  (:documentation "comment argument string"))

(defmethod comment ((s string))
  (line-pad s "// "))

;;------------------------------------------------------------
;; Dependencies
;;------------------------------------------------------------

(defgeneric c-find-dep (&rest x-list)
  (:documentation "(c-find-dep X ...)

Find dependencies of arguments X .... Returns a flat list of
unique strings. The result list member's order is independent
of the order or structure of the arguments X."))

(defmethod c-find-dep (&rest x-list)
  (let* ((x-list (alexandria:flatten x-list))
	 (y-list (mapcar #'c-find-dep-single x-list)))
    (sort
     (reduce
      #'(lambda (a b) (union a b :test #'string=))
      y-list
      :initial-value '())
     #'string<)))

;;------------------------------------------------------------
;; Obligations
;;------------------------------------------------------------

(defgeneric c-->string (x)
  (:documentation "(c-->string X)

Compile X. Returns a string.")
  (:method (x)
    (error
	 (format
	  nil
	  "c-->string not defined for value of type ~a"
	  (type-of x)))))



(defgeneric c-find-dep-single (x)
  (:documentation "(c-find-dep-single X)

Takes a single argument X which must be an instance of either
c-tp or c-ex and traverses it to find all system dependencies
of X. Returns a string list enumerating the library names the
argument depends on.")
  (:method (x)
	(error
	 (format
	  nil
	  "c-find-dep-single undefined for value of type ~a"
	  (type-of x)))))
