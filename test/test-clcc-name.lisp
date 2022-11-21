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

(test clcc-name

  (let ((t1 (c-t-vector (c-n-x "blub"))))
    (is (equal "blub" (name (car (arg-list t1)))))
    (is (c-name-eq "std::vector" (name t1)))
    (is (c-name-eq "std::vector<blub>" t1))))

