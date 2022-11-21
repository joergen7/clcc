(defsystem :clcc
  :author      "Jörgen Brandt <joergen@cuneiform-lang.org>"
  :version     (:read-file-form "version.sexp")
  :description "C++ language model and compiler in Common Lisp"
  :license     "Apache License, Version 2.0"
  :depends-on  (:alexandria :lang-util)
  :pathname    "src/"
  :components  ((:file "package")
				(:file "clcc-util" :depends-on ("package"))
				(:file "clcc-tp"   :depends-on ("clcc-util"))
				(:file "clcc-ex"   :depends-on ("clcc-util"))
				(:file "clcc-name" :depends-on ("clcc-tp" "clcc-ex"))
				(:file "clcc-st"   :depends-on ("clcc-name"))
				(:file "clcc-tl"   :depends-on ("clcc-st"))
				(:file "clcc"      :depends-on ("clcc-tl")))
  :in-order-to ((test-op (test-op :clcc/test))))

(defsystem :clcc/test
  :author      "Jörgen Brandt <joergen@cuneiform-lang.org>"
  :description "Test suite for clcc"
  :license     "Apache License, Version 2.0"
  :depends-on  (:clcc :fiveam)
  :pathname    "test/"
  :components  ((:file "package")
				(:file "clcc-suite"     :depends-on ("package"))
				(:file "test-clcc-tp"   :depends-on ("clcc-suite"))
				(:file "test-clcc-ex"   :depends-on ("clcc-suite"))
				(:file "test-clcc-name" :depends-on ("clcc-suite"))
				(:file "test-clcc"      :depends-on ("clcc-suite")))
  :perform     (test-op (o c)
						(unless
							(symbol-call :fiveam :run! (find-symbol* :clcc-suite :clcc/test))
						  (error "there were test failures"))))
