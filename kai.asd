;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-

(defsystem :kai
  :version "0.3.1"
  :author "Yusuke Kominami"
  :license :MIT
  :depends-on (#:alexandria
	       "dexador"
               "cl-who"
               "cl-css"
               "jonathan")
  :serial t
  :components ((:module "src"
                :components
                ((:file #:environment)
		 (:file #:browser)
		 (:file "util")
		 (:file #:kai-init)
                 (:file "converter")
                 (:module "plotly"
                  :serial t
                  :depends-on ("converter")
                  :components
                  ((:file "generate")
                   (:file "launch")))
                 (:file "interface")
                 (:file "kai"))))
  :in-order-to ((test-op (test-op :kai-test)))
  :description "Plotter for Common Lisp"
  :long-description #.(with-open-file (stream (merge-pathnames
                                               #p"README.md"
                                               (or *load-pathname* *compile-file-pathname*))
                                              :if-does-not-exist nil
                                              :direction :input)))
