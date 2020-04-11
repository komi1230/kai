(defsystem "kai"
  :version "0.1.8"
  :author "Yusuke Kominami"
  :license "MIT License"
  :depends-on ("trivial-open-browser"
               "dexador"
               "cl-who"
               "cl-css"
               "jonathan")
  :serial t
  :components ((:module "src"
                :components
                ((:file "util")
                 (:file "converter")
                 (:module "plotly"
                  :serial t
                  :depends-on ("converter")
                  :components
                  ((:file "generate")
                   (:file "launch")))
                 (:file "interface")
                 (:file "kai"))))
  :in-order-to ((test-op (test-op kai-test)))
  :description "Plotter for Common Lisp"
  :long-description #.(with-open-file (stream (merge-pathnames
                                               #p"README.md"
                                               (or *load-pathname* *compile-file-pathname*))
                                              :if-does-not-exist nil
                                              :direction :input)))
