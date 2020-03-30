(defsystem "kai"
  :version "0.1.8"
  :author "Yusuke Kominami"
  :license "MIT License"
  :depends-on ("trivial-open-browser"
               "drakma"
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
  :description "Plotter for Common Lisp")
