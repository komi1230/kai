(defsystem "kai"
  :version "0.0.1"
  :author "Yusuke Kominami"
  :license "MIT License"
  :depends-on ("cl-opengl"
               "cl-glu"
               "cl-glut")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("plot"))
                 (:file "plot" :depends-on ("util" "window"))
                 (:file "window" :depends-on ("util"))
                 (:file "util"))))
  :description "Plotter for Common Lisp")
