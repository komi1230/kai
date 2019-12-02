(defsystem "kai"
  :version "0.0.1"
  :author "Yusuke Kominami"
  :license "MIT License"
  :depends-on ("cl-opengl"
               "cl-glu"
               "cl-glut")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "util")
                 (:file "window"))))
  :description "Plotter for Common Lisp")
