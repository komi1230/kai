(defsystem "kai"
  :version "0.1.0"
  :author "Yusuke Kominami"
  :license "MIT License"
  :depends-on ("cl-opengl"
               "cl-glu"
               "cl-glut")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Plotter for Common Lisp"
  :in-order-to ((test-op (test-op "kai/tests"))))
