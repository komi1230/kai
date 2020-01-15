(defsystem "kai"
  :version "0.0.1"
  :author "Yusuke Kominami"
  :license "MIT License"
  :depends-on ("trivial-shell")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "util"))))
  :description "Plotter for Common Lisp")
