(defsystem "kai-example"
  :version "0.1.8"
  :author "Yusuke Kominami"
  :license "MIT License"
  :depends-on ("kai")
  :serial t
  :components ((:module "examples"
                :components
                ((:file "main"))))
  :description "Examples of Kai"
  :long-description #.(with-open-file (stream (merge-pathnames
                                               #p"README.md"
                                               (or *load-pathname* *compile-file-pathname*))
                                              :if-does-not-exist nil
                                              :direction :input)))
