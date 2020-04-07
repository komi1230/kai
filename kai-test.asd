(defsystem "kai-test"
  :author ""
  :license ""
  :depends-on ("kai"
               "rove")
  :components ((:module "test"
                :components
                        ((:file "main" :depends-on ("rove" "kai")))))
  :description "Test system for kai"
  :perform (test-op (op c)
                    (symbol-call :rove :run c)))
