(defsystem "kai-test"
  :author ""
  :license ""
  :depends-on ("rove")
  :components ((:module "tests"
                :components
                        ((:file "main"))))
  :description "Test system for kai"
  :perform (test-op (op c)
                    (symbol-call :rove :run c)))
