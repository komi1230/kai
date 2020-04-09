(defpackage kai-test
  (:use :cl
        :kai))
(in-package :kai-test)

;; NOTE: To run this test file, execute `(asdf:test-system :kai)' in your Lisp.


(rove:deftest test-target-kai
  (rove:testing "Initial setup for state"
    (rove:ok (equal kai:*state* '())))

  (rove:testing "Initial setup for style"
    (rove:ok (equal kai:*style "{}"))))
