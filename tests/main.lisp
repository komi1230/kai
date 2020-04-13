(defpackage kai-test
  (:use :cl
        :rove
        :kai))
(in-package :kai-test)

;; NOTE: To run this test file, execute `(asdf:test-system :kai)' in your Lisp.


(deftest test-target-kai
  (testing "Initial setup for state"
    (ok (equal kai:*state* '())))

  (testing "Initial setup for style"
    (ok (equal kai:*style* "{}"))))
