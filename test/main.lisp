(defpackage kai-test
  (:use :cl
        :kai
        :rove))
(in-package :kai-test)

;; NOTE: To run this test file, execute `(asdf:test-system :kai)' in your Lisp.

(ql:quickload :rove)

(rove:deftest test-target-kai
  (rove:testing "Initial setup for state"
    (rove:ok (equal kai:*state* '())))

  (testing "Initial setup for style"
    (rove:ok (equal kai:*style "{}"))))
