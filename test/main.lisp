(defpackage kai/tests/main
  (:use :cl
        :kai
        :rove))
(in-package :kai/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :kai)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
