(defpackage kai-test
  (:use :cl
        :rove
        :kai))
(in-package :kai-test)

;; NOTE: To run this test file, execute `(asdf:test-system :kai)' in your Lisp.


(deftest test-target-kai
  (testing "Initial setup"
           (ok (equal kai:*state* '()))
           (ok (equal kai:*style* '())))

  (testing "Line"
           (ok (kai:line '(1 2 3) :color :red)))

  (testing "Marker"
           (ok (kai:marker '(1 2 3) :color :red)))

  (testing "Bar plot"
           (ok (kai:bar '(10 20 30))))

  (testing "Pie chart"
           (ok (kai:pie '(1 2 3)
                         '("hoge" "foo" "bar"))))

  (testing "Box chart"
           (ok (kai:box '(1 2 3))))

  (testing "Heatmap chart"
           (ok (kai:heatmap '((1 2) (3 4)))))

  (testing "Contour chart"
           (ok (kai:contour '((1 2) (3 4)))))

  (testing "Line3D plot"
           (ok (kai:line3d '(1 2 3)
                              '(1 2 3)
                              '(1 2 3))))

  (testing "Marker3D plot"
           (ok (kai:marker3d '(1 2 3)
                             '(1 2 3)
                             '(1 2 3))))

  (testing "Surface plot"
           (ok (kai:surface '((1 2) (3 4))))))
