(defpackage kai-test
  (:use :cl
        :rove
        :kai))
(in-package :kai-test)

;; NOTE: To run this test file, execute `(asdf:test-system :kai)' in your Lisp.


(deftest test-target-kai
    (kai:reset!)
  (testing "Initial setup"
           (ok (equal kai:*state* '()))
           (ok (equal kai:*style* '())))

  (kai:reset!)
  (testing "Scatter"
           (ok (kai:scatter '(1 2 3)
                             :marker '(:size 12))))

  (kai:reset!)
  (testing "Bar plot"
           (ok (kai:bar '(10 20 30))))

  (kai:reset!)
  (testing "Pie chart"
           (ok (kai:pie '(1 2 3)
                         '("hoge" "foo" "bar"))))

  (kai:reset!)
  (testing "Sunburst chart"
           (ok (kai:sunburst '(1 2 3)
                              '("hoge" "foo" "bar")
                              '("" "hoge" "hoge"))))

  (kai:reset!)
  (testing "Box chart"
           (ok (kai:box '(1 2 3))))

  (kai:reset!)
  (testing "Heatmap chart"
           (ok (kai:heatmap '((1 2) (3 4)))))

  (kai:reset!)
  (testing "Contour chart"
           (ok (kai:contour '((1 2) (3 4)))))

  (kai:reset!)
  (testing "Scatter3D plot"
           (ok (kai:scatter3d '(1 2 3)
                              '(1 2 3)
                              '(1 2 3)))))
