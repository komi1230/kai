(in-package :cl-user)
(defpackage :kai
  (:use :cl)
  (:export :hoge))
(in-package :kai)



(defun hoge ()
  (format t "This is Kai package !"))
