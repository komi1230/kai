(in-package :cl-user)

(ql:quickload :cl-opengl)
(ql:quickload :cl-glu)
(ql:quickload :cl-glut)

(defpackage :kai
  (:use :cl)
  (:import-from :kai.window
                :*width*
                :*height*
                :make-base-window
                :setup-base-window)
  (:import-from :kai.plot
                :make-figure)
  (:export :plot
           :hoge))
(in-package :kai)



(defun plot (data &key (type :line) (color :blue) (title ""))
  (make-base-window)
  (setup-base-window)
  (make-figure data type color title)
  (glut:display-window (make-instance 'base-window)))



(defun hoge ()
  (format t "this is kai package."))

;;----------------- For Debug ---------------------
(defun main ()
  (make-base-window *title* *width* *height*)
  (setup-base-window)
  (make-figure)
  (glut:display-window (make-instance 'base-window)))
