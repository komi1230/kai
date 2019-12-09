(in-package :cl-user)

;(ql:quickload :cl-opengl)
;(ql:quickload :cl-glu)
;(ql:quickload :cl-glut)

(defpackage :kai
  (:use :cl)
  (:import-from :kai.window
                :*width*
                :*height*
                :make-base-window
                :setup-base-window)
  (:import-from :kai.plot
                :plot)
  (:export :plot
           :hoge))
(in-package :kai)



