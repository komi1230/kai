(in-package :cl-user)

;(ql:quickload :cl-opengl)
;(ql:quickload :cl-glu)
;(ql:quickload :cl-glut)

(defpackage :kai
  (:use :cl)
  (:import-from :kai.plot
                :plot)
  (:export :plot))
(in-package :kai)

