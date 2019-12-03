;;;; plot.lisp --- plotter as an interface
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file covers plotting interface and preprocess for it.
;;; Some preprocess functions are imported from util.lisp


(in-package :cl-user)
(defpackage #:kai.plot
  (:use :cl))
(in-package #:kai.plot)



;; Draw input data and frame
(defmacro make-figure ()
  `(defmethod glut:display ((w base-window))
     (gl:clear :color-buffer)
     
     ;; Draw frame
     (gl:color ,0 ,0 ,0)
     (gl:with-primitive :line-strip  ; bottom
       (gl:vertex ,0.13 ,0.10 ,0)
       (gl:vertex ,0.93 ,0.10 ,0))
     (gl:with-primitive :line-strip  ; top
       (gl:vertex ,0.13 ,0.90 ,0)
       (gl:vertex ,0.93 ,0.90 ,0))
     (gl:with-primitive :line-strip  ; left
       (gl:vertex ,0.13 ,0.10 ,0)
       (gl:vertex ,0.13 ,0.90 ,0))
     (gl:with-primitive :line-strip  ; right
       (gl:vertex ,0.93 ,0.10 ,0)
       (gl:vertex ,0.93 ,0.90 ,0))

     ;; Draw Scale
     ;; x-axis
     (gl:color ,0 ,0 ,0)
     (gl:with-primitive :line-strip     
       (gl:vertex ,0.155 ,0.10 ,0)
       (gl:vertex ,0.155 ,0.09 ,0))
     (gl:with-primitive :line-strip 
       (gl:vertex ,0.305 ,0.10 ,0)
       (gl:vertex ,0.305 ,0.09 ,0))
     (gl:with-primitive :line-strip 
       (gl:vertex ,0.455 ,0.10 ,0)
       (gl:vertex ,0.455 ,0.09 ,0))
     (gl:with-primitive :line-strip
       (gl:vertex ,0.605 ,0.10 ,0)
       (gl:vertex ,0.605 ,0.09 ,0))
     (gl:with-primitive :line-strip
       (gl:vertex ,0.755 ,0.10 ,0)
       (gl:vertex ,0.755 ,0.09 ,0))
     (gl:with-primitive :line-strip
       (gl:vertex ,0.905 ,0.10 ,0)
       (gl:vertex ,0.905 ,0.09 ,0))
     ;; y-axis
     (gl:with-primitive :line-strip 
       (gl:vertex ,0.12 ,0.12 ,0)
       (gl:vertex ,0.13 ,0.12 ,0))
     (gl:with-primitive :line-strip 
       (gl:vertex ,0.12 ,0.215 ,0)
       (gl:vertex ,0.13 ,0.215 ,0))
     (gl:with-primitive :line-strip 
       (gl:vertex ,0.12 ,0.31 ,0)
       (gl:vertex ,0.13 ,0.31 ,0))
     (gl:with-primitive :line-strip 
       (gl:vertex ,0.12 ,0.405 ,0)
       (gl:vertex ,0.13 ,0.405 ,0))
     (gl:with-primitive :line-strip 
       (gl:vertex ,0.12 ,0.50 ,0)
       (gl:vertex ,0.13 ,0.50 ,0))
     (gl:with-primitive :line-strip 
       (gl:vertex ,0.12 ,0.595 ,0)
       (gl:vertex ,0.13 ,0.595 ,0))
     (gl:with-primitive :line-strip 
       (gl:vertex ,0.12 ,0.69 ,0)
       (gl:vertex ,0.13 ,0.69 ,0))
     (gl:with-primitive :line-strip 
       (gl:vertex ,0.12 ,0.785 ,0)
       (gl:vertex ,0.13 ,0.785 ,0))
     (gl:with-primitive :line-strip 
       (gl:vertex ,0.12 ,0.88 ,0)
       (gl:vertex ,0.13 ,0.88 ,0))
     

     ;; FIXME: Draw input data.
     
     ;; Start processing buffered OpenGL routines.
     (gl:flush)))
