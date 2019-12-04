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




;; Draw title of the figure
(defun draw-title (str)
  (let ((x-pos (- 0.53         ; centering string
                  (* 0.018     ; 1 character = 0.018 points
                     (/ (length str)
                        2))))
        (y-pos 0.93))
    (gl:raster-pos x-pos y-pos)
    (gl:color 0 0 0)
    (loop for i from 0 below (length str) do
         (glut:bitmap-character glut:+bitmap-helvetica-18+
                                (char-code (aref str i))))))


;; Draw frame of the figure: outline
(defun draw-frame-2d (x0 y0 x1 y1)
  (gl:color 0 0 0)
  (gl:with-primitive :line-strip  ; bottom
     (gl:vertex x0 y0 0)
     (gl:vertex x1 y0 0))
  (gl:with-primitive :line-strip  ; left
   (gl:vertex x0 y0 0)
   (gl:vertex x0 y1 0))
  (gl:with-primitive :line-strip  ; right
     (gl:vertex x1 y0 0)
     (gl:vertex x1 y1 0))
  (gl:with-primitive :line-strip  ; top
     (gl:vertex x0 y1 0)
     (gl:vertex x1 y1 0)))


;; Draw each scale line
(defun draw-scale-line ()
  (let ((x-init-pos 0.13)
        (x-final-pos 0.93)
        (x-buffer 0.025)
        (x-split-time 6)
        (y-init-pos 0.10)
        (y-final-pos 0.90)
        (y-buffer 0.02)
        (y-split-time 8)
        (scale-line-len 0.01))
    (gl:color 0 0 0)
    ;; x-axis
    (loop for i from 0 below x-split-time do
         (gl:with-primitive :line-strip
           (gl:vertex (+ x-init-pos
                         x-buffer
                         (* i
                            (/ (- x-final-pos x-init-pos)
                               x-split-time)))
                      y-init-pos
                      0)
           (gl:vertex (+ x-init-pos
                         x-buffer
                         (* i
                            (/ (- x-final-pos x-init-pos)
                               x-split-time)))
                      (- y-init-pos scale-line-len)
                      0)))
    ;; y-axis
    (loop for i from 0 below y-split-time do
         (gl:with-primitive :line-strip
           (gl:vertex x-init-pos
                      (+ y-init-pos
                         y-buffer
                         (* i
                            (/ (- y-final-pos y-init-pos)
                               y-split-time)))
                      0)
           (gl:vertex (- x-init-pos scale-line-len)
                      (+ y-init-pos
                         y-buffer
                         (* i
                            (/ (- y-final-pos y-init-pos)
                               y-split-time)))
                      0)))))
 


;; Draw input data and frame
(defmacro make-figure ()
  `(defmethod glut:display ((w base-window))
     (gl:clear :color-buffer)
     
     ;; Draw frame
     (draw-frame-2d ,0.13 ,0.10 ,0.93 ,0.90)


     ;; Draw Scale
     (draw-scale-line)
     

     ;; FIXME: Draw input data.
     (draw-title ,"Figure 1")
     
     ;; Start processing buffered OpenGL routines.
     (gl:flush)))
