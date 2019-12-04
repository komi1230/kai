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


;; Frame information
(defparameter *x0-lim* 0.13)
(defparameter *y0-lim* 0.10)
(defparameter *x1-lim* 0.93)
(defparameter *y1-lim* 0.90)



;;;; General functions
;;;
;;; In making window like a figure, we have to draw outline, scale
;;; and other character.
;;; To make it easy to draw line and string, we make some general functions.

(defun draw-string (str x y font)
  (gl:raster-pos x y)
  (set-color :black)
  (loop for i from 0 below (length str) do
       (glut:bitmap-character font
                              (char-code (aref str i)))))


(defun draw-line (x0 y0 x1 y1)
  (set-color :black)
  (gl:with-primitive :line-strip
     (gl:vertex x0 y0 0)
     (gl:vertex x1 y1 0)))



;;;; Basic functions
;;;
;;; These functions draw outlines and scales of the figure.
;;; They cover some user interface:
;;;       1) draw title
;;;       2) set frame
;;;       3) set scales

;; Draw title of the figure
(defun draw-title (str)
  (let ((x-pos (- 0.53         ; centering string
                  (* 0.018     ; 1 character = 0.018 points
                     (/ (length str)
                        2))))
        (y-pos 0.93))
    (draw-string str
                 x-pos
                 y-pos
                 glut:+bitmap-helvetica-18+)))


;; Draw frame of the figure: outline
(defun draw-frame-2d (x0 y0 x1 y1)
  (draw-line x0 y0 x1 y0)   ; bottom
  (draw-line x0 y0 x0 y1)   ; left
  (draw-line x1 y0 x1 y1)   ; right
  (draw-line x0 y1 x1 y1))  ; top

  

;; Draw each scale line and number
(defun draw-scale (min-max)
  (let ((x-buffer 0.025)
        (x-split-time 5)
        (y-buffer 0.02)
        (y-split-time 8)
        (scale-line-len 0.01)
        (x-min (caar min-max))
        (x-max (cadr min-max))
        (y-min (cadar min-max))
        (y-max (caddr min-max)))
    (gl:color 0 0 0)
    ;; x-axis
    (loop for i from 0 to x-split-time do
         (let* ((x-pos (+ *x0-lim*
                          x-buffer
                          (* i
                             (/ (- *x1-lim*
                                   *x0-lim*
                                   (* x-buffer 2))
                                x-split-time))))
                (num (write-to-string (float
                                         (+ x-min
                                           (* (/ (- x-max x-min)
                                                 x-split-time)
                                              i)))))
                (num-pos (- x-pos        
                            (* 0.012    ; 1 character = 0.012 point
                               (/ (length num)
                                  2)))))
           ;; scale line
           (draw-line x-pos
                      *y0-lim*
                      x-pos
                      (- *y0-lim* scale-line-len))
           ;; scale num
           (draw-string num
                        num-pos
                        (- *y0-lim* 0.04)
                        glut:+bitmap-helvetica-12+)))
    ;; y-axis
    (loop for i from 0 to y-split-time do
         (let* ((y-pos (+ *y0-lim*
                          y-buffer
                          (* i
                             (/ (- *y1-lim*
                                   *y0-lim*
                                   (* 2 y-buffer))
                                y-split-time))))
                (num (write-to-string (float
                                         (+ y-min
                                            (* (/ (- y-max y-min)
                                                  x-split-time)
                                               i)))))
                (num-pos (- *x0-lim*
                            0.02
                            (* 0.012 (length num)))))
           ;; scale line
           (draw-line *x0-lim*
                      y-pos
                      (- *x0-lim* scale-line-len)
                      y-pos)
           ;; scale num
           (draw-string num
                        num-pos
                        y-pos
                        glut:+bitmap-helvetica-12+)))))



;;;; Plotting
;;;
;;; Here we finally plot or dot in the figure.
;;; To make user interface simple, we wrap functions (plotting, dotting ...)
;;; with generic functions or macros.

(defun plot-dot (x y color)
  (set-color color)
  (gl:with-primitive :points
    (gl:vertex x y 0)))

;; To plot with line, connect each dot.
(defun connect-dot (x0 y0 x1 y1 color)
  (set-color color)
  (gl:line-width 3)
  (gl:with-primitive :line-strip
     (gl:vertex x0 y0 0)
     (gl:vertex x1 y1 0)))




;;;; Draw
;;;
;;; Finally we draw a figure.
;;;

;; Draw input data and frame
(defmacro make-figure ()
  `(defmethod glut:display ((w base-window))
     (gl:clear :color-buffer)
     
     ;; Draw frame
     (draw-frame-2d *x0-lim*
                    *y0-lim*
                    *x1-lim*
                    *y1-lim*)


     ;; Draw Scale
     (draw-scale '((1.0 10.0) -3.2 10.5))
     

     ;; FIXME: Draw input data.
     (draw-title "Figure 1")


     (plot-line 0.3 0.2 0.6 0.7 :blue)
     
     ;; Start processing buffered OpenGL routines.
     (gl:flush)))
