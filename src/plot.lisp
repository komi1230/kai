;;;; plot.lisp --- plotter as an interface
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file covers plotting interface and preprocess for it.
;;; Some preprocess functions are imported from util.lisp


(in-package :cl-user)
(defpackage #:kai.plot
  (:use :cl)
  (:import-from :kai.window
                :draw-line
                :draw-string)
  (:import-from :kai.util
                :set-color
                :find-min-max
                :to-array)
  (:export :make-figure))
(in-package #:kai.plot)


;; Frame information
(defparameter *x0-lim* 0.13)
(defparameter *y0-lim* 0.10)
(defparameter *x1-lim* 0.93)
(defparameter *y1-lim* 0.90)
(defparameter *x-buffer* 0.025)
(defparameter *y-buffer* 0.02)



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
  (let ((x-split-time 5)
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
                          *x-buffer*
                          (* i
                             (/ (- *x1-lim*
                                   *x0-lim*
                                   (* *x-buffer* 2))
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
                          *y-buffer*
                          (* i
                             (/ (- *y1-lim*
                                   *y0-lim*
                                   (* 2 *y-buffer*))
                                y-split-time))))
                (num (write-to-string (float
                                         (+ y-min
                                            (* (/ (- y-max y-min)
                                                  y-split-time)
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

(defun plot-one-dot (x y color)
  (set-color color)
  (gl:with-primitive :points
    (gl:vertex x y 0)))

(defun plot-dot (data color)
  (let ((num (car (array-dimensions data))))
    (loop for i from 0 below num do
         (plot-one-dot (aref data i 0)
                       (aref data i 1)
                       color))))

;; Plot with line, connect each dot.
(defun connect-dot (x0 y0 x1 y1 color)
  (set-color color)
  (gl:line-width 3)
  (gl:with-primitive :line-strip
     (gl:vertex x0 y0 0)
     (gl:vertex x1 y1 0)))

(defun plot-line (data color)
  (let ((num (car (array-dimensions data))))
    (loop for i from 0 below (1- num) do
         (connect-dot (aref data i 0)
                      (aref data i 1)
                      (aref data (1+ i) 0)
                      (aref data (1+ i) 1)
                      color))))


;; Regularize each element to accordings
(defun set-accordings (data)
  (let* ((shape (array-dimensions data))
         (min-max (find-min-max data))
         (x-min (caar min-max))
         (x-max (cadr min-max))
         (x-range (- x-max x-min))
         (y-min (cadar min-max))
         (y-max (caddr min-max))
         (y-range (- y-max y-min))
         (data-accord (make-array shape)))
    (loop for i from 0 below (car shape) do
         (setf (aref data-accord i 0)
               (+ *x0-lim*
                  *x-buffer*
                  (* (- *x1-lim* *x0-lim* (* *x-buffer* 2))
                     (/ (- (aref data i 0) x-min)
                        x-range))))
         (setf (aref data-accord i 1)
               (+ *y0-lim*
                  *y-buffer*
                  (* (- *y1-lim* *y0-lim* (* *y-buffer* 2))
                     (/ (- (aref data i 1) y-min)
                        y-range)))))
    data-accord))




;;;; Draw
;;;
;;; Finally we draw a figure.
;;;

;; Draw input data and frame
(defmacro make-figure (data type color title)
  (let ((converted-data (set-accordings (to-array data))))
    `(defmethod glut:display ((w base-window))
       (gl:clear :color-buffer)
     
       ;; Draw frame
       (draw-frame-2d *x0-lim*
                      *y0-lim*
                      *x1-lim*
                      *y1-lim*)

       ;; Draw Scale
       (draw-scale (find-min-max ,converted-data))

       ;; FIXME: Draw input data.
       (draw-title ,title)

       ;; Draw line or dot
       (case ,type
         (:dot (plot-dot ,converted-data ,color))
         (:line (plot-line ,converted-data ,color))
         (t (plot-line ,converted-data ,color)))
     
       ;; Start processing buffered OpenGL routines.
       (gl:flush))))
