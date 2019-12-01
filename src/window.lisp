;;;; window.lisp --- A base class of GL window to plot in.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file is composed of a base of meta class of window for
;;; plotting. The constructor can be used to plot easily.


(in-package :cl-user)
(defpackage #:kai.window
  (:use :cl)
  (:import-from :glut
                :window)
  (:import-from :gl
                :color
                :clear
                :clear-color
                :flush
                :matrix-mode
                :load-identity
                :ortho
                :vertex
                :with-primitive)
  (:export :make-base-window
           :setup-base-window))
(in-package #:kai.window)


;;;; Base window
;;; Declare initial window size, position, and display mode (single
;;; buffer and RGBA).
;;; Open window with "Kai" in its title bar.
;;; Call initialization routines.
;;; Register callback function to display graphics.
;;; Enter main loop and process events.


(defparameter *title* "Kai")
(defparameter *width* 500)
(defparameter *height* 500)

(defmacro make-base-window (title width height)
  `(defclass base-window (glut:window)
     ()
     (:default-initargs :pos-x ,100
                        :pos-y ,100
                        :width ,width
                        :height ,height
                        :mode '(:single :rgb) :title ,title)))

(defmacro setup-base-window ()
  `(defmethod glut:display-window :before ((w base-window))
     ;; Select clearing color.
     (gl:clear-color ,1 ,1 ,1 ,1)
     ;; Initialize viewing values.
     (gl:matrix-mode :projection)
     (gl:load-identity)
     (gl:ortho ,0 ,1 ,0 ,1 ,-1 ,1)))

(defmacro plot-hoge ()
  `(defmethod glut:display ((w base-window))
     (gl:clear :color-buffer)
  ;; Draw white polygon (rectangle) with corners at
  ;; (0.25, 0.25, 0.0) and (0.75, 0.75, 0.0).
  (gl:color ,1 ,0 ,0)
  (gl:with-primitive :polygon
    (gl:vertex ,0.25 ,0.25 ,0)
    (gl:vertex ,0.75 ,0.25 ,0)
    (gl:vertex ,0.75 ,0.75 ,0)
    (gl:vertex ,0.25 ,0.75 ,0))
  ;; Start processing buffered OpenGL routines.
  (gl:flush)))


