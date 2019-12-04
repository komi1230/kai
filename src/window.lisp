;;;; window.lisp --- A base class of GL window to plot in.
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file is composed of a base of meta class of window for
;;; plotting. The constructor can be used to plot easily.


(in-package :cl-user)

(ql:quickload :cl-opengl)
(ql:quickload :cl-glu)
(ql:quickload :cl-glut)

(defpackage #:kai.window
  (:use :cl)
  (:export :make-base-window
           :setup-base-window
           :plot))
(in-package #:kai.window)



;;;; Base window
;;;
;;; Declare initial window size, position, and display mode (single
;;; buffer and RGBA).
;;; Open window with "Kai" in its title bar.
;;; Call initialization routines.
;;; Register callback function to display graphics.
;;; Enter main loop and process events.


;; Initial settings
(defparameter *title* "Kai")
(defparameter *width* 600)
(defparameter *height* 600)



;; Open base window
(defmacro make-base-window (title width height)
  `(defclass base-window (glut:window)
     ()
     (:default-initargs :pos-x ,100
                        :pos-y ,100
                        :width ,width
                        :height ,height
                        :mode '(:single :rgb) :title ,title)))


;; Draw initial background
(defmacro setup-base-window ()
  `(defmethod glut:display-window :before ((w base-window))
     ;; Clear buffer
     (gl:clear-color ,1 ,1 ,1 ,0)
              
     ;; Initialize viewing values.
     (gl:matrix-mode ,:projection)
     (gl:load-identity)
     (gl:ortho ,0 ,1 ,0 ,1 ,-1 ,1)))






;;----------------- For Debug ---------------------
(defun main ()
  (make-base-window *title* *width* *height*)
  (setup-base-window)
  (make-figure)
  (glut:display-window (make-instance 'base-window)))
