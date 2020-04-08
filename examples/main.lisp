;;;; scatter.lisp - A collection of scatter plotting
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file is composed of a collection of exampeles of plotting.
;;; Here's codes are mainly deal scatter plotting.

(in-package :cl-user)
(defpackage :kai-example
  (:use :cl)
  (:import-from :kai
                :scatter
                :style
                :show)
  (:export :line
           :marker))
(in-package :kai-example)


;;;; Parameters
;;;
;;; These are parameters to plot in the figure.

(defparameter x-data-index
  (loop for i from 0 below 10
        collect i))

(defparameter x-data-random
  (loop for i from 0 below 100
        collect (random 10.0)))

(defparameter y-data-10
  (loop for i from 0 below 10
        collect (random 10.0)))

(defparameter y-data-100
  (loop for i from 0 below 100
        collect (random 10.0)))

(defparameter z-data-100
  (loop for i from 0 below 100
        collect (random 10.0)))



;;;; Basic plotting
;;;
;;; Line and scatter

(defun line ()
  (scatter x-data-index
           y-data-10)
  (style :title "Line plot example")
  (show))

(defun marker ()
  (scatter x-data-random
           y-data-100
           :mode "markers")
  (style :title "Marker plot example")
  (show))
