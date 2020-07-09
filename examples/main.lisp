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
                :title
                :line
                :pie
                :box
                :heatmap
                :contour
                :line3d
                :surface
                :fillarea
                :marker
                :marker3d
                :xaxis
                :yaxis
                :show)
  (:export :line
           :marker-example
           :pie-chart
           :sunburst-chart
           :heatmap-chart
           :box-chart
           :contour-chart
           :line3d
           :marker3d-example
           :surface-chart))
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

(defparameter pie-label
  '("Residential" "Non-Residential" "Utility"))

(defparameter pie-data
  '(19 26 55))

(defparameter box-data
  (loop :repeat 50
        :collect (random 100.0)))

(defparameter heatmap-data
  (loop :repeat 100
        :collect (loop :repeat 100
                       :collect (random 10.0))))

(defparameter contour-data
  (loop :repeat 20
        :collect (loop :repeat 20
                       :collect (random 10.0))))


;;;; Basic plotting
;;;
;;; Line and scatter

(defun line-chart ()
  (line x-data-index
        y-data-10)
  (title "Line plot example")
  (xaxis (list :|title| "X axis"))
  (yaxis (list :|title| "Y axis"))
  (show))

(defun marker-example ()
  (marker x-data-random
          y-data-100
          :size 20)
  (title "Marker plot example")
  (show))

(defun fill-chart ()
  (fillarea '(1 2 3)
            '(2 1 3)
            '(5 2 7)
            :color :aqua)
  (title "Fill chart with line plotting")
  (show))


;;;; Pie chart
;;;
;;; pie cahrt with labels

(defun pie-chart ()
  (pie pie-data pie-label)
  (title "Pie chart example")
  (show))


;;;; Box
;;;
;;; box plots example

(defun box-chart ()
  (box box-data)
  (title "Box chart example")
  (show))


;;;; Heatmap
;;;
;;; heatmap example

(defun heatmap-chart ()
  (heatmap heatmap-data
           :showscale t)
  (title "Heatmap example")
  (show))


;;;; Contour
;;;
;;; Contour example

(defun contour-chart ()
  (contour contour-data
           :showscale t)
  (title "Contour example")
  (show))


;;;; Scatter3D
;;;
;;; scatter3d plots: line and marker

(defun line3d-example ()
  (line3d x-data-random
          y-data-100
          z-data-100)
  (title "Line3D plot example")
  (show))

(defun marker3d-example ()
  (marker3d x-data-random
            y-data-100
            z-data-100)
  (title "Marker3D plot example")
  (show))


;;;; Surface
;;;
;;; surface chart-example

(defun surface-chart ()
  (surface contour-data)
  (title "Surface chart example")
  (show))
