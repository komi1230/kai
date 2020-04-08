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
                :style
                :scatter
                :pie
                :sunburst
                :box
                :heatmap
                :contour
                :scatter3d
                :surface
                :show)
  (:export :line
           :marker
           :pie-chart
           :sunburst-chart
           :heatmap-chart
           :box-chart
           :contour-chart))
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

(defparameter sunburst-value
  '(10 14 12 10 2 6 6 4 4))

(defparameter sunburst-label
  '("Eve" "Cain" "Seth" "Enos" "Noam" "Abel" "Awan" "Enoch" "Azura"))

(defparameter sunburst-parent
  '("" "Eve" "Eve" "Seth" "Seth" "Eve" "Eve" "Awan" "Eve"))

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


;;;; Pie chart
;;;
;;; pie cahrt with labels

(defun pie-chart ()
  (pie pie-data pie-label)
  (style :title "Pie chart example")
  (show))


;;;; Sunburst
;;;
;;; sunburst chart can be usable only in plotly

(defun sunburst-chart ()
  (sunburst sunburst-value
            sunburst-label
            sunburst-parent)
  (style :title "Sunburst chart example")
  (show))


;;;; Box
;;;
;;; box plots example

(defun box-chart ()
  (box box-data)
  (style :title "Box chart example")
  (show))


;;;; Heatmap
;;;
;;; heatmap example

(defun heatmap-chart ()
  (heatmap heatmap-data
           :showscale t)
  (style :title "Heatmap example")
  (show))


;;;; Contour
;;;
;;; Contour example

(defun contour-chart ()
  (contour contour-data
           :showscale t)
  (style :title "Contour example")
  (show))
