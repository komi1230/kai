;;;; converter.lisp --- JSON generator from Common Lisp codes 
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file is composed of a collection of JSON file generator.
;;; Kai can be used with a variety of backends.
;;; Here we use JSON and provide a common platform for a variety of
;;; backends.

(in-package :cl-user)
(defpackage #:kai.interface
  (:use :cl)
  (:import-from :kai.converter
                :check-file-exist
                :data-to-json
                :style-to-json
                :make-kai-cache)
  (:import-from :kai.plotly.generate
                :download-plotlyjs
                :save-html
                :save-js)
  (:import-from :kai.plotly.launch
                :open-browser)
  (:export :*state*
           :*style*
           :reset!
           :scatter
           :pie
           :sunburst
           :box
           :heatmap
           :contour
           :scatter3d
           :style
           :show))
(in-package #:kai.interface)



;;;; Input style converter
;;;
;;; When getting input data, we accept variable length args.
;;; We cannot realize to accept one or two args with some options by
;;; standard style, so we papare such a function to convert args.

(defun convert-data (&rest data)
  (let ((x (car data))
        (y (cadr data)))
    (if (or (consp x)        ; check first data
            (vectorp x))
        (if (or (consp y)    ; check second data  
                (vectorp y))
            data
            `(,(loop for i below (length x) collect i)
              ,x
              ,@(cdr data)))
        (error "Invalid input"))))


;;;; State
;;;
;;; To make it able to plot multiple graph, we have a state as list.

(defparameter *state* '())

(defparameter *style* "{}")

(defun reset! ()
  (setf *state* '())
  (setf *style* "{}"))


;;;; Scatter and Line
;;;
;;; This covers scatter and line plotting and their options.

;; 2D scatter
(defun scatter (&rest data)
  (push (apply #'-scatter (apply #'convert-data data))
        *state*)
  (format t "Set scatter plotting."))

(defun -scatter (data0
                 data1
                 &key
                   (mode "lines")
                   (name "")
                   (text '())
                   (error-x '())
                   (error-y '())
                   (fill "")
                   (fillcolor "")
                   (line '())
                   (marker '()))
  (data-to-json :data0 data0
                :data1 data1
                :type "scatter"
                :mode mode
                :name name
                :text text
                :error-x error-x
                :error-y error-y
                :fill fill
                :fillcolor fillcolor
                :line line
                :marker marker))

;; Bar plot
(defun bar (&rest data)
  (push (apply #'-bar (apply #'convert-data data))
        *state*)
  (format t "Set bar plotting."))

(defun -bar (data0
             data1
             &key
               (name "")
               (text '())
               (error-x '())
               (error-y '())
               (fill "")
               (fillcolor "")
               (marker '()))
  (data-to-json :data0 data0
                :data1 data1
                :type "bar"
                :name name
                :text text
                :error-x error-x
                :error-y error-y
                :fill fill
                :fillcolor fillcolor
                :marker marker))

;; Pie chart
(defun pie (&rest data)
  (push (apply #'-pie data)
        *state*)
  (format t "Set pie plotting."))

(defun -pie (value
             label
             &key
               (name "")
               (marker '()))
  (data-to-json :type "pie"
                :name name
                :marker marker
                :value value
                :label label))

;; Sunburst
(defun sunburst (&rest data)
  (push (apply #'-sunburst data)
        *state*)
  (format t "Set sunburst plotting."))

(defun -sunburst (value
                  label
                  parents
                  &key
                    (marker '()))
  (data-to-json :type "sunburst"
                :marker marker
                :value value
                :label label
                :parents parents))



;; Box plots
(defun box (&rest data)
  (push (apply #'-box data)
        *state*)
  (format t "Set box plotting."))

(defun -box (data
             &key
               (name "")
               (marker '())
               (boxmean t)
               (boxpoints :false))
  (data-to-json :data1 data
                :type "box"
                :name name
                :marker marker
                :boxmean boxmean
                :boxpoints boxpoints))

;; Heatmap
(defun heatmap (&rest data)
  (push (apply #'-heatmap data)
        *state*)
  (format t "Set heatmap plotting."))

(defun -heatmap (z
                 &key
                   (x '())
                   (y '())
                   (colorscale '())
                   (showscale :false))
  (data-to-json :data0 x
                :data1 y
                :data2 z
                :type "heatmap"
                :colorscale colorscale
                :showscale showscale))

;; Contour
(defun contour (&rest data)
  (push (apply #'-contour data)
        *state*)
  (format t "Set contour plotting."))

(defun -contour (z
                 &key
                   (x '())
                   (y '())
                   (colorscale '())
                   (showscale :false)
                   (autocontour :false)
                   (contours '()))
  (data-to-json :data0 x
                :data1 y
                :data2 z
                :type "contour"
                :colorscale colorscale
                :showscale showscale
                :autocontour autocontour
                :contours contours))


;; Scatter3D
(defun scatter3d (&rest data)
  (push (apply #'-scatter3d data)
        *state*)
  (format t "Set scatter3D plotting."))

(defun -scatter3d (x
                   y
                   z
                   &key
                     (mode "markers")
                     (name "")
                     (text '())
                     (marker '())
                     (line '()))
  (data-to-json :data0 x
                :data1 y
                :data2 z
                :type "scatter3d"
                :mode mode
                :name name
                :text text
                :marker marker
                :line line))



;;;; Layout
;;;
;;; To attach title or axis options to the graph.

(defun style (&key
                (title "")
                (xaxis '())
                (yaxis '()))
  (setf *style* (style-to-json :title title
                               :xaxis xaxis
                               :yaxis yaxis))
  (format t "Set style."))


;;;; Plot
;;;
;;; Launch viewer and draw traces and styles.

(defun show ()
  (make-kai-cache)
  (ensure-directories-exist (merge-pathnames "plotly/"
                                             (make-kai-cache)))
  (if (not (check-file-exist "plotly/kai.html"))
      (save-html))
  (if (not (check-file-exist "plotly/plotly-latest.min.js"))
      (download-plotlyjs))
  (save-js *state* *style*)
  (open-browser)
  (reset!)
  (format t "Show your graph."))
