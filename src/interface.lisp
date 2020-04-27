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
(defpackage :kai.interface
  (:use :cl)
  (:import-from :kai.util
                :convert-data
                :symbol-downcase)
  (:import-from :kai.converter
                :check-file-exist
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
           :surface
           :style
           :show))
(in-package :kai.interface)



;;;; State
;;;
;;; To make it able to plot multiple graph, we have a state as list.

(defparameter *state* '())

(defparameter *style* '())

(defun reset! ()
  (setf *state* '())
  (setf *style* '()))


;;;; Scatter and Line
;;;
;;; This covers scatter and line plotting and their options.

;; 2D scatter
(defun scatter (&rest data)
  (push (apply #'-scatter (apply #'convert-data data))
        *state*)
  T)

(defun -scatter (x
                 y
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
  (symbol-downcase
   `( :x ,x
      :y ,y
      :type "scatter"
      :mode ,mode
      ,@(if (not (string= name ""))
            (list :name name))
      ,@(if (not (null text))
            (list :text text))
      ,@(if (not (null error-x))
            (list :error_x error-x))
      ,@(if (not (null error-y))
            (list :error_y error-y))
      ,@(if (not (string= fill ""))
            (list :fill fill))
      ,@(if (not (string= fillcolor ""))
            (list :fillcolor fillcolor))
      ,@(if (not (null line))
            (list :line (symbol-downcase line)))
      ,@(if (not (null marker))
            (list :marker (symbol-downcase marker))))))


;; Bar plot
(defun bar (&rest data)
  (push (apply #'-bar (apply #'convert-data data))
        *state*)
  T)

(defun -bar (x
             y
             &key
               (name "")
               (text '())
               (error-x '())
               (error-y '())
               (fill "")
               (fillcolor "")
               (marker '()))
  (symbol-downcase
   `( :x ,x
      :y ,y
      :type "bar"
      ,@(if (not (string= name ""))
            (list :name name))
      ,@(if (not (null text))
            (list :text text))
      ,@(if (not (null error-x))
            (list :error_x error-x))
      ,@(if (not (null error-y))
            (list :error_y error-y))
      ,@(if (not (string= fill ""))
            (list :fill fill))
      ,@(if (not (string= fillcolor ""))
            (list :fillcolor fillcolor))
      ,@(if (not (null marker))
            (list :marker (symbol-downcase marker))))))


;; Pie chart
(defun pie (&rest data)
  (push (apply #'-pie data)
        *state*)
  T)

(defun -pie (values
             labels
             &key
               (name ""))
  (symbol-downcase
   `(:values ,values
     :labels ,labels
     ,@(if (not (string= name ""))
           (list :name name)))))


;; Sunburst
(defun sunburst (&rest data)
  (push (apply #'-sunburst data)
        *state*)
  T)

(defun -sunburst (values
                  labels
                  parents
                  &key
                    (marker '()))
  (symbol-downcase
   `(:value ,values
     :label ,labels
     :parents ,parents
     :type "sunburst"
     ,@(if (not (null marker))
           (list :marker (symbol-downcase marker))))))


;; Box plots
(defun box (&rest data)
  (push (apply #'-box data)
        *state*)
  T)

(defun -box (y
             &key
               (name "")
               (marker '())
               (boxmean t)
               (boxpoints :false))
  (symbol-downcase
   `(:y ,y
     :type "box"
     :boxmean ,boxmean
     :boxpoints ,boxpoints
     ,@(if (not (string= name ""))
           (list :name name))
     ,@(if (not (null marker))
           (list :marker (symbol-downcase marker))))))


;; Heatmap
(defun heatmap (&rest data)
  (push (apply #'-heatmap data)
        *state*)
  T)

(defun -heatmap (z
                 &key
                   (colorscale '())
                   (showscale :false))
  (symbol-downcase
   `(:z ,z
     :type "heatmap"
     :showscale ,showscale
     ,@(if (not (null colorscale))
           (list :marker (symbol-downcase colorscale))))))


;; Contour
(defun contour (&rest data)
  (push (apply #'-contour data)
        *state*)
  T)

(defun -contour (z
                 &key
                   (colorscale '())
                   (showscale :false)
                   (autocontour :false)
                   (contours '()))
  (symbol-downcase
   `(:z ,z
     :type "contour"
     :showscale ,showscale
     :autocontour ,autocontour
     ,@(if (not (null colorscale))
           (list :marker (symbol-downcase colorscale)))
     ,@(if (not (null contours))
           (list :marker (symbol-downcase contours))))))


;; Scatter3D
(defun scatter3d (&rest data)
  (push (apply #'-scatter3d data)
        *state*)
  T)

(defun -scatter3d (x
                   y
                   z
                   &key
                     (mode "markers")
                     (name "")
                     (text '())
                     (marker '())
                     (line '()))
  (symbol-downcase
   `(:x ,x
     :y ,y
     :z ,z
     :type "scatter3d"
     :mode ,mode
     ,@(if (not (string= name ""))
           (list :name name))
     ,@(if (not (null text))
           (list :text text))
     ,@(if (not (null line))
           (list :line (symbol-downcase line)))
     ,@(if (not (null marker))
           (list :marker (symbol-downcase marker))))))


;; Surface
(defun surface (&rest data)
  (push (apply #'-surface data)
        *state*)
  T)

(defun -surface (z
                 &key
                   (name ""))
  (symbol-downcase
   `(:z ,z
     :type "surface"
     ,@(if (not (string= name ""))
           (list :name name)))))



;;;; Layout
;;;
;;; To attach title or axis options to the graph.

(defun style (&key
                (title "")
                (xaxis '())
                (yaxis '()))
  (setf *style*
        `(:title ,title
                 ,@(if (not (null xaxis))
                       (list :xaxis (symbol-downcase xaxis)))
                 ,@(if (not (null yaxis))
                       (list :yaxis (symbol-downcase yaxis)))))
  T)


;;;; Plot
;;;
;;; Launch viewer and draw traces and styles.

(defun show ()
  (ensure-directories-exist
   (make-kai-cache "plotly"))
  (if (not (check-file-exist "plotly"
                             "kai.html"))
      (save-html))
  (if (not (check-file-exist "plotly"
                             "plotly-latest.min.js"))
      (download-plotlyjs))
  (save-js *state* *style*)
  (open-browser)
  (reset!)
  T)
