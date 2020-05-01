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
                :symbol-downcase
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
  (setf *style* '())
  T)

(defparameter *palette*
  #("blue" "red" "green" "yellow" "cyan" "magenta"))

(defun choose-color (color supplied-p)
  (if supplied-p
      color
      (aref *palette*
            (mod (length *state*)
                 (length *palette*)))))



;;;; Scatter and Line
;;;
;;; This covers scatter and line plotting and their options.

;; Line2D
(defun line (&rest data)
  (push (apply #'-line (apply #'convert-data data))
        *state*)
  T)

(defun -line (x
              y
              &key
                (color "blue" c)
                (width 1 w)
                (name "" n))
  (remove-if #'null
             `((:x . ,x)
               (:y . ,y)
               (:type . "line")
               (:color . ,(choose-color color c))
               ,(if w (cons :width width))
               ,(if n (cons :name name)))))


;; Marker2D
(defun marker (&rest data)
  (push (apply #'-marker (apply #'convert-data data))
        *state*)
  T)

(defun -marker (x
                y
                &key
                  (color "blue" c)
                  (size 5 s)
                  (name "" n))
  (remove-if #'null
             `((:x . ,x)
               (:y . ,y)
               (:type . "marker")
               (:color . ,(choose-color color c))
               ,(if s (cons :size size))
               ,(if n (cons :name name)))))


;; fill
(defun fill (&rest data)
  (push (apply #'-fill data)
        *state*)
  T)

(defun -fill (x
              y0
              y1
              &key
                (color "blue" c)
                (name "" n))
  (remove-if #'null
             `((:x . ,x)
               (:y0 . ,y0)
               (:y1 . ,y1)
               (:type . "fill")
               (:color . ,(choose-color color c))
               ,(if n (cons :name name)))))


;; ErrorBar
(defun errorbar (&rest data)
  (push (apply #'-errorbar (apply #'convert-data data))
        *state*)
  T)

(defun -errorbar (x
                  y
                  &key
                    (error-x '())
                    (error-y '())
                    (color "blue" c))
  (assert (or error-x error-y))
  (remove-if #'null
             `((:x . ,x)
               (:y . ,y)
               (:type . "errorbar")
               (:color . ,(choose-color color c))
               ,(if (not (null error-x))
                    (cons :error-x error-x))
               ,(if (not (null error-y))
                    (cons :error-y error-y)))))


;; Bar plot
(defun bar (&rest data)
  (push (apply #'-bar (apply #'convert-data data))
        *state*)
  T)

(defun -bar (x
             y
             &key
               (name "" n))
  (remove-if #'null
             `((:x . ,x)
               (:y . ,y)
               (:type . "bar")
               ,(if n (cons :name name)))))


;; Pie chart
(defun pie (&rest data)
  (push (apply #'-pie data)
        *state*)
  T)

(defun -pie (values
             labels
             &key
               (name "" n))
  (remove-if #'null
             `((:values . ,values)
               (:labels . ,labels)
               (:type . "pie")
               ,(if n (cons :name name)))))


;; Box plots
(defun box (&rest data)
  (push (apply #'-box data)
        *state*)
  T)

(defun -box (y
             &key
               (color "blue" c)
               (name "" n)
               (boxmean t)
               (boxpoints :false))
  (remove-if #'null
             `((:y . ,y)
               (:type . "box")
               (:color . ,(choose-color color c))
               (:boxmean . ,boxmean)
               (:boxpoints . ,boxpoints)
               ,(if n (cons :name name)))))


;; Heatmap
(defun heatmap (&rest data)
  (push (apply #'-heatmap data)
        *state*)
  T)

(defun -heatmap (z
                 &key
                   (showscale :false))
  `((:z . ,z)
    (:type . "heatmap")
    (:showscale . ,showscale)))


;; Contour
(defun contour (&rest data)
  (push (apply #'-contour data)
        *state*)
  T)

(defun -contour (z
                 &key
                   (showscale :false)
                   (autocontour :false))
  `((:z . ,z)
    (:type . "contour")
    (:showscale . ,showscale)
    (:autocontour . ,autocontour)))


;; Line3D
(defun line3d (&rest data)
  (push (apply #'-line3d data)
        *state*)
  T)

(defun -line3d (x
                y
                z
                &key
                  (color "blue" c)
                  (width 1 w)
                  (name "" n))
  (remove-if #'null
             `((:x . ,x)
               (:y . ,y)
               (:z . ,z)
               (:type . "line3d")
               (:color . ,(choose-color color c))
               ,(if w (:width . ,width))
               ,(if n (cons :name name)))))


;; Marker3D
(defun marker3d (&rest data)
  (push (apply #'-marker3d data)
        *state*)
  T)

(defun -marker3d (x
                  y
                  z
                  &key
                    (color "blue" c)
                    (size 5 s)
                    (name "" n))
  (remove-if #'null
             `((:x . ,x)
               (:y . ,y)
               (:z . ,z)
               (:type . "marker3d")
               (:color . ,(choose-color color c))
               ,(if s (:size . ,size))
               ,(if n (cons :name name)))))


;; Surface
(defun surface (&rest data)
  (push (apply #'-surface data)
        *state*)
  T)

(defun -surface (z
                 &key
                   (name "" n))
  (remove-if #'null
             `((:z . ,z)
               (:type . "surface")
               ,(if n (cons :name name)))))



;;;; Layout
;;;
;;; To attach title or axis options to the graph.

(defun title (text)
  (push (cons :title text)
        *style*)
  T)

(defun xaxis (text)
  (push (cons :xaxis text)
        *style*)
  T)

(defun yaxis (text)
  (push (cons :yaxis text)
        *style*)
  T)

(defun showlegend ()
  (push (cons :showlegend t)
        *style*))



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
