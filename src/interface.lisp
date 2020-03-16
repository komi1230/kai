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
                :data-to-json))
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
            `((quote ,(loop for i below (length x) collect i))
              (quote ,x)
              ,@(cdr data)))
        (error "Invalid input"))))


;;;; State
;;;
;;; To make it able to plot multiple graph, we have a state as list.

(defparameter *state* '())

(defparameter *style* "")

(defun reset! ()
  (setf *state* '())
  (setf *style* ""))


;;;; Scatter and Line
;;;
;;; This covers scatter and line plotting and their options.

;; 2D scatter
(defun scatter (&rest data)
  (push (eval `(-scatter ,@(apply #'convert-data data)))
        *state*))

(defun -scatter (data0
                 data1
                 &key
                   (type "scatter")
                   (mode "markers")
                   (name "")
                   (text '())
                   (error-x '())
                   (error-y '())
                   (fill "")
                   (fillcolor "")
                   (line '())
                   (marker '()))
  (scatter-to-json :data0 data0
                   :data1 data1
                   :type type
                   :mode mode
                   :name name
                   :text text
                   :error-x error-x
                   :error-y error-y
                   :fill fill
                   :fillcolor fillcolor
                   :line line
                   :marker marker))


;;;; Layout
;;;
;;; To attach title or axis options to the graph.

(defun style (&key
                 (title "Scatter Plot"))
  (setf *style* (style-to-json :title title)))


;;;; Plot
;;;
;;; Launch viewer and draw traces and styles.

(defun show ()
  (save-js *state* *style*)
  (if (equal *style* "")
      (style))
  (open-browser)
  (reset!))
