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
(defpackage #:kai.converter
  (:use :cl)
  (:export :make-kai-cache
           :check-file-exist
           :data-to-json
           :style-to-json))
(in-package #:kai.converter)



;;;; JSON File Path Check
;;;
;;; When plotting, we will generate json file from common lisp codes.
;;; Here we check file path.
;;; Check if .cache file exists in the home directory.
;;; And create cache directory for Kai to put .js files.

(defun make-kai-cache (dir-name)
  (ensure-directories-exist
   (merge-pathnames (format nil ".cache/kai/~A/" dir-name)
                    (user-homedir-pathname))))

(defun check-file-exist (dir filename)
  (probe-file (merge-pathnames filename
                               (make-kai-cache dir))))

;;;; JSON Generator
;;;
;;; When accepted some inputs, we convert the data to JSON data at first
;;; because we design kai to be able to switch backends easily.

(defun to-json (param)
  (let ((jonathan:*false-value* :false)
        (jonathan:*null-value* :null)
        (jonathan:*empty-array-value* :empty-array)
        (jonathan:*empty-object-value* :empty-object))
    (jonathan:to-json param)))

;; Generate
(defun data-to-json (&key
                     (data0 :null)
                     (data1 :null)
                     (data2 '())
                     (type "scatter")
                     (mode "")
                     (name "")
                     (text '())
                     (error-x '())
                     (error-y '())
                     (fill "")
                     (fillcolor "")
                     (line '())
                     (marker '())
                     (value '())
                     (label '())
                     (parents '())
                     (boxmean "")
                     (boxpoints "")
                     (showscale :false)
                     (colorscale '())
                     (contours '())
                     (autocontour :false))
  (to-json `(:|x| ,data0
             :|y| ,data1
             :|z| ,data2
             :|type| ,type
             :|mode| ,mode
             :|name| ,name
             :|text| ,text
             :|error_x| ,error-x
             :|error_y| ,error-y
             :|fill| ,fill
             :|fillcolor| ,fillcolor
             :|line| ,line
             :|marker| ,marker
             :|values| ,value
             :|labels| ,label
             :|parents| ,parents
             :|boxmean| ,boxmean
             :|boxpoints| ,boxpoints
             :|showscale| ,showscale
             :|colorscale| ,colorscale
             :|contours| ,contours
             :|autocontour| ,autocontour)))


(defun style-to-json (&key
                      title
                      xaxis
                      yaxis)
  (to-json `(:|title| ,title
             :|xaxis| ,xaxis
             :|yaxis| ,yaxis)))
