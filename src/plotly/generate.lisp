;;;; generate.lisp --- File generator and downloader
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file is composed of a collection of functions of file generator
;;; and file donwloader. 

(in-package :cl-user)

(defpackage :kai.plotly.generate
  (:use :cl))
(in-package :kai.plotly.generate)


;;;; File exists
;;;
;;; Check if .cache file exists in the home directory.
;;; And create cache directory for Kai to put .js files.
(defun make-kai-cache ()
  (ensure-directories-exist
   (merge-pathnames ".cache/kai/"
                    (user-homedir-pathname))))

(defun check-plotly-file ()
  (probe-file (merge-pathnames ".cache/kai/plotly-latest.min.js"
                               (user-homedir-pathname))))


;;;; Donwload client
;;;
;;; When using Plotly, plotly-latest.js is needed.
;;; Here is a set of file donwload client and file checker.
(defun download-file (filename uri)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede
                   :element-type '(unsigned-byte 8))
    (with-open-stream (input (drakma:http-request uri :want-stream t :connection-timeout nil))
      (loop :for b := (read-byte input nil -1)
            :until (minusp b)
            :do (write-byte b out)))))

(defun download-plotlyjs ()
  (download-file (merge-pathnames "plotly-latest.min.js"
                                  (make-kai-cache))
                 "https://cdn.plot.ly/plotly-latest.min.js"))
