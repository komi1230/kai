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

(defun check-file-exist (file-name)
  (probe-file (merge-pathnames file-name
                               (make-kai-cache))))


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


;;;; HTML generator
;;;
;;; Create HTML file to plot in the browser.
;;; This file will be saved in the cache directory.

(defun generate-plot (plot-code &key (width 1000) (height 700))
  (let ((style (cl-css:css `((html :height 100%)
                             (body :height 100%
                                   :display flex
                                   :justify-content center
                                   :align-items center)
                             ("#plot" :width ,#?"${width}px"
                                      :height ,#?"${height}px")))))
    (who:with-html-output-to-string (_)
      (:html
       (:head
        (:script :src "https://cdn.plot.ly/plotly-latest.min.js")
        (:style (who:str style)))
       (:body
        (:div :id "plot")
        (:script (who:str plot-code)))))))

