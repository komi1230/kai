;;;; generate.lisp --- File generator and downloader
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file is composed of a collection of functions of file generator
;;; and file donwloader. 

(in-package :cl-user)

(defpackage :kai.plotly.generate
  (:use :cl)
  (:import-from :kai.converter
                :make-kai-cache)
  (:export :download-file
           :download-plotlyjs
           :save-html
           :save-js))
(in-package :kai.plotly.generate)


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
  (download-file (merge-pathnames "plotly/plotly-latest.min.js"
                                  (make-kai-cache))
                 "https://cdn.plot.ly/plotly-latest.min.js"))


;;;; HTML generator
;;;
;;; Create HTML file to plot in the browser.
;;; This file will be saved in the cache directory.

(defun generate-html ()
  (let ((style (cl-css:css `((html :height 100%)
                             (body :height 100%
                                   :display flex
                                   :justify-content center
                                   :align-items center))))
        (plotly-path (namestring (merge-pathnames "plotly/plotly-latest.min.js"
                                                  (make-kai-cache))))
        (my-plot (namestring (merge-pathnames "plotly/kai.js"
                                              (make-kai-cache)))))
    (who:with-html-output-to-string (_)
      (:html
       (:head
        (:style (who:str style))
        (:script :type "text/javascript" :src plotly-path))
       (:body
        (:div :id "myDiv")
        (:script :type "text/javascript" :src my-plot))))))


(defun save-html ()
  (let ((html-path (namestring (merge-pathnames "plotly/kai.html"
                                                (make-kai-cache))))
        (content (generate-html)))
    (with-open-file (s html-path :direction :output
                                 :if-exists :supersede)
      (format s "~A" content))))




(defun generate-js (states style)
  (let* ((len (length states))
         (traces (format nil "~{~A~}~3&"
                         (loop for i below len
                               collect (format nil "var trace~A = ~A;~&~&" i (nth i states)))))
         (layout (format nil "var layout = ~A;~3&" style))
         (data (format nil "var data = [~{~A~}];~3&"
                       (loop for i below len
                             collect (format nil "trace~A, " i))))
         (final-set (format nil "Plotly.newPlot('myDiv', data, layout)")))
    (format nil "~A~A~A~A" traces layout data final-set)))


(defun save-js (states style)
  (let ((js-path (namestring (merge-pathnames "plotly/kai.js"
                                              (make-kai-cache))))
        (content (generate-js states style)))
    (with-open-file (s js-path :direction :output
                               :if-exists :supersede)
      (format s "~A" content))))
