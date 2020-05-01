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
                :to-json
                :plotly-code)
  (:import-from :kai.util
                :make-kai-cache
                :download-file)
  (:export :download-file
           :download-plotlyjs
           :save-html
           :save-js))
(in-package :kai.plotly.generate)


;;;; Donwload client
;;;
;;; When using Plotly, plotly-latest.js is needed.
;;; Here is a set of file donwload client and file checker.
(defun download-plotlyjs ()
  (download-file (merge-pathnames "plotly-latest.min.js"
                                  (make-kai-cache "plotly"))
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
        (plotly-path (namestring (merge-pathnames "plotly-latest.min.js"
                                                  (make-kai-cache "plotly"))))
        (my-plot (namestring (merge-pathnames "kai.js"
                                              (make-kai-cache "plotly")))))
    (who:with-html-output-to-string (_)
      (:html
       (:head
        (:style (who:str style))
        (:script :type "text/javascript" :src plotly-path))
       (:body
        (:div :id "myDiv")
        (:script :type "text/javascript" :src my-plot))))))


(defun save-html ()
  (let ((html-path (namestring (merge-pathnames "kai.html"
                                                (make-kai-cache "plotly"))))
        (content (generate-html)))
    (with-open-file (s html-path :direction :output
                                 :if-exists :supersede)
      (format s "~A" content))))




(defun generate-js (states style)
  (let* ((len (length states))
         (json-traces (plotly-code states))
         (traces (format nil "~{~A~}~3&"
                         (loop for i below len
                               collect (format nil "var trace~A = ~A;~&~&" i
                                               (nth i json-traces)))))
         (layout (format nil "var layout = ~A;~3&" (to-json style)))
         (data (format nil "var data = [~{~A~}];~3&"
                       (loop for i below len
                             collect (format nil "trace~A, " i))))
         (final-set (format nil "Plotly.newPlot('myDiv', data, layout)")))
    (format nil "~A~A~A~A" traces layout data final-set)))


(defun save-js (states style)
  (let ((js-path (namestring (merge-pathnames "kai.js"
                                              (make-kai-cache "plotly"))))
        (content (generate-js states style)))
    (with-open-file (s js-path :direction :output
                               :if-exists :supersede)
      (format s "~A" content))))
