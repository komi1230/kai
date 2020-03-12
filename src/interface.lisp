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


;;;; Scatter and Line
;;;
;;; This covers scatter and line plotting and their options.

;; 2D
(defun plot (data0 &optional data1 (mode "markers") (name ""))
  (let ((len (length data0))
        (x-axis (loop for i below len collect i)))
    (if data1
        (data-to-json x-axis
                      data0
                      :mode mode
                      :name name)
        (data-to-json data0
                      data1
                      :mode mode
                      :name name))))
