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
(defpackage :kai.converter
  (:use :cl)
  (:export :to-json))
(in-package :kai.converter)


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
