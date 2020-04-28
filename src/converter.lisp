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


;;;; Ploly converter : JSON Generator
;;;
;;; When accepted some inputs, we convert the data to JSON data at first
;;; because we design kai to be able to switch backends easily.

(defun to-json (param)
  (let ((jonathan:*false-value* :false)
        (jonathan:*null-value* :null)
        (jonathan:*empty-array-value* :empty-array)
        (jonathan:*empty-object-value* :empty-object))
    (jonathan:to-json param)))



;;;; GR converter : regularization
;;;
;;; Accordings in GR is expressed with relative values between
;;; 0 and 1. Here we provide accordings regularizer.

(defun min-max (lst)
  (cons (apply #'min lst)
        (apply #'max lst)))


(defun regularize (lst)
  (let* ((tmp-min-max (min-max lst))
         (range (- (cdr tmp-min-max)
                   (car tmp-min-max))))
    (mapcar #'(lambda (x)
                (/ (- x (car tmp-min-max))
                   range))
            lst)))
