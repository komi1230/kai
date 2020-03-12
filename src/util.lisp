;;;; util.lisp --- A collection of utility functions 
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file is composed of a collection of utility functions for
;;; plotting. This checks type and shape of input data.


(in-package :cl-user)
(defpackage #:kai.util
  (:use :cl)
  (:export :check-shape-type
           :find-min-max
           :to-array
           :sort-input))
(in-package #:kai.util)


;;;; Type checker
;;;
;;; Here we will accept input data whose type is integer or float.
;;; Input data has to be one-dimensional array or list.

(defun type-check (data)
  (every #'numberp data))


;;;; Sort
;;;
;;; When plotting line, the figure should be sorted.
;;; Here we provide sort function keeping x-value and y-value
;;; with the same index.

(defun sort-array (x-data y-data)
  (let* ((len (car (array-dimensions x-data)))
         (merged-data (loop for i from 0 below len
                            collect (cons (aref x-data i)
                                          (aref y-data i))))
         (sorted-lst (sort (copy-list merged-data)
                           #'< :key #'car))
         (x-axis (make-array len
                             :initial-contents
                             (mapcar #'car sorted-lst)))
         (y-axis (make-array len
                             :initial-contents
                             (mapcar #'cdr sorted-lst))))
    (cons x-axis y-axis)))
