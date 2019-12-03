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
  (:export :check-shape-type-simple
           :check-shape-type-nested))
(in-package #:kai.util)


;;;; Type and shape check
;;; 
;;; The shape of input data should be
;;;
;;;   1) (value1 value2 ...)      : Simple list
;;;
;;;   2) #(value1 value2 ...)     : Simple vector
;;;
;;;   3) (#C(value-r1 value-i1)   : Simple list with
;;;       #C(value-r2 value-i2)     complex number
;;;       ...)
;;;
;;;   4) #(#C(value-r1 value-i1)  : Simple vector with
;;;        #C(value-r2 value-i2)    complex number
;;;        ...)
;;;
;;;   5) ((value-x1 value-y1)     : Nested list
;;;       (value-x2 value-y2)
;;;        ...)
;;;
;;;   6) #(#(value-x1 value-y1)   : Nested vector
;;;        #(value-x2 value-y2)
;;;        ...)
;;;
;;;
;;;
;;; And all of the input data should be integer or float.
;;;


;; Case 1), 2), 3) and 4)
(defun check-shape-type-simple (data)
  (every #'numberp data))

;; Case 5) and 6)
(defun check-shape-type-nested (data)
  (every #'(lambda (x)
             (and (every #'numberp x)   ; check int or float
                  (or (= (length x) 2)  ; check dimension
                      (= (length x) 3))))
         data))




;;;; Find max and min value in input data
;;;
;;; When plotting in the figure, we have to know
;;; max and min value of the input data to calculate and
;;; scale the size of the figure.
;;;
;;; Like the above function (type and shape check), we will
;;; make some functions for each shape of the input data.
;;;
;;; Each finding function returns cons: (MIN . MAX)

;; Case 1)
(defun find-min-max-simple-lst (data)
  (labels ((f (min-value max-value lst)
             (if (null lst)
                 (cons min-value max-value)
                 (if (< max-value (car lst))
                     (f min-value (car lst) (cdr lst))
                     (if (< (car lst) min-value)
                         (f (car lst) max-value (cdr lst))
                         (f min-value max-value (cdr lst)))))))
    (f (car data) (car data) data)))


;; Case 2)
(defun find-min-max-simple-vector (data)
  (let ((min-value (aref data 0))
        (max-value (aref data 0)))
    (loop for i from 0 below (length data) do
         (if (< (aref data i) min-value)
             (setq min-value (aref data i))
             (if (< max-value (aref data i))
                 (setq max-value (aref data i)))))
    (cons min-value max-value)))

;; Case 3)
