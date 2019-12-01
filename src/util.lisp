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
