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
           :find-min-max))
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

(defun check-shape-type (data)
  (if (listp data)
      (if (listp (nth 0 data))
          (check-shape-type-nested data)
          (check-shape-type-simple data))
      (if (and (vectorp data) (not (stringp data)))
          (if (vectorp (aref data 0))
              (check-shape-type-nested data)
              (check-shape-type-simple data))
          (error "Invalid input data: ~A~&" data))))



;;;; Find max and min value in input data
;;;
;;; When plotting in the figure, we have to know
;;; max and min value of the input data to calculate and
;;; scale the size of the figure.
;;;
;;; Like the above function (type and shape check), we will
;;; make some functions for each shape of the input data.
;;;
;;; Each finding function returns cons:
;;;     Simple => (MIN . MAX)
;;;     Nested => ((X-MIN Y-MIN) . (X-MAX Y-MAX))
;;;                  or ((X-MIN Y-MIN Z-MIN) . (X-MAX Y-MAX Z-MAX))

;; Case 1)
(defun find-min-max-simple-lst (data)
  (labels ((f (min-value max-value lst)
             (if (null lst)
                 (list min-value max-value)
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
    (list min-value max-value)))

;; Case 3)
(defun find-min-max-simple-complex-lst (data)
  (find-min-max-nested-lst (mapcar #'(lambda (x) (list (realpart x)
                                                       (imagpart x)))
                                   data)))

;; Case 4)
(defun find-min-max-nested-complex-vector (data)
  (find-min-max-nested-lst (coerce (map 'vector
                                        #'(lambda (x) (list (realpart x)
                                                            (imagpart x)))
                                        data)
                                   'list)))

;; Case 5)
(defun find-min-max-nested-lst (data)
  (labels ((replace-small (lst1 lst2)
             (if (null lst1)
                 nil
                 (if (< (car lst1) (car lst2))
                     (cons (car lst1)
                           (replace-small (cdr lst1) (cdr lst2)))
                     (cons (car lst2)
                           (replace-small (cdr lst1) (cdr lst2))))))
           (replace-big (lst1 lst2)
             (if (null lst1)
                 nil
                 (if (< (car lst1) (car lst2))
                     (cons (car lst2)
                           (replace-big (cdr lst1) (cdr lst2)))
                     (cons (car lst1)
                           (replace-big (cdr lst1) (cdr lst2))))))
           (get-big-pair (init lst)
             (if (null lst)
                 init
                 (get-big-pair (replace-big init (car lst))
                               (cdr lst))))
           (get-small-pair (init lst)
             (if (null lst)
                 init
                 (get-small-pair (replace-small init (car lst))
                                 (cdr lst)))))
    (cons (get-small-pair (car data) data)
          (get-big-pair (car data) data))))

;; Case 6)
(defun find-min-max-nested-vector (data)
  (let ((init-small (coerce (aref data 0) 'list))
        (init-big (coerce (aref data 0) 'list)))
    (loop for ith from 0 below (length data) do
         (loop for idx from 0 below (length init-small) do
              (let ((target-elem (aref (aref data ith) idx)))
                (if (< target-elem (nth idx init-small))
                    (setf (nth idx init-small) target-elem)
                    (if (< (nth idx init-big) target-elem)
                        (setf (nth idx init-big) target-elem))))))
    (cons (coerce init-small 'list)
          (coerce init-big 'list))))

;; Summarize functions to find min max value.
(defun find-min-max (data)
  (if (listp data)
      (if (listp (car data))
          (find-min-max-nested-lst data)
          (if (complexp (car data))
              (find-min-max-simple-complex-lst data)
              (find-min-max-simple-lst data)))
      (if (and (vectorp data) (not (stringp data)))
          (if (vectorp (aref data 0))
              (find-min-max-nested-vector data)
              (if (complexp (aref data 0))
                  (find-min-max-nested-complex-vector data)
                  (find-min-max-simple-vector data)))
          (error "Invalid input data: ~a~&" data))))


