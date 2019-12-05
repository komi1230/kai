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
           :to-array))
(in-package #:kai.util)


;;;; Type and shape check
;;; 
;;; The shape of input data should be
;;;
;;;   1) (value1 value2 ...)       : Simple list
;;;
;;;   2) #(value1 value2 ...)      : Simple vector
;;;
;;;   3) (#C(value-r1 value-i1)    : Simple list with
;;;       #C(value-r2 value-i2)      complex number
;;;       ...)
;;;
;;;   4) #(#C(value-r1 value-i1)   : Simple vector with
;;;        #C(value-r2 value-i2)     complex number
;;;        ...)
;;;
;;;   5) ((value-x1 value-y1)      : Nested list
;;;       (value-x2 value-y2)
;;;        ...)
;;;
;;;   6) #(#(value-x1 value-y1)    : Nested vector
;;;        #(value-x2 value-y2)
;;;        ...)
;;;
;;;   7) #{N}A((value-x1 value-y1) : Multi-dimensional
;;;            (value-x2 value-y2)   array
;;;             ...)
;;;
;;;
;;; And all of the input data should be integer or float.
;;;
;;; These accept 3-dimensional input data.


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

;; Case 7)
(defun check-shape-type-multiple (data)
  (let ((shape (array-dimensions data)))
    (if (= (length shape) 2)
        (if (or (= (cadr shape) 2)
                (= (cadr shape) 3))
            (every #'identity
               (loop for i from 0 below (car shape) do
                    (loop for j from 0 below (cadr shape)
                       collect (numberp (aref data i j)))))
            nil)
        nil)))


(defun check-shape-type (data)
  (if (listp data)
      (if (listp (nth 0 data))
          (check-shape-type-nested data)
          (check-shape-type-simple data))
      (if (= (length (array-dimensions data)) 2)
          (check-shape-type-multiple data)
          (if (and (vectorp (aref data 0))
                   (not (stringp (aref data 0))))
              (if (vectorp (aref data 0))
                  (check-shape-type-nested data)
                  (check-shape-type-simple data))
              (error "Invalid input data: ~A~&" data)))))



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
;;;
;;; These accept 3-dimensional input data.

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
(defun find-min-max-simple-array (data)
  (let ((min-value (aref data 0))
        (max-value (aref data 0)))
    (loop for i from 0 below (length data) do
         (if (< (aref data i) min-value)
             (setq min-value (aref data i))
             (if (< max-value (aref data i))
                 (setq max-value (aref data i)))))
    (list min-value max-value)))

;; Case 3)
(defun find-min-max-complex-lst (data)
  (find-min-max-nested-lst (mapcar #'(lambda (x) (list (realpart x)
                                                       (imagpart x)))
                                   data)))

;; Case 4)
(defun find-min-max-complex-array (data)
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
(defun find-min-max-nested-array (data)
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

;; Case 7)
(defun find-min-max-multiple (data)
  (labels ((array-to-list (array)
             (let* ((dimensions (array-dimensions array))
                    (depth (1- (length dimensions)))
                    (indices (make-list (1+ depth) :initial-element 0)))
               (labels ((recurse (n)
                          (loop for j below (nth n dimensions) do
                               (setf (nth n indices) j)
                               collect (if (= n depth)
                                           (apply #'aref array indices)
                                           (recurse (1+ n))))))
                 (recurse 0)))))
    (find-min-max-nested-lst (array-to-list data))))

;; Summarize functions to find min max value.
(defun find-min-max (data)
  (if (listp data)
      (if (listp (nth 0 data))
          (find-min-max-nested-lst data)
          (if (complexp (nth 0 data))
              (find-min-max-complex-lst data)
              (find-min-max-simple-lst data)))
      (if (= (length (array-dimensions data)) 2)
          data
          (if (and (vectorp (aref data 0))
                   (not (stringp (aref data 0))))
              (if (vectorp (aref data 0))
                  (find-min-max-nested-array data)
                  (find-min-max-simple-array data))
              (if (complexp (aref data 0))
                  (find-min-max-complex-array data)
                  (error "Invalid input data: ~A~&" data))))))



;;;; Data Converter
;;;
;;; Before plotting, the input data should be proper shape and type.
;;; These functions makes the data structure of the input data LIST.


;; Case 1)
(defun simple-lst-to-2d-array (data)
  (labels ((f (idx lst)
             (if (null lst)
                 nil
                 (cons (list idx (car lst))
                       (f (1+ idx) (cdr lst))))))
    (make-array (list (length data) 2)
                :initial-contents (f 0 data))))

;; Case 2)
(defun simple-array-to-2d-array (data)
  (simple-lst-to-2d-array (coerce data 'list)))

;; Case 3)
(defun complex-lst-to-2d-array (data)
  (make-array (list (length data) 2)
              :initial-contents (mapcar #'(lambda (x)
                                            (list (realpart x)
                                                  (imagpart x)))
                                        data)))

;; Case 4)
(defun complex-array-to-2d-array (data)
  (complex-lst-to-2d-array (coerce data 'list)))

;; Case 5)
(defun nested-lst-to-array (data)
  (make-array (list (length data) (length (car data)))
              :initial-contents data))

;; Case 6)
(defun nested-array-to-array (data)
  (nested-lst-to-2d-array (mapcar #'(lambda (x) (coerce x 'list))
                                  (coerce data 'list))))


(defun to-array (data)
  (if (listp data)
      (if (listp (nth 0 data))
          (nested-lst-to-array data)
          (if (complexp (nth 0 data))
              (complex-lst-to-2d-array data)
              (simple-lst-to-2d-array data)))
      (if (= (length (array-dimensions data)) 2)
          data
          (if (and (vectorp (aref data 0))
                   (not (stringp (aref data 0))))
              (if (vectorp (aref data 0))
                  (nested-array-to-array data)
                  (simple-array-to-2d-array data))
              (if (complexp (aref data 0))
                  (complex-array-to-2d-array data)
                  (error "Invalid input data: ~A~&" data))))))
              



;;;; Color
;;;
;;; When we plot or paste in the figure, we have to set color.
;;; This function make it easy to set color.

(defun set-color (color)
  (case color
    (:red (gl:color 1 0 0))
    (:green (gl:color 0 1 0))
    (:blue (gl:color 0 0 1))
    (:cyan (gl:color 0 1 1))
    (:magenta (gl:color 1 0 1))
    (:yellow (gl:color 1 1 0))
    (:black (gl:color 0 0 0))
    (t (gl:color 0 0 0))))
