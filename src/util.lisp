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
  (:export :convert-data
           :check-shape-type
           :find-min-max
           :to-array
           :sort-input))
(in-package #:kai.util)


;;;; Input style converter
;;;
;;; When getting input data, we accept variable length args.
;;; We cannot realize to accept one or two args with some options by
;;; standard style, so we papare such a function to convert args.

(defun convert-data (&rest data)
  (let ((x (car data))
        (y (cadr data)))
    (if (or (consp x)        ; check first data
            (vectorp x))
        (if (or (consp y)    ; check second data  
                (vectorp y))
            data
            `(,(loop for i below (length x) collect i)
              ,x
              ,@(cdr data)))
        (error "Invalid input"))))


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
         (merged-data (loop for x across x-data
                            for y across y-data
                            collect (cons x y)))
         (sorted-lst (sort merged-data #'< :key #'car))
         (x-axis (make-array len
                             :initial-contents
                             (mapcar #'car sorted-lst)))
         (y-axis (make-array len
                             :initial-contents
                             (mapcar #'cdr sorted-lst))))
    (cons x-axis y-axis)))


;;;; Symbol Converter
;;;
;;; We will get input data as list.
;;; When using plotly as backend, we have to convert list data to JSON.
;;; In converting list data to JSON, symbols will be converted as capital texts.
;;; Here we provide a function to prevent this.

(defun make-keyword (name)
  (values (intern name "KEYWORD")))

(defun symbol-downcase (data)
  (mapcar #'(lambda (x)
              (if (keywordp x)
                  (make-keyword (string-downcase (symbol-name x)))
                  x))
          data))



;;;; Download
;;;
;;; When setting up, we have to get some files via networks.
;;; Multiple backends need to download resources, so we implement
;;; a download client here.

(defun download-file (filename uri)
  (dex:fetch uri filename
             :if-exists :supersede))


;;;; OS Distribution
;;;
;;; SBCL can get an informatioin about OS, but it is very abstract.
;;; Because we can find whether the OS is Linux, but we cannot find
;;; which distribution the OS is.

(defun redhat-version ()
  (let* ((content (uiop:read-file-lines "/etc/redhat-release"))
         (trim-before (subseq content
                              (+ 8 (search "release" content))))
         (trim-after (subseq trim-before
                             0
                             (search " " trim-before))))
    trim-after))


(defun get-dist (key)
  (if (probe-file #P"/etc/redhat-release")
      "redhat"
      (loop :for file :in (uiop:directory-files "/etc/" "*-release")
            :do (loop :for line :in (uiop:read-file-lines file)
                      :if (uiop:string-prefix-p (format nil "~A=" key) line)
                      :do (return-from get-dist (subseq line (1+ (length key))))))))
