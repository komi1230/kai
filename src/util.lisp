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


;;;; Download
;;;
;;; When setting up, we have to get some files via networks.
;;; Multiple backends need to download resources, so we implement
;;; a download client here.

(defun download-file (filename uri)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede
                   :element-type '(unsigned-byte 8))
    (with-open-stream (input (drakma:http-request uri :want-stream t :connection-timeout nil))
      (loop :for b := (read-byte input nil -1)
            :until (minusp b)
            :do (write-byte b out)))))


;;;; OS Distribution and machine type
;;;
;;; SBCL can get an informatioin about OS, but it is very abstract.
;;; Because we can find whether the OS is Linux, but we cannot find
;;; which distribution the OS is.

(defun split (x str)
  (let ((pos (search x str))
        (size (length x)))
    (if pos
      (cons (subseq str 0 pos)
            (split x (subseq str (+ pos size))))
      (list str))))


(defun redhat-version ()
  (let* ((content (uiop:read-file-lines "/etc/redhat-release"))
         (trim-before (subseq content
                              (+ 8 (search "release" content))))
         (trim-after (subseq trim-before
                             0
                             (search " " trim-before))))
    trim-after))


(defun get-dist (key)
  (loop :for file :in (uiop:directory-files "/etc/" "*-release")
        :do (loop :for line :in (uiop:read-file-lines file)
                  :if (uiop:string-prefix-p (format nil "~A=" key) line)
                  :do (return-from get-dist (subseq line (1+ (length key)))))))


(defun get-os-and-arch ()
  #+(or win32 mswindows windows) ; Windows
  '("windows" )
  #+(or macos darwin) ; macOS
  "darwin"
  #-(or win32 mswindows macos darwin windows) ;Linux
  (get-dist))
