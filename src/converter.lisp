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
(defpackage #:kai.converter
  (:use :cl))
(in-package #:kai.converter)



;;;; JSON File Path Check
;;;
;;; When plotting, we will generate json file from common lisp codes.
;;; Here we check file path.
;;; Check if .cache file exists in the home directory.
;;; And create cache directory for Kai to put .js files.

(defun make-kai-cache ()
  (ensure-directories-exist
   (merge-pathnames ".cache/kai/"
                    (user-homedir-pathname))))

(defun check-file-exist (filename)
  (probe-file (merge-pathnames filename
                               (make-kai-cache))))

;;;; JSON Generator
;;;
;;; 
