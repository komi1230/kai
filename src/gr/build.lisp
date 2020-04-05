;;;; build.lisp --- Donwloader of binaries of GR
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file is composed of download functions, which is branched to 
;;; OS types.
;;;
;;; see: https://github.com/jheinen/GR.jl/blob/master/deps/build.jl

(in-package :cl-user)
(defpackage :kai.gr.build
  (:use :cl)
  (:import-from :kai.util
                :download-file)
  (:import-from :kai.converter
                :make-kai-cache))
(in-package :kai.gr.build)


;;;; Donwload client
;;;
;;; When using GR, some sources is needed.
;;; Here is a set of file donwload client and file checker.

(defun download-gr ()
  (download-file (merge-pathnames "GR"
                                  (make-kai-cache "GR"))
                 "https://cdn.plot.ly/plotly-latest.min.js"))
