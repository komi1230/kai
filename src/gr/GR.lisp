;;;; GR.lisp --- A collection of API for GR
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file is composed of a collection of API for GR. These APIs
;;; are mainly just bindings to gr.h
;;;
;;; see: https://github.com/jheinen/GR.jl/blob/master/src/GR.jl

(in-package :cl-user)
(defpackage :kai.GR.GR
  (:use :cl)
  (:import-from :kai.converter
                :make-kai-cache))
(in-package :kai.GR.GR)


;;;; Setup
;;;
;;; We load some shared files (.dll or .so) to make bindings to
;;; GR API.

;; Shared files
(defparameter libGR
   #+(or win32 mswindows windows) ; Windows
   "libGR.dll"
   #-(or win32 mswindows windows) ; macOS or Linux
   "libGR.so")

(defparameter libGR3
  #+(or win32 mswindows windows) ; Windows
   "libGR3.dll"
   #-(or win32 mswindows windows) ; macOS or Linux
   "libGR3.so")

(defparameter libGRM
  #+(or win32 mswindows windows) ; Windows
   "libGRM.dll"
   #-(or win32 mswindows windows) ; macOS or Linux
   "libGRM.so")

;; Set environment variables
(defun init ()
  (let ((kai-cache-dir (make-kai-cache "gr"))))
  (setf (uiop:getenv "GRDIR") kai-cache-dir)
  (setf (uiop:getenv "GKS_FONTPATH") kai-cache-dir)
  (setf (uiop:getenv "GKS_USE_CAIRO_PNG") "true")
  (setf (uiop:getenv )))



;;;; sample codes

(cffi:load-foreign-library
   ;;(merge-pathnames "gr/lib/libGR.so"
   ;;                 (make-kai-cache "gr"))
 "/Users/komi/.cache/kai/gr/lib/libGR.so")

(defparameter *x*
  (cffi:foreign-alloc :double
                      :initial-contents
                      (list 0.0d0 0.2d0 0.4d0 0.6d0 0.8d0 1.0d0)))
(defparameter *y*
  (cffi:foreign-alloc :double
                      :initial-contents
                      (list 0.3d0 0.5d0 0.4d0 0.2d0 0.6d0 0.7d0)))

(cffi:defcfun ("gr_polyline" polyline) :void
  (n :int)
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("gr_tick" tick) :double
  (a :double)
  (b :double))

(cffi:defcfun ("gr_axes" axes) :void
  (x-tick :double)
  (y-tick :double)
  (x-org :double)
  (y-org :double)
  (major-x :int)
  (major-y :int)
  (tick-size :double))

(cffi:defcfun ("gr_initgr" initgr) :void)

(cffi:defcfun ("gr_opengks" opengks) :void)

(cffi:defcfun ("gr_closegks" closegks) :void)
