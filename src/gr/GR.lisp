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



;; First Setup before launching GR
(defun init ()
  (let ((kai-cache-dir (namestring (make-kai-cache "gr"))))
    ;; Set environment variables
    (setf (uiop:getenv "GRDIR") kai-cache-dir)
    (setf (uiop:getenv "GKS_FONTPATH") kai-cache-dir)
    (setf (uiop:getenv "GKS_USE_CAIRO_PNG") "true")

    ;; Load shared objects
    (cffi:load-foreign-library
     (merge-pathnames (format nil "lib/~A" libGR)
                      (make-kai-cache "gr")))

    ;; Default float number is as Double
    (setf *read-default-float-format* 'double-float)))




;;;; Bindings
;;;
;;; We have to wrap GR functions with CFFI.


;; Initialize GR states

(cffi:defcfun ("gr_initgr" initgr) :void)

(cffi:defcfun ("gr_opengks" opengks) :void)

(cffi:defcfun ("gr_closegks" closegks) :void)



#|
    openws(workstation_id::Int, connection, workstation_type::Int)

Open a graphical workstation.

workstation_id :
    A workstation identifier.

connection :
    A connection identifier.

workstation_type :
    The desired workstation type.

Available workstation types:

    +-------------+------------------------------------------------------+
    |            5|Workstation Independent Segment Storage               |
    +-------------+------------------------------------------------------+
    |         7, 8|Computer Graphics Metafile (CGM binary, clear text)   |
    +-------------+------------------------------------------------------+
    |           41|Windows GDI                                           |
    +-------------+------------------------------------------------------+
    |           51|Mac Quickdraw                                         |
    +-------------+------------------------------------------------------+
    |      61 - 64|PostScript (b/w, color)                               |
    +-------------+------------------------------------------------------+
    |     101, 102|Portable Document Format (plain, compressed)          |
    +-------------+------------------------------------------------------+
    |    210 - 213|X Windows                                             |
    +-------------+------------------------------------------------------+
    |          214|Sun Raster file (RF)                                  |
    +-------------+------------------------------------------------------+
    |     215, 218|Graphics Interchange Format (GIF87, GIF89)            |
    +-------------+------------------------------------------------------+
    |          216|Motif User Interface Language (UIL)                   |
    +-------------+------------------------------------------------------+
    |          320|Windows Bitmap (BMP)                                  |
    +-------------+------------------------------------------------------+
    |          321|JPEG image file                                       |
    +-------------+------------------------------------------------------+
    |          322|Portable Network Graphics file (PNG)                  |
    +-------------+------------------------------------------------------+
    |          323|Tagged Image File Format (TIFF)                       |
    +-------------+------------------------------------------------------+
    |          370|Xfig vector graphics file                             |
    +-------------+------------------------------------------------------+
    |          371|Gtk                                                   |
    +-------------+------------------------------------------------------+
    |          380|wxWidgets                                             |
    +-------------+------------------------------------------------------+
    |          381|Qt4                                                   |
    +-------------+------------------------------------------------------+
    |          382|Scaleable Vector Graphics (SVG)                       |
    +-------------+------------------------------------------------------+
    |          390|Windows Metafile                                      |
    +-------------+------------------------------------------------------+
    |          400|Quartz                                                |
    +-------------+------------------------------------------------------+
    |          410|Socket driver                                         |
    +-------------+------------------------------------------------------+
    |          415|0MQ driver                                            |
    +-------------+------------------------------------------------------+
    |          420|OpenGL                                                |
    +-------------+------------------------------------------------------+
    |          430|HTML5 Canvas                                          |
    +-------------+------------------------------------------------------+
|#

(cffi:defcfun ("gr_openws" openws) :void
  (workstation-id :int)
  (connection (:pointer :char))
  (type :int))


#|
    closews(workstation_id::Int)

Close the specified workstation.

workstation_id :
    A workstation identifier.
|#

(cffi:defcfun ("gr_closews" closews) :void
  (workstation-id :int))


#| 
    activatews(workstation_id::Int)

Activate the specified workstation.

workstation_id :
    A workstation identifier.

|#

(cffi:defcfun ("gr_activatews" activatews) :void
  (workstation-id :int))


#|
    deactivatews(workstation_id::Int)

Deactivate the specified workstation.

`workstation_id` :
    A workstation identifier.
|#

(cffi:defcfun ("gr_deactivatews" deactivatews) :void
  (workstation-id :int))


;; Configure the specified workstation

(cffi:defcfun ("gr_configurews" configurews) :void)


;; Clear the specified workstation
(cffi:defcfun ("gr_clearws" clearws) :void)


;; Update the specified workstation
(cffi:defcfun ("gr_updatews" updatews) :void)




;;;; sample codes



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


