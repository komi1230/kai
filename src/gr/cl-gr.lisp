;;;; cl-gr.lisp --- An interface for GR
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file is a bridge between API of GR and Kai.

(in-package :cl-user)
(defpackage :kai.GR.cl-gr
  (:use :cl)
  (:import-from :kai.util
                :make-kai-cache)
  (:import-from :kai.GR.GR
                :init
                :openws
                :closews
                :activatews
                :deactivatews
                :polyline
                :polymarker
                :text
                :inqtext
                :fillarea
                :cellarray
                :nonuniformcellarray
                :polarcellarray
                :gdp
                :spline
                :gridit
                :setlinetype
                :inqlinetype
                :setlinewidth
                :inqlinewidth
                :setlinecolorind
                :inqlinecolorind
                :setmarkertype
                :inqmarkertype
                :setmarkersize
                :inqmarkersize
                :setmarkercolorind
                :inqmarkercolorind
                :settextfontprec
                :setcharexpan
                :setcharspace
                :settextcolorind
                :inqtextcolorind
                :setcharheight
                :inqcharheight
                :setcharup
                :settextpath
                :settextalign
                :setfillinstyle
                :inqfillinstyle
                :setfillstyle
                :inqfillstyle
                :setfillcolorind
                :inqfillcolorind
                :setcolorrep
                :setwindow
                :inqwindow
                :setviewport
                :inqviewport
                :selntran
                :setclip
                :setwswindow
                :setwsviewport
                :createseg
                :copysegws
                :redrawsegws
                :setsegtran
                :closeseg
                :emergencyclosegks
                :updategks
                :setspace
                :inqspace
                :setscale
                :inqscale
                :textext
                :inqtextext
                :axes
                :axeslbl
                :grid
                :grid3d
                :verrorbars
                :herrorbars
                :polyline3d
                :polymarker3d
                :axes3d
                :titles3d
                :surface
                :contour
                :contourf
                :tricontour
                :hexbin
                :setcolormap
                :inqcolormap
                :setcolormapfromrgb
                :colorbar
                :inqcolor
                :inqcolorfromrgb
                :hsvtorgb
                :tick
                :validaterange
                :adjustlimits
                :adjustrange
                :beginprint
                :beginprinttext
                :endprint
                :ndctowc
                :wctondc
                :wc3towc
                :drawrect
                :fillrect
                :drawarc
                :fillarc
                :drawpath
                :setarrowstyle
                :setarrowsize
                :drawarrow
                :readimage
                :drawimage
                :importgraphics
                :setshadow
                :settransparency
                :setcoordxform
                :begingraphics
                :endgraphics
                :getgraphics
                :drawgraphics
                :mathtex
                :inqmathtex
                :setregenflags
                :inqregenflags
                :savestate
                :restorestate
                :selectcontext
                :destroycontext
                :uselinespec
                :delaunay
                :reducepoints
                :trisurface
                :gradient
                :quiver
                :interp2
                :version
                :shade
                :shadepoints
                :shadelines
                :panzoom
                :path
                :setborderwidth
                :setbordercolorind
                :setprojectiontype
                :setperspectiveprojection
                :settransformationparameters
                :setorthographicprojection
                :setwindow3d)
  (:export :init))
(in-package :kai.GR.cl-gr)



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
                      (make-kai-cache "gr")))))



