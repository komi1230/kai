;;;; cl-gr.lisp --- An interface for GR
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file is a bridge between API of GR and Kai.
;;; ref) https://github.com/JuliaPlots/Plots.jl/blob/master/src/backends/gr.jl

(in-package :cl-user)
(defpackage :kai.GR.cl-gr
  (:use :cl)
  (:import-from :kai.util
                :make-kai-cache)
  (:import-from :kai.GR.GR
                :init
                :openws
                :closews
                :inqdspsize
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


;;;; GR plot type
;;;
;;; GR has many types of plot for line and marker.
;;; And also GR has some choices of fonts.

(defun gr-linetype (ltype)
  (let ((table (list (cons :auto 1)
                     (cons :solid 1)
                     (cons :dash 2)
                     (cons :dot 3)
                     (cons :dashdot 4)
                     (cons :dashdotdot -1))))
    (cdr (assoc ltype table))))


(defun gr-markertype (mtype)
  (let ((table (list (cons :auto 1)
                     (cons :none -1)
                     (cons :circle -1)
                     (cons :rect -7)
                     (cons :diamond -13)
                     (cons :utriangle -3)
                     (cons :dtriangle -5)
                     (cons :ltriangle -18)
                     (cons :rtriangle -17)
                     (cons :pentagon -21)
                     (cons :hexagon -22)
                     (cons :heptagon -23)
                     (cons :octagon -24)
                     (cons :cross 2)
                     (cons :xcross 5)
                     (cons :+ 2)
                     (cons :x 5)
                     (cons :star4 -25)
                     (cons :star5 -26)
                     (cons :star6 -27)
                     (cons :star7 -28)
                     (cons :star8 -29)
                     (cons :vline -30)
                     (cons :hline -31))))
    (cdr (assoc mtype table))))


(defun gr-halign (pos)
  (let ((table (list (cons :left 1)
                     (cons :center 2)
                     (cons :right 3))))
    (cdr (assoc pos table))))


(defun gr-valign (pos)
  (let ((table (list (cons :top 1)
                     (cons :center 3)
                     (cons :bottom 5))))
    (cdr (assoc pos table))))


(defun gr-font-family (font-type)
  (let ((table (list (cons :times 1)
                     (cons :helvetica 5)
                     (cons :courier 9)
                     (cons :bookman 14)
                     (cons :newcenturyschlbk 18)
                     (cons :avantgarde 22)
                     (cons :palatino 26))))
    (cdr (assoc font-type table))))


(defun gr-vector-font (font-type)
  (let ((table (list (cons :serif-roman 232)
                     (cons :sans-serif 233))))
    (cdr (assoc font-type table))))


;;;; Color
;;;
;;; GR designates color not as RGB but as color index.
;;; This color index is set by INQCOLORFROMRGB.
;;; When we input some RGB to this function, this returns
;;; color index.
;;; When we set some color of line or marker, we use this system.
;;; Note: "ind" means "index."

;; get color index from RGB and Alpha
(defun gr-getcolorind (color-rgb &key (alpha 1))
  (settransparency alpha)
  (inqcolorfromrgb (first color-rgb)
                   (second color-rgb)
                   (third color-rgb)))

(defun linecolor (color-rgb)
  (setlinecolorind (gr-getcolorind color-rgb)))

(defun markercolor (color-rgb)
  (setmarkercolorind (gr-getcolorind color-rgb)))

(defun fillcolor (color-rgb)
  (setfillcolorind (gr-getcolorind color-rgb)))

(defun bordercolor (color-rgb)
  (setbordercolorind (gr-getcolorind color-rgb)))

(defun textcolor (color-rgb)
  (settextcolorind (gr-getcolorind color-rgb)))


;; Transparency
(defun transparency (alpha)
  (let ((a (if (cond
                 ((< alpha 0) 0)
                 ((> alpha 1) 1)
                 (t alpha)))))
    (settransparency alpha)))




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




;;;; Check window information
;;;
;;; Here we check environment on window information;
;;; width, height, DPI (Dots per inch)
;;; Note: 0.0254 is some coefficient to calculate DPI.

(defun window-info ()
  (let* ((win-info (inqdspsize)))
    (list (cons :mwidth (first win-info))
          (cons :mheight (second win-info))
          (cons :width (third win-info))
          (cons :height (fourth win-info))
          (cons :dpi (* 0.0254 (/ width mwidth))))))


