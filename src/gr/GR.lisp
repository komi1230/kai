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
                :make-kai-cache)
  (:import-from :kai.util
                :flatten))
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
                      (make-kai-cache "gr")))))




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

workstation_id :
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



#|
    Memory allocation for Array

When drawing graph, we have to provide data as array pointer
to drawing function.

lst : 
    A list containing data to plot
|#

(defun data-alloc (lst type)
  (cffi:foreign-alloc type
                      :initial-contents
                      (mapcar #'(lambda (x)
                                  (case type
                                    (:int (coerce x 'integer))
                                    (:float (coerce x 'single-float))
                                    (:double (coerce x 'double-float))))
                              lst)))

(defun string-alloc (str)
  (cffi:foreign-string-alloc str))


#|
    polyline

Draw a polyline using the current line attributes, starting from the
first data point and ending at the last data point.

x :
    A list containing the X coordinates

y :
    A list containing the Y coordinates

|#

(cffi:defcfun ("gr_polyline" gr-polyline) :void
  (n :int)
  (x (:pointer :double))
  (y (:pointer :double)))

(defun polyline (x y)
  (if (= (length x)
         (length y))
      (gr-polyline (length x)
                   (data-alloc x :double)
                   (data-alloc y :double))
      (error "Invalid input data")))


#|
    polymarker

Draw marker symbols centered at the given data points.

x :
    A list containing the X coordinates

y :
    A list containing the Y coordinates

|#

(cffi:defcfun ("gr_polymarker" gr-polymarker) :void
  (n :int)
  (x (:pointer :double))
  (y (:pointer :double)))

(defun polymarker (x y)
  (if (= (length x)
         (length y))
      (gr-polymarker (length x)
                     (data-alloc x :double)
                     (data-alloc y :double))
      (error "Invalid input data")))


#|
    text(x::Real, y::Real, string)

Draw a text at position `x`, `y` using the current text attributes.

x :
    The X coordinate of starting position of the text string

y :
    The Y coordinate of starting position of the text string

strin` :
    The text to be drawn

The values for x and y are in normalized device coordinates.
The attributes that control the appearance of text are text font and precision,
character expansion factor, character spacing, text color index, character
height, character up vector, text path and text alignment.
|#

(cffi:defcfun ("gr_text" gr-text) :void
  (x :double)
  (y :double)
  (str (:pointer :char)))

(defun text (x y str)
  (gr-text (coerce x 'double-float)
           (coerce y 'double-float)
           (string-alloc str)))

(cffi:defcfun ("gr_inqtext" gr-inqtext) :void
  (x :double)
  (y :double)
  (str (:pointer :char))
  (tbx (:pointer :double))
  (tby (:pointer :double)))

(defun inqtext (x y str)
  (gr-inqtext (coerce x 'double-float)
              (coerce y 'double-float)
              (string-alloc str)
              (data-alloc '(0 0 0 0) :double)
              (data-alloc '(0 0 0 0) :double)))


#|
    fillarea(x, y)

Allows you to specify a polygonal shape of an area to be filled.

x :
    A list containing the X coordinates
y :
    A list containing the Y coordinates

The attributes that control the appearance of fill areas are fill area interior
style, fill area style index and fill area color index.
|#

(cffi:defcfun ("gr_fillarea" gr-fillarea) :void
  (n :int)
  (x (:pointer :double))
  (y (:pointer :double)))

(defun fillarea (x y)
  (if (= (length x)
         (length y))
      (gr-fillarea (length x)
                   (data-alloc x :double)
                   (data-alloc y :double))
      (error "Invalid data inputs")))


#|
    cellarray(xmin::Real, xmax::Real, ymin::Real, ymax::Real, dimx::Int, dimy::Int, color)

Display rasterlike images in a device-independent manner. The cell array
function partitions a rectangle given by two corner points into DIMX X DIMY
cells, each of them colored individually by the corresponding color index
of the given cell array.

xmin, ymin :
    Lower left point of the rectangle

xmax, ymax :
    Upper right point of the rectangle

dimx, dimy :
    X and Y dimension of the color index array

color :
    Color index array

The values for xmin, xmax, ymin and ymax are in world coordinates.
|#

(cffi:defcfun ("gr_cellarray" gr-cellarray) :void
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double)
  (dimx :int)
  (dimy :int)
  (scol :int)
  (srow :int)
  (ncol :int)
  (nrow :int)
  (color (:pointer :int)))

(defun cellarray (xmin xmax ymin ymax dimx dimy color)
  (gr-cellarray xmin
                xmax
                ymin
                ymax
                dimx
                dimy
                1    ; scol
                1    ; srow
                dimx ; ncol
                dimy ; nrow
                (data-alloc (flatten color) :int)))


#|
    nonuniformcellarray(x, y, dimx::Int, dimy::Int, color)

 Display a two dimensional color index array with nonuniform cell sizes.

x, y :
    X and Y coordinates of the cell edges

dimx, dimy :
    X and Y dimension of the color index array

color :
    Color index array

The values for x and y are in world coordinates. x must contain dimx + 1 elements
and y must contain dimy + 1 elements. The elements i and i+1 are respectively the edges
of the i-th cell in X and Y direction.
|#

(cffi:defcfun ("gr_nonuniformcellarray" gr-nonuniformcellarray) :void
  (x (:pointer :double))
  (y (:pointer :double))
  (dimx :int)
  (dimy :int)
  (scol :int)
  (srow :int)
  (ncol :int)
  (nrow :int)
  (color (:pointer :int)))

(defun nonuniformcellarray (x y dimx dimy color)
  (gr-nonuniformcellarray (data-alloc x :double)
                          (data-alloc y :double)
                          dimx
                          dimy
                          1
                          1
                          dimx
                          dimy
                          (data-alloc (flatten color) :int)))


#|
    polarcellarray(xorg::Real, yorg::Real, phimin::Real, phimax::Real, rmin::Real, rmax::Real, imphi::Int, dimr::Int, color)

Display a two dimensional color index array mapped to a disk using polar
coordinates.

xorg :
    X coordinate of the disk center in world coordinates

yorg :
    Y coordinate of the disk center in world coordinates

phimin :
    start angle of the disk sector in degrees

phimax :
    end angle of the disk sector in degrees

rmin :
    inner radius of the punctured disk in world coordinates

rmax :
    outer radius of the punctured disk in world coordinates

dimiphi, dimr :
    Phi (X) and iR (Y) dimension of the color index array

color :
    Color index array

The two dimensional color index array is mapped to the resulting image by
interpreting the X-axis of the array as the angle and the Y-axis as the raidus.
The center point of the resulting disk is located at xorg, yorg and the
radius of the disk is `rmax`.
|#

(cffi:defcfun ("gr_polarcellarray" gr-polarcellarray) :void
  (xorg :double)
  (yorg :double)
  (phimin :double)
  (phimax :double)
  (rmin :double)
  (rmax :double)
  (dimphi :int)
  (dimr :int)
  (scol :int)
  (srow :int)
  (ncol :int)
  (nrow :int)
  (color (:pointer :int)))

(defun polarcellarray (xorg yorg phimin phimax rmin rmax dimphi dimr color)
  (gr-polarcellarray xorg
                     yorg
                     phimin
                     phimax
                     rmin
                     rmax
                     dimphi
                     dimr
                     1
                     1
                     dimphi
                     dimr
                     (data-alloc (flatten color) :int)))



;;;; sample codes



(defparameter *x*
  (cffi:foreign-alloc :double
                      :initial-contents
                      (list 0.0d0 0.2d0 0.4d0 0.6d0 0.8d0 1.0d0)))
(defparameter *y*
  (cffi:foreign-alloc :double
                      :initial-contents
                      (list 0.3d0 0.5d0 0.4d0 0.2d0 0.6d0 0.7d0)))



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


