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
  (assert (= (length x) (length y)))
  (gr-polyline (length x)
               (data-alloc x :double)
               (data-alloc y :double)))


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
  (assert (= (length x) (length y)))
  (gr-polymarker (length x)
                 (data-alloc x :double)
                 (data-alloc y :double)))


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
  (assert (= (length x) (length y)))
  (gr-fillarea (length x)
               (data-alloc x :double)
               (data-alloc y :double)))


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
  (gr-cellarray (coerce xmin 'double-float)
                (coerce xmax 'double-float)
                (coerce ymin 'double-float)
                (coerce ymax 'double-float)
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
  (gr-polarcellarray (coerce xorg 'double-float)
                     (coerce yorg 'double-float)
                     (coerce phimin 'double-float)
                     (coerce phimax 'double-float)
                     (coerce rmin 'double-float)
                     (coerce rmax 'double-float)
                     dimphi
                     dimr
                     1
                     1
                     dimphi
                     dimr
                     (data-alloc (flatten color) :int)))


#|
    gdp(x, y, primid, datrec)

Generates a generalized drawing primitive (GDP) of the type you specify,
using specified points and any additional information contained in a data
record.

x :
    A list containing the X coordinates

y :
    A list containing the Y coordinates

primid :
    Primitive identifier

datrec :
    Primitive data record
|#

(cffi:defcfun ("gr_gdp" gr-gdp) :void
  (n :int)
  (x (:pointer :double))
  (y (:pointer :double))
  (primid :int)
  (ldr :int)
  (datrec (:pointer :int)))

(defun gdp (x y primid datrec)
  (assert (= (length x) (length y)))
  (gr-gdp (length x)
          (data-alloc x :double)
          (data-alloc y :double)
          primid
          (length primid)
          (data-alloc datrec :int)))


#|
    spline(x, y, m, method)

Generate a cubic spline-fit, starting from the first data point and
ending at the last data point.

x :
    A list containing the X coordinates

y :
    A list containing the Y coordinates

m :
    The number of points in the polygon to be drawn (m > len(x))

method :
    The smoothing method

The values for x and y are in world coordinates. The attributes that
control the appearance of a spline-fit are linetype, linewidth and color
index.

If method is > 0, then a generalized cross-validated smoothing spline is calculated.
If method is 0, then an interpolating natural cubic spline is calculated.
If method is < -1, then a cubic B-spline is calculated.
|#

(cffi:defcfun ("gr_spline" gr-spline) :void
  (n :int)
  (px (:pointer :double))
  (py (:pointer :double))
  (m :int)
  (method :int))

(defun spline (x y m method)
  (assert (= (length x) (length y)))
  (gr-spline (length x)
             (data-alloc x :double)
             (data-alloc y :double)
             m
             method))

(cffi:defcfun ("gr_gridit" gr-gridit) :void
  (nd :int)
  (xd (:pointer :double))
  (yd (:pointer :double))
  (zd (:pointer :double))
  (nx :int)
  (ny :int)
  (x (:pointer :double))
  (y (:pointer :double))
  (z (:pointer :double)))

(defun gridit (xd yd zd nx ny)
  (assert (= (length xd) (length yd) (length zd)))
  (gr-gridit (length xd)
             (data-alloc xd :double)
             (data-alloc yd :double)
             (data-alloc zd :double)
             nx
             ny
             (data-alloc (loop for i from 1 to nx
                               collect i)
                         :double)
             (data-alloc (loop for i from 1 to ny
                               collect i)
                         :double)
             (data-alloc (loop for i from 1 to (* nx ny)
                               collect i)
                         :double)))



#|
    setlinetype(style::Int)

Specify the line style for polylines.

style :
    The polyline line style

The available line types are:

    +---------------------------+----+---------------------------------------------------+
    |LINETYPE_SOLID             |   1|Solid line                                         |
    +---------------------------+----+---------------------------------------------------+
    |LINETYPE_DASHED            |   2|Dashed line                                        |
    +---------------------------+----+---------------------------------------------------+
    |LINETYPE_DOTTED            |   3|Dotted line                                        |
    +---------------------------+----+---------------------------------------------------+
    |LINETYPE_DASHED_DOTTED     |   4|Dashed-dotted line                                 |
    +---------------------------+----+---------------------------------------------------+
    |LINETYPE_DASH_2_DOT        |  -1|Sequence of one dash followed by two dots          |
    +---------------------------+----+---------------------------------------------------+
    |LINETYPE_DASH_3_DOT        |  -2|Sequence of one dash followed by three dots        |
    +---------------------------+----+---------------------------------------------------+
    |LINETYPE_LONG_DASH         |  -3|Sequence of long dashes                            |
    +---------------------------+----+---------------------------------------------------+
    |LINETYPE_LONG_SHORT_DASH   |  -4|Sequence of a long dash followed by a short dash   |
    +---------------------------+----+---------------------------------------------------+
    |LINETYPE_SPACED_DASH       |  -5|Sequence of dashes double spaced                   |
    +---------------------------+----+---------------------------------------------------+
    |LINETYPE_SPACED_DOT        |  -6|Sequence of dots double spaced                     |
    +---------------------------+----+---------------------------------------------------+
    |LINETYPE_DOUBLE_DOT        |  -7|Sequence of pairs of dots                          |
    +---------------------------+----+---------------------------------------------------+
    |LINETYPE_TRIPLE_DOT        |  -8|Sequence of groups of three dots                   |
    +---------------------------+----+---------------------------------------------------+

|#

(cffi:defcfun ("gr_setlinetype" gr-setlinetype) :void
  (linetype :int))

(defun setlinetype (linetype)
  (gr-setlinetype linetype))


(cffi:defcfun ("gr_inqlinetype" gr-inqlinetype) :void
  (linetype (:pointer :int)))

(defun inqlinetype (linetype)
  (gr-setinqlinetype (data-alloc linetype :double)))



#|
    setlinewidth(width::Real)

Define the line width of subsequent polyline output primitives.

width :
    The polyline line width scale factor

The line width is calculated as the nominal line width generated
on the workstation multiplied by the line width scale factor.
This value is mapped by the workstation to the nearest available line width.
The default line width is 1.0, or 1 times the line width generated on the graphics device.

|#

(cffi:defcfun ("gr_setlinewidth" gr-setlinewidth) :void
  (width :double))

(defun setlinewidth (width)
  (gr-setlinetype (coerce width 'double-float)))


(cffi:defcfun ("gr_inqlinewidth" gr-inqlinewidth) :void
  (width (:pointer :double)))

(defun inqlinewidth (width)
  (gr-inqlinewidth (data-alloc width :double)))


#|
    setlinecolorind(color::Int)

Define the color of subsequent polyline output primitives.

color :
    The polyline color index (COLOR < 1256)

|#

(cffi:defcfun ("gr_setlinecolorind" gr-setlinecolorind) :void
  (color :int))

(defun setlinecolorind (color)
  (gr-setlinecolorind (color)))


(cffi:defcfun ("gr_inqlinecolorind" gr-inqlinecolorind) :void
  (coli (:pointer :int)))

(defun inqlinecolorind (coli)
  (gr-inqlinecolorind (data-alloc coli :int)))


#|
    setmarkertype(mtype::Int)

Specifiy the marker type for polymarkers.

style :
    The polymarker marker type

The available marker types are:

    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_DOT               |    1|Smallest displayable dot                        |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_PLUS              |    2|Plus sign                                       |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_ASTERISK          |    3|Asterisk                                        |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_CIRCLE            |    4|Hollow circle                                   |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_DIAGONAL_CROSS    |    5|Diagonal cross                                  |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_SOLID_CIRCLE      |   -1|Filled circle                                   |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_TRIANGLE_UP       |   -2|Hollow triangle pointing upward                 |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_SOLID_TRI_UP      |   -3|Filled triangle pointing upward                 |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_TRIANGLE_DOWN     |   -4|Hollow triangle pointing downward               |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_SOLID_TRI_DOWN    |   -5|Filled triangle pointing downward               |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_SQUARE            |   -6|Hollow square                                   |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_SOLID_SQUARE      |   -7|Filled square                                   |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_BOWTIE            |   -8|Hollow bowtie                                   |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_SOLID_BOWTIE      |   -9|Filled bowtie                                   |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_HGLASS            |  -10|Hollow hourglass                                |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_SOLID_HGLASS      |  -11|Filled hourglass                                |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_DIAMOND           |  -12|Hollow diamond                                  |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_SOLID_DIAMOND     |  -13|Filled Diamond                                  |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_STAR              |  -14|Hollow star                                     |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_SOLID_STAR        |  -15|Filled Star                                     |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_TRI_UP_DOWN       |  -16|Hollow triangles pointing up and down overlaid  |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_SOLID_TRI_RIGHT   |  -17|Filled triangle point right                     |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_SOLID_TRI_LEFT    |  -18|Filled triangle pointing left                   |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_HOLLOW PLUS       |  -19|Hollow plus sign                                |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_SOLID PLUS        |  -20|Solid plus sign                                 |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_PENTAGON          |  -21|Pentagon                                        |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_HEXAGON           |  -22|Hexagon                                         |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_HEPTAGON          |  -23|Heptagon                                        |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_OCTAGON           |  -24|Octagon                                         |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_STAR_4            |  -25|4-pointed star                                  |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_STAR_5            |  -26|5-pointed star (pentagram)                      |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_STAR_6            |  -27|6-pointed star (hexagram)                       |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_STAR_7            |  -28|7-pointed star (heptagram)                      |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_STAR_8            |  -29|8-pointed star (octagram)                       |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_VLINE             |  -30|verical line                                    |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_HLINE             |  -31|horizontal line                                 |
    +-----------------------------+-----+------------------------------------------------+
    |MARKERTYPE_OMARK             |  -32|o-mark                                          |
    +-----------------------------+-----+------------------------------------------------+
Polymarkers appear centered over their specified coordinates.

|#

(cffi:defcfun ("gr_setmarkertype" gr-setmarkertype) :void
  (markertype :int))

(defun setmarkertype (markertype)
  (gr-setlinetype markertype))


(cffi:defcfun ("gr_inqmarkertype" gr-inqmarkertype) :void
  (markertype (:pointer :int)))

(defun inqmarkertype (markertype)
  (gr-inqmarkertype (data-alloc markertype :int)))


#|
    setmarkersize(mtype::Real)

Specify the marker size for polymarkers.

size :
    Scale factor applied to the nominal marker size

The polymarker size is calculated as the nominal size generated on the graphics device
multiplied by the marker size scale factor.

|#

(cffi:defcfun ("gr_setmarkersize" gr-setmarkersize) :void
  (markersize :double))

(defun setmarkersize (markersize)
  (gr-setmarkersize (coerce markersize 'double-float)))


(cffi:defcfun ("gr_inqmarkersize" gr-inqmarkersize) :void
  (markersize (:pointer :double)))

(defun inqmarkersize (markersize)
  (gr-inqmarkersize (data-alloc markersize :double)))


#|
    setmarkercolorind(color::Int)

Define the color of subsequent polymarker output primitives.

color :
    The polymarker color index (COLOR < 1256)

|#

(cffi:defcfun ("gr_setmarkercolorind" gr-setmarkercolorind) :void
  (color :int))

(defun setmarkercolorind (color)
  (gr-setmarkercolorind color))


(cffi:defcfun ("gr_inqmarkercolorind" gr-inqmarkercolorind) :void
  (color (:pointer :int)))

(defun inqmarkercolorind (color)
  (gr-inqmarkercolorind (data-alloc color :int)))
