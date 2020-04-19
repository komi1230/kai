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

(defun free (&rest vars)
  (loop for var in vars
        do (cffi:foreign-free var)))

(defun string-alloc (str)
  (cffi:foreign-string-alloc str))

(defun arr-aref (arr type index)
  (cffi:mem-aref arr type index))





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
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double)))
    (gr-polyline (length x)
                 x-data
                 y-data)
    (free x-data
          y-data)))


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
  (let ((x-data (data-alloc x :dobule))
        (y-data (data-alloc y :double)))
    (gr-polymarker (length x)
                   x-data
                   y-data)
    (free x-data)
    (free y-data)))


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
  (let ((str-data (string-alloc str)))
    (gr-text (coerce x 'double-float)
             (coerce y 'double-float)
             (string-alloc str))
    (free str-data)))

(cffi:defcfun ("gr_inqtext" gr-inqtext) :void
  (x :double)
  (y :double)
  (str (:pointer :char))
  (tbx (:pointer :double))
  (tby (:pointer :double)))

(defun inqtext (x y str)
  (let ((str-data (string-alloc str))
        (tbx (data-alloc '(0 0 0 0) :double))
        (tby (data-alloc '(0 0 0 0) :double)))
    (gr-inqtext (coerce x 'double-float)
                (coerce y 'double-float)
                str-data
                tbx
                tby)
    (free str-data)
    (list tbx tby)))


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
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double)))
    (gr-fillarea (length x)
                 (data-alloc x :double)
                 (data-alloc y :double))
    (free x-data
          y-data)))


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
  (let ((color-data (data-alloc (flatten color) :int)))
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
                  color-data)
    (free color-data)))


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
  (let ((color-data (data-alloc (flatten color) :int)))
    (gr-nonuniformcellarray (data-alloc x :double)
                            (data-alloc y :double)
                            dimx
                            dimy
                            1
                            1
                            dimx
                            dimy
                            color-data)
    (free color-data)))


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
  (let ((color-data (data-alloc (flatten color) :int)))
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
                       color-data)
    (free color-data)))


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
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double))
        (datrec-data (data-alloc datrec :int)))
    (gr-gdp (length x)
            x-data
            y-data
            primid
            (length primid)
            datrec-data)
    (free x-data
          y-data
          datrec-data)))


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
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double)))
    (gr-spline (length x)
               x-data
               y-data
               m
               method)
    (free x-data
          y-data)))

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
  (let ((xd-data (data-alloc xd :double))
        (yd-data (data-alloc yd :double))
        (zd-data (data-alloc zd :double))
        (x-data (data-alloc (loop for i from 1 to nx
                                  collect i)
                            :double))
        (y-data (data-alloc (loop for i from 1 to ny
                                  collect i)
                            :double))
        (z-data (data-alloc (loop for i from 1 to (* nx ny)
                                  collect i)
                            :double)))
    (gr-gridit (length xd)
               xd-data
               yd-data
               zd-data
               nx
               ny
               x-data
               y-data
               z-data)
    (free xd-data
          yd-data
          zd-data
          x-data
          y-data
          z-data)))


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
  (let ((linetype-data (data-alloc linetype :double)))
    (gr-setinqlinetype linetype-data)
    (free linetype-data)))



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
  (let ((width-data (data-alloc width :double)))
    (gr-inqlinewidth width-data)
    (free width-data)))


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
  (let ((coli-data (data-alloc coli :int)))
    (gr-inqlinecolorind coli-data)
    (free coli-data)))


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
  (let ((markertype-data (data-alloc markertype :int)))
    (gr-inqmarkertype markertype-data)
    (free markertype-data)))


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
  (let ((markersize-data (data-alloc markersize :double)))
    (gr-inqmarkersize markersize-data)
    (free markersize-data)))


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
  (let ((color-data (data-alloc color :int)))
    (gr-inqmarkercolorind color-data)
    (free color-data)))



#|
    settextfontprec(font::Int, precision::Int)

Specify the text font and precision for subsequent text output primitives.

font :
    Text font (see tables below)

precision :
    Text precision (see table below)

The available text fonts are:

    +--------------------------------------+-----+
    |FONT_TIMES_ROMAN                      |  101|
    +--------------------------------------+-----+
    |FONT_TIMES_ITALIC                     |  102|
    +--------------------------------------+-----+
    |FONT_TIMES_BOLD                       |  103|
    +--------------------------------------+-----+
    |FONT_TIMES_BOLDITALIC                 |  104|
    +--------------------------------------+-----+
    |FONT_HELVETICA                        |  105|
    +--------------------------------------+-----+
    |FONT_HELVETICA_OBLIQUE                |  106|
    +--------------------------------------+-----+
    |FONT_HELVETICA_BOLD                   |  107|
    +--------------------------------------+-----+
    |FONT_HELVETICA_BOLDOBLIQUE            |  108|
    +--------------------------------------+-----+
    |FONT_COURIER                          |  109|
    +--------------------------------------+-----+
    |FONT_COURIER_OBLIQUE                  |  110|
    +--------------------------------------+-----+
    |FONT_COURIER_BOLD                     |  111|
    +--------------------------------------+-----+
    |FONT_COURIER_BOLDOBLIQUE              |  112|
    +--------------------------------------+-----+
    |FONT_SYMBOL                           |  113|
    +--------------------------------------+-----+
    |FONT_BOOKMAN_LIGHT                    |  114|
    +--------------------------------------+-----+
    |FONT_BOOKMAN_LIGHTITALIC              |  115|
    +--------------------------------------+-----+
    |FONT_BOOKMAN_DEMI                     |  116|
    +--------------------------------------+-----+
    |FONT_BOOKMAN_DEMIITALIC               |  117|
    +--------------------------------------+-----+
    |FONT_NEWCENTURYSCHLBK_ROMAN           |  118|
    +--------------------------------------+-----+
    |FONT_NEWCENTURYSCHLBK_ITALIC          |  119|
    +--------------------------------------+-----+
    |FONT_NEWCENTURYSCHLBK_BOLD            |  120|
    +--------------------------------------+-----+
    |FONT_NEWCENTURYSCHLBK_BOLDITALIC      |  121|
    +--------------------------------------+-----+
    |FONT_AVANTGARDE_BOOK                  |  122|
    +--------------------------------------+-----+
    |FONT_AVANTGARDE_BOOKOBLIQUE           |  123|
    +--------------------------------------+-----+
    |FONT_AVANTGARDE_DEMI                  |  124|
    +--------------------------------------+-----+
    |FONT_AVANTGARDE_DEMIOBLIQUE           |  125|
    +--------------------------------------+-----+
    |FONT_PALATINO_ROMAN                   |  126|
    +--------------------------------------+-----+
    |FONT_PALATINO_ITALIC                  |  127|
    +--------------------------------------+-----+
    |FONT_PALATINO_BOLD                    |  128|
    +--------------------------------------+-----+
    |FONT_PALATINO_BOLDITALIC              |  129|
    +--------------------------------------+-----+
    |FONT_ZAPFCHANCERY_MEDIUMITALIC        |  130|
    +--------------------------------------+-----+
    |FONT_ZAPFDINGBATS                     |  131|
    +--------------------------------------+-----+

The available text precisions are:

    +---------------------------+---+--------------------------------------+
    |TEXT_PRECISION_STRING      |  0|String precision (higher quality)     |
    +---------------------------+---+--------------------------------------+
    |TEXT_PRECISION_CHAR        |  1|Character precision (medium quality)  |
    +---------------------------+---+--------------------------------------+
    |TEXT_PRECISION_STROKE      |  2|Stroke precision (lower quality)      |
    +---------------------------+---+--------------------------------------+

The appearance of a font depends on the text precision value specified.
STRING, CHARACTER or STROKE precision allows for a greater or lesser
realization of the text primitives, for efficiency. STRING is the default
precision for GR and produces the highest quality output.

|#

(cffi:defcfun ("gr_settextfontprec" gr-settextfontprec) :void
  (font :int)
  (precision :int))

(defun settextfontprec (font precision)
  (gr-settextfontprec font precision))


#|
    setcharexpan(factor::Real)

Set the current character expansion factor (width to height ratio).

factor :
    Text expansion factor applied to the nominal text width-to-height ratio

setcharexpan defines the width of subsequent text output primitives. The expansion
factor alters the width of the generated characters, but not their height. The default
text expansion factor is 1, or one times the normal width-to-height ratio of the text.

|#

(cffi:defcfun ("gr_setcharexpan" gr-setcharexpan) :void
  (factor :double))

(defun setcharexpan (factor)
  (gr-setcharexpan (coerce factor 'double-float)))


(cffi:defcfun ("gr_setcharspace" gr-setcharspace) :void
  (spacing :double))

(defun setcharspace (spacing)
  (gr-setcharspace (double spaced 'double-float)))


#|
    settextcolorind(color::Int)

Sets the current text color index.

color :
    The text color index (COLOR < 1256)

settextcolorind defines the color of subsequent text output primitives.
GR uses the default foreground color (black=1) for the default text color index.

|#

(cffi:defcfun ("gr_settextcolorind" gr-settextcolorind) :void
  (color :int))

(defun settextcolorind (color)
  (gr-settextcolorind color))


(cffi:defcfun ("gr_inqtextcolorind" gr-inqtextcolorind) :void
  (color (:pointer :int)))

(defun inqtextcolorind (color)
  (let ((color-data (data-alloc color :int)))
    (gr-inqtextcolorind color-data)
    (free color-data)))


#|
    setcharheight(height::Real)

Set the current character height.

height :
    Text height value

setcharheight defines the height of subsequent text output primitives. Text height
is defined as a percentage of the default window. GR uses the default text height of
0.027 (2.7% of the height of the default window).

|#

(cffi:defcfun ("gr_setcharheight" gr-setcharheight) :void
  (height :double))

(defun setcharheight (height)
  (gr-setcharheight (coerce height 'double-float)))


(cffi:defcfun ("gr_inqcharheight" gr-inqcharheight) :void
  (height (:pointer :double)))

(defun inqcharheight (height)
  (let ((height-data (data-alloc height :double)))
    (gr-inqcharheight height-data)
    (free height-data)))


#|
    setcharup(ux::Real, uy::Real)

Set the current character text angle up vector.

ux, uy :
    Text up vector

setcharup defines the vertical rotation of subsequent text output primitives.
The text up vector is initially set to (0, 1), horizontal to the baseline.

|#

(cffi:defcfun ("gr_setcharup" gr-setcharup) :void
  (ux :double)
  (uy :double))

(defun setcharup (ux uy)
  (gr-setcharup (coerce ux 'double-float)
                (coerce uy 'double-float)))


#|
    settextpath(path::Int)

Define the current direction in which subsequent text will be drawn.

path :
    Text path (see table below)

    +----------------------+---+---------------+
    |TEXT_PATH_RIGHT       |  0|left-to-right  |
    +----------------------+---+---------------+
    |TEXT_PATH_LEFT        |  1|right-to-left  |
    +----------------------+---+---------------+
    |TEXT_PATH_UP          |  2|downside-up    |
    +----------------------+---+---------------+
    |TEXT_PATH_DOWN        |  3|upside-down    |
    +----------------------+---+---------------+
|#

(cffi:defcfun ("gr_settextpath" gr-settextpath) :void
  (path :int))

(defun settextpath (path)
  (gr-settextpath path))


#|
    settextalign(horizontal::Int, vertical::Int)

Set the current horizontal and vertical alignment for text.

horizontal :
    Horizontal text alignment (see the table below)

vertical :
    Vertical text alignment (see the table below)

settextalign specifies how the characters in a text primitive will be aligned
in horizontal and vertical space. The default text alignment indicates horizontal left
alignment and vertical baseline alignment.

    +-------------------------+---+----------------+
    |TEXT_HALIGN_NORMAL       |  0|                |
    +-------------------------+---+----------------+
    |TEXT_HALIGN_LEFT         |  1|Left justify    |
    +-------------------------+---+----------------+
    |TEXT_HALIGN_CENTER       |  2|Center justify  |
    +-------------------------+---+----------------+
    |TEXT_HALIGN_RIGHT        |  3|Right justify   |
    +-------------------------+---+----------------+
    +-------------------------+---+------------------------------------------------+
    |TEXT_VALIGN_NORMAL       |  0|                                                |
    +-------------------------+---+------------------------------------------------+
    |TEXT_VALIGN_TOP          |  1|Align with the top of the characters            |
    +-------------------------+---+------------------------------------------------+
    |TEXT_VALIGN_CAP          |  2|Aligned with the cap of the characters          |
    +-------------------------+---+------------------------------------------------+
    |TEXT_VALIGN_HALF         |  3|Aligned with the half line of the characters    |
    +-------------------------+---+------------------------------------------------+
    |TEXT_VALIGN_BASE         |  4|Aligned with the base line of the characters    |
    +-------------------------+---+------------------------------------------------+
    |TEXT_VALIGN_BOTTOM       |  5|Aligned with the bottom line of the characters  |
    +-------------------------+---+------------------------------------------------+
|#

(cffi:defcfun ("gr_settextalign" gr-settextalign) :void
  (horizontal :int)
  (vertical :int))

(defun settextalign (horizontal vertical)
  (gr-settextalign horizontal vertical))


#|
    setfillintstyle(style::Int)


Set the fill area interior style to be used for fill areas.

style :
    The style of fill to be used

setfillintstyle defines the interior style  for subsequent fill area output
primitives. The default interior style is HOLLOW.

    +---------+---+--------------------------------------------------------------------------------+
    |HOLLOW   |  0|No filling. Just draw the bounding polyline                                     |
    +---------+---+--------------------------------------------------------------------------------+
    |SOLID    |  1|Fill the interior of the polygon using the fill color index                     |
    +---------+---+--------------------------------------------------------------------------------+
    |PATTERN  |  2|Fill the interior of the polygon using the style index as a pattern index       |
    +---------+---+--------------------------------------------------------------------------------+
    |HATCH    |  3|Fill the interior of the polygon using the style index as a cross-hatched style |
    +---------+---+--------------------------------------------------------------------------------+

|#

(cffi:defcfun ("gr_setfillinstyle" gr-setfillinstyle) :void
  (style :int))

(defun setfillinstyle (style)
  (gr-setfillinstyle style))


(cffi:defcfun ("gr_inqfillinstyle" gr-inqfillinstyle) :void
  (style (:pointer :int)))

(defun inqfillinstyle (style)
  (let ((style-data (data-alloc style :int)))
    (gr-inqfillinstyle style-data)
    (free style-data)))


#|
    setfillstyle(index::Int)

Sets the fill style to be used for subsequent fill areas.

index :
    The fill style index to be used

setfillstyle specifies an index when PATTERN fill or HATCH fill is requested by the
setfillintstyle function. If the interior style is set to PATTERN, the fill style
index points to a device-independent pattern table. If interior style is set to HATCH
the fill style index indicates different hatch styles. If HOLLOW or SOLID is specified
for the interior style, the fill style index is unused.

|#

(cffi:defcfun ("gr_setfillstyle" gr-setfillstyle) :void
  (index :int))

(defun setfillstyle (index)
  (gr-setfillstyle index))


(cffi:defcfun ("gr_inqfillstyle" gr-inqfillstyle) :void
  (index (:pointer :int)))

(defun inqfillstyle (index)
  (let ((index-data (data-alloc index :int)))
    (gr-inqfillstyle index-data)
    (free index-data)))


#|
    setfillcolorind(color::Int)

Sets the current fill area color index.

color :
    The fill area color index (COLOR < 1256)

setfillcolorind defines the color of subsequent fill area output primitives.
GR uses the default foreground color (black=1) for the default fill area color index.

|#

(cffi:defcfun ("gr_setfillcolorind" gr-setfillcolorind) :void
  (color :int))

(defun setfillcolorind (color)
  (gr-setfillcolorind color))


(cffi:defcfun ("gr_inqfillcolorind" gr-inqfillcolorind) :void
  (color (:pointer :int)))

(defun inqfillcolorind (color)
  (let ((color-data (data-alloc color :int)))
    (gr-inqfillcolorind color-data)
    (free color-data)))


#|
    setcolorrep(index::Int, red::Real, green::Real, blue::Real)

`setcolorrep` allows to redefine an existing color index representation by specifying
an RGB color triplet.

index :
    Color index in the range 0 to 1256

red :
    Red intensity in the range 0.0 to 1.0

green :
    Green intensity in the range 0.0 to 1.0

blue:
    Blue intensity in the range 0.0 to 1.0

|#

(cffi:defcfun ("gr_setcolorrep" gr-setcolorrep) :void
  (index :int)
  (red :double)
  (green :double)
  (blue :double))

(defun setcolorrep (index red green blue)
  (gr-setcolorrep index
                  (coerce red 'double-float)
                  (coerce green 'double-float)
                  (coerce blue 'double-float)))


#|
    setwindow(xmin::Real, xmax::Real, ymin::Real, ymax::Real)

setwindow establishes a window, or rectangular subspace, of world coordinates to be
plotted. If you desire log scaling or mirror-imaging of axes, use the SETSCALE function.

xmin :
    The left horizontal coordinate of the window (`xmin` < `xmax`).

xmax :
    The right horizontal coordinate of the window.

ymin :
    The bottom vertical coordinate of the window (`ymin` < `ymax`).

ymax :
    The top vertical coordinate of the window.

setwindow defines the rectangular portion of the World Coordinate space (WC) to be
associated with the specified normalization transformation. The WC window and the
Normalized Device Coordinates (NDC) viewport define the normalization transformation
through which all output primitives are mapped. The WC window is mapped onto the
rectangular NDC viewport which is, in turn, mapped onto the display surface of the
open and active workstation, in device coordinates. By default, GR uses the range
[0,1] x [0,1], in world coordinates, as the normalization transformation window.

|#

(cffi:defcfun ("gr_setwindow" gr-setwindow) :void
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double))

(defun setwindow (xmin xmax ymin ymax)
  (gr-setwindow (coerce xmin 'double-float)
                (coerce xmax 'double-float)
                (coerce ymin 'double-float)
                (coerce ymax 'double-float)))


(cffi:defcfun ("gr_inqwindow" gr-inqwindow) :void
  (xmin (:pointer :double))
  (xmax (:pointer :double))
  (ymin (:pointer :double))
  (ymax (:pointer :double)))

(defun inqwindow ()
  (let ((xmin (data-alloc '(0) :double))
        (xmax (data-alloc '(0) :doubl))
        (ymin (data-alloc '(0) :doubl))
        (ymax (data-alloc '(0) :doubl)))
    (gr-inqwindow xmin xmax ymin ymax)
    (list (arr-aref xmin :double 0)
          (arr-aref xmax :double 0)
          (arr-aref ymin :double 0)
          (arr-aref ymax :double 0))))


#|
    setviewport(xmin::Real, xmax::Real, ymin::Real, ymax::Real)

setviewport establishes a rectangular subspace of normalized device coordinates.

xmin :
    The left horizontal coordinate of the viewport.

xmax :
    The right horizontal coordinate of the viewport (0 <= xmin < xmax <= 1).

ymin :
    The bottom vertical coordinate of the viewport.

ymax :
    The top vertical coordinate of the viewport (0 <= ymin < ymax <= 1).

setviewport defines the rectangular portion of the Normalized Device Coordinate
(NDC) space to be associated with the specified normalization transformation. The
NDC viewport and World Coordinate (WC) window define the normalization transformation
through which all output primitives pass. The WC window is mapped onto the rectangular
NDC viewport which is, in turn, mapped onto the display surface of the open and active
workstation, in device coordinates.

|#

(cffi:defcfun ("gr_setviewport" gr-setviewport) :void
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double))

(defun setviewport (xmin xmax ymin ymax)
  (gr-setviewport (coerce xmin 'double-float)
                  (coerce xmax 'double-float)
                  (coerce ymin 'double-float)
                  (coerce ymax 'double-float)))


(cffi:defcfun ("gr_inqviewport" gr-inqviewport) :void
  (xmin (:pointer :double))
  (xmax (:pointer :double))
  (ymin (:pointer :double))
  (ymax (:pointer :double)))

(defun inqviewport ()
  (let ((xmin (data-alloc '(0) :double))
        (xmax (data-alloc '(0) :double))
        (ymin (data-alloc '(0) :double))
        (ymax (data-alloc '(0) :double)))
    (gr-inqviewport xmin xmax ymin ymax)
    (list (arr-aref xmin :double 0)
          (arr-aref xmax :double 0)
          (arr-aref ymin :double 0)
          (arr-aref ymax :double 0))))


#|
    selntran(transform::Int)

selntran selects a predefined transformation from world coordinates to normalized
device coordinates.

transform :
    A normalization transformation number.

    +------+----------------------------------------------------------------------------------------------------+
    |     0|Selects the identity transformation in which both the window and viewport have the range of 0 to 1  |
    +------+----------------------------------------------------------------------------------------------------+
    |  >= 1|Selects a normalization transformation as defined by setwindow and setviewport                      |
    +------+----------------------------------------------------------------------------------------------------+

|#

(cffi:defcfun ("gr_selntran" gr-selntran) :void
  (transform :int))

(defun selntran (transform)
  (gr-selntran transform))


#|
    setclip(indicator::Int)

Set the clipping indicator.

indicator :
    An indicator specifying whether clipping is on or off.

    +----+---------------------------------------------------------------+
    |   0|Clipping is off. Data outside of the window will be drawn.     |
    +----+---------------------------------------------------------------+
    |   1|Clipping is on. Data outside of the window will not be drawn.  |
    +----+---------------------------------------------------------------+

setclip enables or disables clipping of the image drawn in the current window.
Clipping is defined as the removal of those portions of the graph that lie outside of
the defined viewport. If clipping is on, GR does not draw generated output primitives
past the viewport boundaries. If clipping is off, primitives may exceed the viewport
boundaries, and they will be drawn to the edge of the workstation window.
By default, clipping is on.

|#

(cffi:defcfun ("gr_setclip" gr-setclip) :void
  (indicator :int))

(defun setclip (indicator)
  (gr-setclip indicator))


#|
    setwswindow(xmin::Real, xmax::Real, ymin::Real, ymax::Real)

Set the area of the NDC viewport that is to be drawn in the workstation window.

xmin :
    The left horizontal coordinate of the workstation window.

xmax :
    The right horizontal coordinate of the workstation window (0 <= `xmin` < `xmax` <= 1).

ymin :
    The bottom vertical coordinate of the workstation window.

ymax :
    The top vertical coordinate of the workstation window (0 <= ymin < ymax <= 1).

setwswindow defines the rectangular area of the Normalized Device Coordinate space
to be output to the device. By default, the workstation transformation will map the
range [0,1] x [0,1] in NDC onto the largest square on the workstation’s display
surface. The aspect ratio of the workstation window is maintained at 1 to 1.

|#

(cffi:defcfun ("gr_setwswindow" gr-setwswindow) :void
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double))

(defun setwswindow (xmin xmax ymin ymax)
  (gr-setwswindow (coerce xmin 'double-float)
                  (coerce xmax 'double-float)
                  (coerce ymin 'double-float)
                  (coerce ymax 'double-float)))


#|
    setwsviewport(xmin::Real, xmax::Real, ymin::Real, ymax::Real)

Define the size of the workstation graphics window in meters.

xmin :
    The left horizontal coordinate of the workstation viewport.

xmax :
    The right horizontal coordinate of the workstation viewport.

ymin :
    The bottom vertical coordinate of the workstation viewport.

ymax :
    The top vertical coordinate of the workstation viewport.

setwsviewport places a workstation window on the display of the specified size in
meters. This command allows the workstation window to be accurately sized for a
display or hardcopy device, and is often useful for sizing graphs for desktop
publishing applications.

|#

(cffi:defcfun ("gr_setwsviewport" gr-setwsviewport) :void
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double))

(defun setwsviewport (xmin xmax ymin ymax)
  (gr-setwsviewport (coerce xmin 'double-float)
                    (coerce xmax 'double-float)
                    (coerce ymin 'double-float)
                    (coerce ymax 'double-float)))


(cffi:defcfun ("gr_createseg" gr-createseg) :void
  (segment :int))

(defun createseg (segment)
  (gr-createseg segment))


(cffi:defcfun ("gr_copysegws" gr-copysegws) :void
  (segment :int))

(defun copysegws (segment)
  (gr-copysegws segment))


(cffi:defcfun ("gr_redrawsegws" gr-redrawsegws) :void)

(defun redrawsegws ()
  (gr-redrawsegws))


(cffi:defcfun ("gr_setsegtran" gr-setsegtran) :void
  (segment :int)
  (fx :double)
  (fy :double)
  (transx :double)
  (transy :double)
  (phi :double)
  (scalex :double)
  (scaley :double))

(defun setsegtran (segment fx fy transx transy phi scalex scaley)
  (gr-setsegtran segment
                 (coerce fx 'double-float)
                 (coerce fy 'double-float)
                 (coerce transx 'double-float)
                 (coerce transy 'double-float)
                 (coerce phi 'double-float)
                 (coerce scalex 'double-float)
                 (coerce scaley 'double-float)))


(cffi:defcfun ("gr_closeseg" gr-closeseg) :void)

(defun closeseg ()
  (gr-closeseg))


(cffi:defcfun ("gr_emergencycloseseg" gr-emergencycloseseg) :void)

(defun emergencycloseseg ()
  (gr-emergencycloseseg))


(cffi:defcfun ("gr_updategks" gr-updategks) :void)

(defun updategks ()
  (gr-updategks))


#|
    setspace(zmin::Real, zmax::Real, rotation::Int, tilt::Int)

Set the abstract Z-space used for mapping three-dimensional output primitives into
the current world coordinate space.

zmin :
    Minimum value for the Z-axis.

zmax :
    Maximum value for the Z-axis.

rotation :
    Angle for the rotation of the X axis, in degrees.

tilt :
    Viewing angle of the Z axis in degrees.

setspace establishes the limits of an abstract Z-axis and defines the angles for
rotation and for the viewing angle (tilt) of a simulated three-dimensional graph,
used for mapping corresponding output primitives into the current window.
These settings are used for all subsequent three-dimensional output primitives until
other values are specified. Angles of rotation and viewing angle must be specified
between 0° and 90°.
|#

(cffi:defcfun ("gr_setspace" gr-setspace) :int
  (zmin :double)
  (zmax :double)
  (rotation :int)
  (tilt :int))

(defun setspace (zmin zmax rotation tilt)
  (gr-setspace (coerce zmin 'double-float)
               (coerce zmax 'double-float)
               rotation
               tilt))

(cffi:defcfun ("gr_inqspace" gr-inqspace) :void
  (zmin (:pointer :double))
  (zmax (:pointer :double))
  (rotation (:pointer :int))
  (tilt (:pointer :int)))

(defun inqspace (zmin zmax rotation tilt)
  (let ((zmin-data (data-alloc zmin :double))
        (zmax-data (data-alloc zmax :double))
        (rotation-data (data-alloc rotation :int))
        (tilt-data (data-alloc tilt :int)))
    (gr-inqspace zmin-data
                 zmax-data
                 rotation-data
                 tilt-data)
    (free zmin-data
          zmax-data
          rotation-data
          tilt-data)))


#|
    setscale(options::Int)

setscale sets the type of transformation to be used for subsequent GR output
primitives.

options :
    Scale specification (see Table below)

    +---------------+--------------------+
    |OPTION_X_LOG   |Logarithmic X-axis  |
    +---------------+--------------------+
    |OPTION_Y_LOG   |Logarithmic Y-axis  |
    +---------------+--------------------+
    |OPTION_Z_LOG   |Logarithmic Z-axis  |
    +---------------+--------------------+
    |OPTION_FLIP_X  |Flip X-axis         |
    +---------------+--------------------+
    |OPTION_FLIP_Y  |Flip Y-axis         |
    +---------------+--------------------+
    |OPTION_FLIP_Z  |Flip Z-axis         |
    +---------------+--------------------+

setscale defines the current transformation according to the given scale
specification which may be or'ed together using any of the above options. GR uses
these options for all subsequent output primitives until another value is provided.
The scale options are used to transform points from an abstract logarithmic or
semi-logarithmic coordinate system, which may be flipped along each axis, into the
world coordinate system.

Note: When applying a logarithmic transformation to a specific axis, the system
assumes that the axes limits are greater than zero.

|#

(cffi:defcfun ("gr_setscale" gr-setscale) :int
  (options :int))

(defun setscale (options)
  (gr-setscale options))


(cffi:defcfun ("gr_inqscale" gr-inqscale) :void
  (options (:pointer :int)))

(defun inqscale ()
  (let ((options '(0)))
    (gr-inqscale (data-alloc options :int))
    options))


#|
    textext(x::Real, y::Real, string)

Draw a text at position x, y using the current text attributes. Strings can be
defined to create basic mathematical expressions and Greek letters.

x :
    The X coordinate of starting position of the text string

y :
    The Y coordinate of starting position of the text string

string :
    The text to be drawn

The values for X and Y are in normalized device coordinates.
The attributes that control the appearance of text are text font and precision,
character expansion factor, character spacing, text color index, character
height, character up vector, text path and text alignment.

The character string is interpreted to be a simple mathematical formula.
The following notations apply:

Subscripts and superscripts: These are indicated by carets ('^') and underscores
('_'). If the sub/superscript contains more than one character, it must be enclosed
in curly braces ('{}').

Fractions are typeset with A '/' B, where A stands for the numerator and B for the
denominator.

To include a Greek letter you must specify the corresponding keyword after a
backslash ('\') character. The text translator produces uppercase or lowercase
Greek letters depending on the case of the keyword.

    +--------+---------+
    |Letter  |Keyword  |
    +--------+---------+
    |Α α     |alpha    |
    +--------+---------+
    |Β β     |beta     |
    +--------+---------+
    |Γ γ     |gamma    |
    +--------+---------+
    |Δ δ     |delta    |
    +--------+---------+
    |Ε ε     |epsilon  |
    +--------+---------+
    |Ζ ζ     |zeta     |
    +--------+---------+
    |Η η     |eta      |
    +--------+---------+
    |Θ θ     |theta    |
    +--------+---------+
    |Ι ι     |iota     |
    +--------+---------+
    |Κ κ     |kappa    |
    +--------+---------+
    |Λ λ     |lambda   |
    +--------+---------+
    |Μ μ     |mu       |
    +--------+---------+
    |Ν ν     |nu       |
    +--------+---------+
    |Ξ ξ     |xi       |
    +--------+---------+
    |Ο ο     |omicron  |
    +--------+---------+
    |Π π     |pi       |
    +--------+---------+
    |Ρ ρ     |rho      |
    +--------+---------+
    |Σ σ     |sigma    |
    +--------+---------+
    |Τ τ     |tau      |
    +--------+---------+
    |Υ υ     |upsilon  |
    +--------+---------+
    |Φ φ     |phi      |
    +--------+---------+
    |Χ χ     |chi      |
    +--------+---------+
    |Ψ ψ     |psi      |
    +--------+---------+
    |Ω ω     |omega    |
    +--------+---------+
For more sophisticated mathematical formulas, you should use the `gr.mathtex`
function.
|#

(cffi:defcfun ("gr_textext" gr-textext) :int
  (x :double)
  (y :double)
  (str (:pointer :char)))

(defun textext (x y str)
  (let ((str-data (string-alloc str)))
    (gr-textext (coerce x 'double-float)
                (coerce y 'double-float)
                str-data)
    (free str-data)))

(cffi:defcfun ("gr_inqtextext" gr-inqtextext) :void
  (x :double)
  (y :double)
  (str (:pointer :char))
  (tbx (:pointer :double))
  (tby (:pointer :double)))

(defun inqtextext (x y str)
  (let ((str-data (string-alloc str))
        (tbx (data-alloc '(0 0 0 0) :double))
        (tby (data-alloc '(0 0 0 0) :double)))
    (gr-inqtextext (coerce x 'double-float)
                   (coerce y 'double-float)
                   (string-alloc str)
                   tbx
                   tby)
    (free str-data)
    (list tbx tby)))

#|
    axes2d(x_tick::Real, y_tick::Real, x_org::Real, y_org::Real, major_x::Int, major_y::Int, tick_size::Real)

Draw X and Y coordinate axes with linearly and/or logarithmically spaced tick marks.

x_tick, y_tick :
    The interval between minor tick marks on each axis.

x_org, y_org :
    The world coordinates of the origin (point of intersection) of the X
    and Y axes.

major_x, major_y :
    Unitless integer values specifying the number of minor tick intervals
    between major tick marks. Values of 0 or 1 imply no minor ticks.
    Negative values specify no labels will be drawn for the associated axis.

tick_size :
    The length of minor tick marks specified in a normalized device
    coordinate unit. Major tick marks are twice as long as minor tick marks.
    A negative value reverses the tick marks on the axes from inward facing
    to outward facing (or vice versa).

Tick marks are positioned along each axis so that major tick marks fall on the axes
origin (whether visible or not). Major tick marks are labeled with the corresponding
data values. Axes are drawn according to the scale of the window. Axes and tick marks
are drawn using solid lines; line color and width can be modified using the
setlinetype and setlinewidth functions. Axes are drawn according to
the linear or logarithmic transformation established by the setscale function.
|#

(cffi:defcfun ("gr_axes" gr-axes) :void
  (x-tick :double)
  (y-tick :double)
  (x-org :double)
  (y-org :double)
  (major-x :int)
  (major-y :int)
  (tick-size :double))

(defun axes (x-tick y-tick x-org y-org major-x major-y tick-size)
  (gr-axes (coerce x-tick 'double-float)
           (coerce y-tick 'double-float)
           (coerce x-org 'double-float)
           (coerce y-org 'double-float)
           major-x
           major-y
           (coerce tick-size 'double-float)))


#|
    function axeslbl(x_tick::Real, y_tick::Real, x_org::Real, y_org::Real, major_x::Int, major_y::Int, tick_size::Real, fpx::Function, fpy::Function)

Draw X and Y coordinate axes with linearly and/or logarithmically spaced tick marks.

Tick marks are positioned along each axis so that major tick marks fall on the
axes origin (whether visible or not). Major tick marks are labeled with the
corresponding data values. Axes are drawn according to the scale of the window.
Axes and tick marks are drawn using solid lines; line color and width can be
modified using the `setlinetype` and `setlinewidth` functions.
Axes are drawn according to the linear or logarithmic transformation established
by the `setscale` function.

x_tick, y_tick :
    The interval between minor tick marks on each axis.

x_org, y_org :
    The world coordinates of the origin (point of intersection) of the X
    and Y axes.

major_x, major_y :
    Unitless integer values specifying the number of minor tick intervals
    between major tick marks. Values of 0 or 1 imply no minor ticks.
    Negative values specify no labels will be drawn for the associated axis.

tick_size :
    The length of minor tick marks specified in a normalized device
    coordinate unit. Major tick marks are twice as long as minor tick marks.
    A negative value reverses the tick marks on the axes from inward facing
    to outward facing (or vice versa).

fx, fy :
    Functions that returns a label for a given tick on the X or Y axis.
    Those functions should have the following arguments:
x, y :
    Normalized device coordinates of the label in X and Y directions.

svalue :
    Internal string representation of the text drawn at (x,y).

value :
    Floating point representation of the label drawn at (x,y).
|#

(cffi:defcfun ("gr_axeslbl" gr-axeslbl) :void
  (x-tick :double)
  (y-tick :double)
  (x-org :double)
  (y-org :double)
  (major-x :double)
  (major-y :double)
  (tick-size :double)
  (fpx :pointer)
  (fpy :pointer))

(defun axeslbl (x-tick y-tick x-org y-org
                major-x major-y tick-size fx fy)
  (cffi:defcallback fpx :void
    ((a :double)
     (b :double)
     (str (:pointer :char))
     (c :double))
    (eval (list fx a b str c)))
  (cffi:defcallback fpy :void
    ((a :double)
     (b :double)
     (str (:pointer :char))
     (c :double))
    (eval (list fy a b str c)))
  (gr-axeslbl x-tick y-tick x-org y-org
              major-x major-y tick-size
              (cffi:callback fpx) (cffi:callback fpy))
  (free fpx fpy))
