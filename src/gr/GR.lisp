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
  (:import-from :kai.util
                :make-kai-cache
                :data-alloc
                :free
                :string-alloc
                :string-free
                :arr-aref
                :flatten)
  (:export :init
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
           :setwindow3d))
(in-package :kai.GR.GR)


;;;; Bindings
;;;
;;; We have to wrap GR functions with CFFI.


;; Initialize GR states

(cffi:defcfun ("gr_initgr" initgr) :void)

(cffi:defcfun ("gr_opengks" opengks) :void)

(cffi:defcfun ("gr_closegks" closegks) :void)


;;;; Check display size
;;;
;;; To plot some data, we have to check windows size.

(cffi:defcfun ("gr_inqdspsize" gr-inqdspsize) :void
  (mwidth (:pointer :double))
  (mheight (:pointer  :double))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun inqdspsize ()
  (let ((mwidth (data-alloc '(0) :double))
        (mheight (data-alloc '(0) :double))
        (width (data-alloc (0) :int))
        (height (data-alloc '(0) :int)))
    (gr-inqdspsize mwidth
                   mheight
                   width
                   height)
    (let ((-mwidth (arr-aref mwidth :double 0))
          (-mheight (arr-aref mheight :double 0))
          (-width (arr-aref width :int 0))
          (-height (arr-aref height :int 0)))
      (free mwidth mheight width height)
      (list -mwidth -mheight -width -height))))



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

(cffi:defcfun ("gr_openws" gr-openws) :void
  (workstation-id :int)
  (connection (:pointer :char))
  (type :int))

(defun openws (ws-id connection type)
  (let ((conn-data (string-alloc connection)))
    (gr-openws ws-id
               conn-data
               type)))


#|
    closews(workstation_id::Int)

Close the specified workstation.

workstation_id :
    A workstation identifier.
|#

(cffi:defcfun ("gr_closews" gr-closews) :void
  (workstation-id :int))

(defun closews (ws-id)
  (gr-closews ws-id))


#| 
    activatews(workstation_id::Int)

Activate the specified workstation.

workstation_id :
    A workstation identifier.

|#

(cffi:defcfun ("gr_activatews" gr-activatews) :void
  (workstation-id :int))

(defun activatews (ws-id)
  (gr-activatews ws-id))


#|
    deactivatews(workstation_id::Int)

Deactivate the specified workstation.

workstation_id :
    A workstation identifier.
|#

(cffi:defcfun ("gr_deactivatews" gr-deactivatews) :void
  (workstation-id :int))

(defun deactivatews (ws-id)
  (gr-deactivatews ws-id))


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
  (let ((x-data (data-alloc x :double))
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
             str-data)
    (string-free str-data)))

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
    (let ((-tbx (loop for i below 4
                      collect (arr-aref tbx :double i)))
          (-tby (loop for i below 4
                      collect (arr-aref tby :double i))))
      (free tbx tby)
      (string-free str-data)
      (list -tbx -tby))))


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
    (gr-inqlinetype linetype-data)
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
  (gr-setlinecolorind color))


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
  (gr-setcharspace (coerce spacing 'double-float)))


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
    (let ((-xmin (arr-aref xmin :double 0))
          (-xmax (arr-aref xmax :double 0))
          (-ymin (arr-aref ymin :double 0))
          (-ymax (arr-aref ymax :double 0)))
      (free xmin xmax ymin ymax)
      (list -xmin -xmax -ymin -ymax))))


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
    (let ((-xmin (arr-aref xmin :double 0))
          (-xmax (arr-aref xmax :double 0))
          (-ymin (arr-aref ymin :double 0))
          (-ymax (arr-aref ymax :double 0)))
      (free xmin xmax ymin ymax)
      (list -xmin -xmax -ymin -ymax))))


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


(cffi:defcfun ("gr_emergencyclosegks" gr-emergencyclosegks) :void)

(defun emergencyclosegks ()
  (gr-emergencyclosegks))


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
  (let ((options (data-alloc '(0) :int)))
    (gr-inqscale options)
    (let ((opt-data (arr-aref options :int 0)))
      (free options)
      opt-data)))


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
    (string-free str-data)))

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
                   str-data
                   tbx
                   tby)
    (let ((-tbx (loop for i below 4
                      collect (arr-aref tbx :double i)))
          (-tby (loop for i below 4
                      collect (arr-aref tby :double i))))
      (free tbx tby)
      (string-free str-data)
      (list -tbx -tby))))

#|
    axes(x_tick::Real, y_tick::Real, x_org::Real, y_org::Real, major_x::Int, major_y::Int, tick_size::Real)

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
              (cffi:callback fpx) (cffi:callback fpy)))


#|
    grid(x_tick::Real, y_tick::Real, x_org::Real, y_org::Real, major_x::Int, major_y::Int)

Draw a linear and/or logarithmic grid.

x_tick, y_tick :
    The length in world coordinates of the interval between minor grid
    lines.

x_org, y_org :
    The world coordinates of the origin (point of intersection) of the grid.

major_x, major_y :
    Unitless integer values specifying the number of minor grid lines
    between major grid lines. Values of 0 or 1 imply no grid lines.

Major grid lines correspond to the axes origin and major tick marks whether visible
or not. Minor grid lines are drawn at points equal to minor tick marks. Major grid
lines are drawn using black lines and minor grid lines are drawn using gray lines.
|#

(cffi:defcfun ("gr_grid" gr-grid) :void
  (x-tick :double)
  (y-tick :double)
  (x-org :double)
  (y-org :double)
  (major-x :int)
  (major-y :int))

(defun grid (x-tick y-tick x-org y-org major-x major-y)
  (gr-grid (coerce x-tick 'double-float)
           (coerce y-tick 'double-float)
           (coerce x-org 'double-float)
           (coerce y-org 'double-float)
           major-x
           major-y))


(cffi:defcfun ("gr_grid3d" gr-grid3d) :void
  (x-tick :double)
  (y-tick :double)
  (z-tick :double)
  (x-org :double)
  (y-org :double)
  (z-org :double)
  (major-x :int)
  (major-y :int)
  (major-z :int))

(defun grid3d (x-tick y-tick z-tick x-org y-org z-org
               major-x major-y major-z)
  (gr-grid3d (coerce x-tick 'double-float)
             (coerce y-tick 'double-float)
             (coerce z-tick 'double-float)
             (coerce x-org 'double-float)
             (coerce y-org 'double-float)
             (coerce z-org 'double-float)
             major-x
             major-y
             major-z))


#|
    verrorbars(px, py, e1, e2)

Draw a standard vertical error bar graph.

px :
    A list of length N containing the X coordinates

py :
    A list of length N containing the Y coordinates

e1 :
     The absolute values of the lower error bar data

e2 :
     The absolute values of the upper error bar data
|#

(cffi:defcfun ("gr_verrorbars" gr-verrorbars) :void
  (n :int)
  (px (:pointer :double))
  (py (:pointer :double))
  (e1 (:pointer :double))
  (e2 (:pointer :double)))

(defun verrorbars (px py e1 e2)
  (assert (= (length px)
             (length py)
             (length e1)
             (length e2)))
  (let ((px-data (data-alloc px :double))
        (py-data (data-alloc py :double))
        (e1-data (data-alloc e1 :double))
        (e2-data (data-alloc e2 :double)))
    (gr-verrorbars (length px)
                   px-data
                   py-data
                   e1-data
                   e2-data)
    (free px-data
          py-data
          e1-data
          e2-data)))



#|
    herrorbars(px, py, e1, e2)

Draw a standard horizontal error bar graph.

px :
    A list of length N containing the X coordinates

py :
    A list of length N containing the Y coordinates

e1 :
     The absolute values of the lower error bar data

e2 :
     The absolute values of the upper error bar data
|#

(cffi:defcfun ("gr_herrorbars" gr-herrorbars) :void
  (n :int)
  (px (:pointer :double))
  (py (:pointer :double))
  (e1 (:pointer :double))
  (e2 (:pointer :double)))

(defun herrorbars (px py e1 e2)
  (assert (= (length px)
             (length py)
             (length e1)
             (length e2)))
  (let ((px-data (data-alloc px :double))
        (py-data (data-alloc py :double))
        (e1-data (data-alloc e1 :double))
        (e2-data (data-alloc e2 :double)))
    (gr-herrorbars (length px)
                   px-data
                   py-data
                   e1-data
                   e2-data)
    (free px-data
          py-data
          e1-data
          e2-data)))


#|
    polyline3d(px, py, pz)

Draw a 3D curve using the current line attributes, starting from the
first data point and ending at the last data point.

x :
    A list of length N containing the X coordinates

y :
    A list of length N containing the Y coordinates

z :
    A list of length N containing the Z coordinates

The values for x, y and z are in world coordinates. The attributes that
control the appearance of a polyline are linetype, linewidth and color
index.
|#

(cffi:defcfun ("gr_polyline3d" gr-polyline3d) :void
  (n :int)
  (px (:pointer :double))
  (py (:pointer :double))
  (pz (:pointer :double)))

(defun polyline3d (x y z)
  (assert (= (length x)
             (length y)
             (length z)))
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double))
        (z-data (data-alloc z :double)))
    (gr-polyline3d (length x)
                   x-data
                   y-data
                   z-data)
    (free x-data
          y-data
          z-data)))


#|
    polymarker3d(px, py, pz)

Draw marker symbols centered at the given 3D data points.

x :
    A list of length N containing the X coordinates

y :
    A list of length N containing the Y coordinates

z :
    A list of length N containing the Z coordinates

The values for x, y and z are in world coordinates. The attributes
that control the appearance of a polymarker are marker type, marker size
scale factor and color index.
|#

(cffi:defcfun ("gr_polymarker3d" gr-polymarker3d) :void
  (n :int)
  (px (:pointer :double))
  (py (:pointer :double))
  (pz (:pointer :double)))

(defun polymarker3d (x y z)
  (assert (= (length x)
             (length y)
             (length z)))
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double))
        (z-data (data-alloc z :double)))
    (gr-polymarker3d (length x)
                     x-data
                     y-data
                     z-data)
    (free x-data
          y-data
          z-data)))


(cffi:defcfun ("gr_axes3d" gr-axes3d) :void
  (x-tick :double)
  (y-tick :double)
  (z-tick :double)
  (x-org :double)
  (y-org :double)
  (z-org :double)
  (major-x :int)
  (major-y :int)
  (major-z :int)
  (tick-size :double))

(defun axes3d (x-tick y-tick z-tick x-org y-org z-org
               major-x major-y major-z tick-size)
  (gr-axes3d (coerce x-tick 'double-float)
             (coerce y-tick 'double-float)
             (coerce z-tick 'double-float)
             (coerce x-org 'double-float)
             (coerce y-org 'double-float)
             (coerce z-org 'double-float)
             major-x
             major-y
             major-z
             (coerce tick-size 'double-float)))


#|
    titles3d(x_title, y_title, z_title)

Display axis titles just outside of their respective axes.

x_title, y_title, z_title :
    The text to be displayed on each axis

|#

(cffi:defcfun ("gr_titles3d" gr-titles3d) :void
  (x-title (:pointer :char))
  (y-title (:pointer :char))
  (z-title (:pointer :char)))

(defun titles3d (x-title y-title z-title)
  (let ((x-data (string-alloc x-title))
        (y-data (string-alloc y-title))
        (z-data (string-alloc z-title)))
    (gr-titles3d x-data
                y-data
                z-data)
    (string-free x-data
                 y-data
                 z-data)))



#|
    surface(px, py, pz, option::Int)

Draw a three-dimensional surface plot for the given data points.

x :
    A list containing the X coordinates

y :
    A list containing the Y coordinates

z :
    A list of length len(x) * len(y) or an appropriately dimensioned
    array containing the Z coordinates

option :
    Surface display option (see table below)

x and y define a grid. z is a singly dimensioned array containing at least
nx * ny data points. Z describes the surface height at each point on the grid.
Data is ordered as shown in the following table:

    +------------------+--+--------------------------------------------------------------+
    |LINES             | 0|Use X Y polylines to denote the surface                       |
    +------------------+--+--------------------------------------------------------------+
    |MESH              | 1|Use a wire grid to denote the surface                         |
    +------------------+--+--------------------------------------------------------------+
    |FILLED_MESH       | 2|Applies an opaque grid to the surface                         |
    +------------------+--+--------------------------------------------------------------+
    |Z_SHADED_MESH     | 3|Applies Z-value shading to the surface                        |
    +------------------+--+--------------------------------------------------------------+
    |COLORED_MESH      | 4|Applies a colored grid to the surface                         |
    +------------------+--+--------------------------------------------------------------+
    |CELL_ARRAY        | 5|Applies a grid of individually-colored cells to the surface   |
    +------------------+--+--------------------------------------------------------------+
    |SHADED_MESH       | 6|Applies light source shading to the 3-D surface               |
    +------------------+--+--------------------------------------------------------------+
|#

(cffi:defcfun ("gr_surface" gr-surface) :void
  (nx :int)
  (ny :int)
  (px (:pointer :double))
  (py (:pointer :double))
  (pz (:pointer :double))
  (option :int))

(defun surface (x y z &key (option 1))
  (assert (= (length (flatten z))
             (* (length x) (length y))))
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double))
        (z-data (data-alloc (flatten z) :double)))
    (gr-surface (length x)
                (length y)
                x-data
                y-data
                z-data
                option)
    (free x-data
          y-data
          z-data)))


#|
    contour(px, py, h, pz, major_h::Int)

Draw contours of a three-dimensional data set whose values are specified over a
rectangular mesh. Contour lines may optionally be labeled.

x :
    A list containing the X coordinates

y :
    A list containing the Y coordinates

h :
    A list containing the Z coordinate for the height values

z :
    A list of length `len(x)` * `len(y)` or an appropriately dimensioned
    array containing the Z coordinates

major_h :
    Directs GR to label contour lines. For example, a value of 3 would label
    every third line. A value of 1 will label every line. A value of 0
    produces no labels. To produce colored contour lines, add an offset
    of 1000 to major_h.
|#

(cffi:defcfun ("gr_contour" gr-contour) :void
  (nx :int)
  (ny :int)
  (nh :int)
  (px (:pointer :double))
  (py (:pointer :double))
  (h (:pointer :double))
  (pz (:pointer :double))
  (major-h :int))

(defun contour (x y h z major-h)
  (assert (= (length (flatten z))
             (* (length x) (length y))))
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double))
        (h-data (data-alloc h :double))
        (z-data (data-alloc z :double)))
    (gr-contour (length x)
                (length y)
                (length h)
                x-data
                y-data
                h-data
                z-data
                major-h)
    (free x-data
          y-data
          h-data
          z-data)))


#|
    contourf(px, py, h, pz, major_h::Int)

Draw filled contours of a three-dimensional data set whose values are
specified over a rectangular mesh.

x :
    A list containing the X coordinates

y :
    A list containing the Y coordinates

h :
    A list containing the Z coordinate for the height values

z :
    A list of length len(x) * len(y) or an appropriately dimensioned
    array containing the Z coordinates

major_h :
    (intended for future use)
|#

(cffi:defcfun ("gr_contourf" gr-contourf) :void
  (nx :int)
  (ny :int)
  (nh :int)
  (px (:pointer :double))
  (py (:pointer :double))
  (h (:pointer :double))
  (pz (:pointer :double))
  (major-h :int))

(defun contourf (x y h z major-h)
  (assert (= (length (flatten z))
             (* (length x) (length y))))
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double))
        (h-data (data-alloc h :double))
        (z-data (data-alloc z :double)))
    (gr-contourf (length x)
                 (length y)
                 (length h)
                 x-data
                 y-data
                 h-data
                 z-data
                 major-h)
    (free x-data
          y-data
          h-data
          z-data)))


#|
    tricontour(x, y, z, levels)

Draw a contour plot for the given triangle mesh.

x :
    A list containing the X coordinates

y :
    A list containing the Y coordinates

z :
    A list containing the Z coordinates

levels :
    A list containing the contour levels

|#

(cffi:defcfun ("gr_tricontour" gr-tricontour) :void
  (npoints :int)
  (x (:pointer :double))
  (y (:pointer :double))
  (z (:pointer :double))
  (nlebels :int)
  (lebels (:pointer :double)))

(defun tricontour (x y z levels)
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double))
        (z-data (data-alloc z :double))
        (levels-data (data-alloc levels :double)))
    (gr-tricontour (length x)
                   x-data
                   y-data
                   z-data
                   (length levels)
                   levels-data)
    (free x-data
          y-data
          z-data
          levels-data)))


(cffi:defcfun ("gr_hexbin" gr-hexbin) :int
  (n :int)
  (x (:pointer :double))
  (y (:pointer :double))
  (nbins :int))

(defun hexbin (x y nbins)
  (assert (= (length x)
             (length y)))
  (let* ((x-data (data-alloc x :double))
         (y-data (data-alloc y :double))
         (nhexbin (gr-hexbin (length x)
                             x-data
                             y-data
                             nbins)))
    (free x-data
          y-data)
    nhexbin))


(cffi:defcfun ("gr_setcolormap" gr-setcolormap) :void
  (index :int))

(defun setcolormap (index)
  (gr-setcolormap index))


(cffi:defcfun ("gr_inqcolormap" gr-inqcolormap) :void
  (index (:pointer :int)))

(defun inqcolormap (index)
  (let ((index-data (data-alloc index :int)))
    (gr-inqcolormap index-data)
    (free index-data)))


(cffi:defcfun ("gr_setcolormapfromrgb" gr-setcolormapfromrgb) :void
  (n :int)
  (r (:pointer :double))
  (g (:pointer :double))
  (b (:pointer :double))
  (x (:pointer :double)))

(defun setcolormapfromrgb (r g b &rest position)
  (assert (= (length r)
             (length g)
             (length b)))
  (let ((r-data (data-alloc r :double))
        (g-data (data-alloc g :double))
        (b-data (data-alloc b :double))
        (pos-data (if (null position)
                      (cffi:null-pointer)
                      (data-alloc (flatten position) :double))))
    (gr-setcolormapfromrgb (length r)
                           r-data
                           g-data
                           b-data
                           pos-data)
    (free r-data
          g-data
          b-data
          pos-data)))


(cffi:defcfun ("gr_colorbar" gr-colorbar) :void)

(defun colorbar ()
  (gr-colorbar))


(cffi:defcfun ("gr_inqcolor" gr-inqcolor) :void
  (color :int)
  (rgb (:pointer :int)))

(defun inqcolor (color)
  (let ((rgb (data-alloc '(0) :int)))
    (gr-inqcolor color rgb)
    (let ((data (cffi:mem-aref rgb :int 0)))
      (free rgb)
      data)))


(cffi:defcfun ("gr_inqcolorfromrgb" gr-inqcolorfromrgb) :int
  (r :double)
  (g :double)
  (b :double))

(defun inqcolorfromrgb (r g b)
  (gr-inqcolorfromrgb (coerce r 'double-float)
                      (coerce g 'double-float)
                      (coerce b 'double-float)))


(cffi:defcfun ("gr_hsvtorgb" gr-hsvtorgb) :void
  (h :double)
  (s :double)
  (v :double)
  (r (:pointer :double))
  (g (:pointer :double))
  (b (:pointer :double)))

(defun hsvtorgb (h s v)
  (let ((r (data-alloc '(0) :double))
        (g (data-alloc '(0) :double))
        (b (data-alloc '(0) :double)))
    (gr-hsvtorgb (coerce h 'double-float)
                 (coerce s 'double-float)
                 (coerce v 'double-float)
                 r
                 g
                 b)
    (let ((-r (arr-aref r :double 0))
          (-g (arr-aref g :double 0))
          (-b (arr-aref b :double 0)))
      (free r g b)
      (list -r -g -b))))


(cffi:defcfun ("gr_tick" gr-tick) :double
  (amin :double)
  (amax :double))

(defun tick (amin amax)
  (gr-tick (coerce amin 'double-float)
           (coerce amax 'double-float)))


(cffi:defcfun ("gr_validaterange" gr-validaterange) :int
  (amin :double)
  (amax :double))

(defun validaterange (amin amax)
  (gr-validaterange (coerce amin 'double-float)
                    (coerce amax 'double-float)))


(cffi:defcfun ("gr_adjustlimits" gr-adjustlimits) :void
  (amin (:pointer :double))
  (amax (:pointer :double)))

(defun adjustlimits (amin amax)
  (let ((amin-data (data-alloc amin :double))
        (amax-data (data-alloc amax :double)))
    (gr-adjustlimits amin-data
                     amax-data)
    (let ((-amin (arr-aref amin-data :double 0))
          (-amax (arr-aref amax-data :double 0)))
      (free amin-data
            amax-data)
      (list -amin -amax))))


(cffi:defcfun ("gr_adjustrange" gr-adjustrange) :void
  (amin (:pointer :double))
  (amax (:pointer :double)))

(defun adjustrange (amin amax)
  (let ((amin-data (data-alloc (list amin) :double))
        (amax-data (data-alloc (list amax) :double)))
    (gr-adjustrange amin-data
                    amax-data)
    (let ((-amin (arr-aref amin-data :double 0))
          (-amax (arr-aref amax-data :double 0)))
      (free amin-data
            amax-data)
      (list -amin -amax))))



#|
    beginprint(pathname)

Open and activate a print device.

pathname :
    Filename for the print device.

beginprint opens an additional graphics output device. The device type is obtained
from the given file extension. The following file types are supported:

    +-------------+---------------------------------------+
    |.ps, .eps    |PostScript                             |
    +-------------+---------------------------------------+
    |.pdf         |Portable Document Format               |
    +-------------+---------------------------------------+
    |.bmp         |Windows Bitmap (BMP)                   |
    +-------------+---------------------------------------+
    |.jpeg, .jpg  |JPEG image file                        |
    +-------------+---------------------------------------+
    |.png         |Portable Network Graphics file (PNG)   |
    +-------------+---------------------------------------+
    |.tiff, .tif  |Tagged Image File Format (TIFF)        |
    +-------------+---------------------------------------+
    |.fig         |Xfig vector graphics file              |
    +-------------+---------------------------------------+
    |.svg         |Scalable Vector Graphics               |
    +-------------+---------------------------------------+
    |.wmf         |Windows Metafile                       |
    +-------------+---------------------------------------+
|#

(cffi:defcfun ("gr_beginprint" gr-beginprint) :void
  (pathname (:pointer :char)))

(defun beginprint (pathname)
  (let ((pathname-data (string-alloc pathname)))
    (gr-beginprint pathname-data)
    (string-free pathname-data)))



#|
    beginprintext(pathname, mode, fmt, orientation)

Open and activate a print device with the given layout attributes.

pathname :
    Filename for the print device.
mode :
    Output mode (Color, GrayScale)
fmt :
    Output format (see table below)
orientation :
    Page orientation (Landscape, Portait)

The available formats are:

    +-----------+---------------+
    |A4         |0.210 x 0.297  |
    +-----------+---------------+
    |B5         |0.176 x 0.250  |
    +-----------+---------------+
    |Letter     |0.216 x 0.279  |
    +-----------+---------------+
    |Legal      |0.216 x 0.356  |
    +-----------+---------------+
    |Executive  |0.191 x 0.254  |
    +-----------+---------------+
    |A0         |0.841 x 1.189  |
    +-----------+---------------+
    |A1         |0.594 x 0.841  |
    +-----------+---------------+
    |A2         |0.420 x 0.594  |
    +-----------+---------------+
    |A3         |0.297 x 0.420  |
    +-----------+---------------+
    |A5         |0.148 x 0.210  |
    +-----------+---------------+
    |A6         |0.105 x 0.148  |
    +-----------+---------------+
    |A7         |0.074 x 0.105  |
    +-----------+---------------+
    |A8         |0.052 x 0.074  |
    +-----------+---------------+
    |A9         |0.037 x 0.052  |
    +-----------+---------------+
    |B0         |1.000 x 1.414  |
    +-----------+---------------+
    |B1         |0.500 x 0.707  |
    +-----------+---------------+
    |B10        |0.031 x 0.044  |
    +-----------+---------------+
    |B2         |0.500 x 0.707  |
    +-----------+---------------+
    |B3         |0.353 x 0.500  |
    +-----------+---------------+
    |B4         |0.250 x 0.353  |
    +-----------+---------------+
    |B6         |0.125 x 0.176  |
    +-----------+---------------+
    |B7         |0.088 x 0.125  |
    +-----------+---------------+
    |B8         |0.062 x 0.088  |
    +-----------+---------------+
    |B9         |0.044 x 0.062  |
    +-----------+---------------+
    |C5E        |0.163 x 0.229  |
    +-----------+---------------+
    |Comm10E    |0.105 x 0.241  |
    +-----------+---------------+
    |DLE        |0.110 x 0.220  |
    +-----------+---------------+
    |Folio      |0.210 x 0.330  |
    +-----------+---------------+
    |Ledger     |0.432 x 0.279  |
    +-----------+---------------+
    |Tabloid    |0.279 x 0.432  |
    +-----------+---------------+

|#

(cffi:defcfun ("gr_beginprinttext" gr-beginprinttext) :void
  (pahtname (:pointer :char))
  (mode (:pointer :char))
  (fmt (:pointer :char))
  (orientation (:pointer :char)))

(defun beginprinttext (pathname mode fmt orientation)
  (let ((pathname-data (string-alloc pathname))
        (mode-data (string-alloc mode))
        (fmt-data (string-alloc fmt))
        (orientation-data (string-alloc orientation)))
    (gr-beginprinttext pathname-data
                       mode-data
                       fmt-data
                       orientation-data)
    (string-free pathname-data
                 mode-data
                 fmt-data
                 orientation-data)))


(cffi:defcfun ("gr_endprint" gr-endprint) :void)

(defun endprint ()
  (gr-endprint))


(cffi:defcfun ("gr_ndctowc" gr-ndctowc) :void
  (x (:pointer :double))
  (y (:pointer :double)))

(defun ndctowc (x y)
  (let ((x-data (data-alloc (list x) :double))
        (y-data (data-alloc (list y) :double)))
    (gr-ndctowc x-data
                y-data)
    (let ((-x (arr-aref x-data :double 0))
          (-y (arr-aref y-data :double 0)))
      (free x-data
            y-data)
      (list -x -y))))


(cffi:defcfun ("gr_wctondc" gr-wctondc) :void
  (x (:pointer :double))
  (y (:pointer :double)))

(defun wctondc (x y)
  (let ((x-data (data-alloc (list x) :double))
        (y-data (data-alloc (list y) :double)))
    (gr-wctondc x-data
                y-data)
    (let ((-x (arr-aref x-data :double 0))
          (-y (arr-aref y-data :double 0)))
      (free x-data
            y-data)
      (list -x -y))))


(cffi:defcfun ("gr_wc3towc" gr-wc3towc) :void
  (x (:pointer :double))
  (y (:pointer :double))
  (z (:pointer :double)))

(defun wc3towc (x y z)
  (let ((x-data (data-alloc (list x) :double))
        (y-data (data-alloc (list y) :double))
        (z-data (data-alloc (list z) :double)))
    (gr-wc3towc x-data
                y-data
                z-data)
    (let ((-x (arr-aref x-data :double 0))
          (-y (arr-aref y-data :double 0))
          (-z (arr-aref z-data :double 0)))
      (free x-data
            y-data
            z-data)
      (list -x -y -z))))


#|
    drawrect(xmin::Real, xmax::Real, ymin::Real, ymax::Real)

Draw a rectangle using the current line attributes.

xmin :
    Lower left edge of the rectangle
xmax :
    Lower right edge of the rectangle
ymin :
    Upper left edge of the rectangle
ymax :
    Upper right edge of the rectangle
|#

(cffi:defcfun ("gr_drawrect" gr-drawrect) :void
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double))

(defun drawrect (xmin xmax ymin ymax)
  (gr-drawrect (coerce xmin 'double-float)
               (coerce xmax 'double-float)
               (coerce ymin 'double-float)
               (coerce ymax 'double-float)))


#|
    fillrect(xmin::Real, xmax::Real, ymin::Real, ymax::Real)

Draw a filled rectangle using the current fill attributes.

xmin :
    Lower left edge of the rectangle
xmax :
    Lower right edge of the rectangle
ymin :
    Upper left edge of the rectangle
ymax :
    Upper right edge of the rectangle
|#

(cffi:defcfun ("gr_fillrect" gr-fillrect) :void
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double))

(defun fillrect (xmin xmax ymin ymax)
  (gr-fillrect (coerce xmin 'double-float)
               (coerce xmax 'double-float)
               (coerce ymin 'double-float)
               (coerce ymax 'double-float)))


#|
    drawarc(xmin::Real, xmax::Real, ymin::Real, ymax::Real, a1::Real, a2::Real)

Draw a circular or elliptical arc covering the specified rectangle.

xmin :
    Lower left edge of the rectangle
xmax :
    Lower right edge of the rectangle
ymin :
    Upper left edge of the rectangle
ymax :
    Upper right edge of the rectangle
a1 :
    The start angle
a2 :
    The end angle

The resulting arc begins at a1 and ends at a2 degrees. Angles are interpreted
such that 0 degrees is at the 3 o'clock position. The center of the arc is the center
of the given rectangle.
|#

(cffi:defcfun ("gr_drawarc" gr-drawarc) :void
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double)
  (a1 :double)
  (a2 :double))

(defun drawarc (xmin xmax ymin ymax a1 a2)
  (gr-drawarc (coerce xmin 'double-float)
              (coerce xmax 'double-float)
              (coerce ymin 'double-float)
              (coerce ymax 'double-float)
              (coerce a1 'double-float)
              (coerce a2 'double-float)))


#|
    fillarc(xmin::Real, xmax::Real, ymin::Real, ymax::Real, a1::Real, a2::Real)

Fill a circular or elliptical arc covering the specified rectangle.

xmin :
    Lower left edge of the rectangle
xmax :
    Lower right edge of the rectangle
ymin :
    Upper left edge of the rectangle
ymax :
    Upper right edge of the rectangle
a1 :
    The start angle
a2 :
    The end angle

The resulting arc begins at a1 and ends at a2 degrees. Angles are interpreted
such that 0 degrees is at the 3 o'clock position. The center of the arc is the center
of the given rectangle.
|#

(cffi:defcfun ("gr_fillarc" gr-fillarc) :void
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double)
  (a1 :double)
  (a2 :double))

(defun fillarc (xmin xmax ymin ymax a1 a2)
  (gr-fillarc (coerce xmin 'double-float)
              (coerce xmax 'double-float)
              (coerce ymin 'double-float)
              (coerce ymax 'double-float)
              (coerce a1 'double-float)
              (coerce a2 'double-float)))


#|
    drawpath(points, codes, fill::Int)

Draw simple and compound outlines consisting of line segments and bezier curves.

points :
    (N, 2) array of (x, y) vertices
codes :
    N-length array of path codes
fill :
    A flag indication whether resulting path is to be filled or not

The following path codes are recognized:

    +----------+-----------------------------------------------------------+
    |      STOP|end the entire path                                        |
    +----------+-----------------------------------------------------------+
    |    MOVETO|move to the given vertex                                   |
    +----------+-----------------------------------------------------------+
    |    LINETO|draw a line from the current position to the given vertex  |
    +----------+-----------------------------------------------------------+
    |    CURVE3|draw a quadratic Bézier curve                              |
    +----------+-----------------------------------------------------------+
    |    CURVE4|draw a cubic Bézier curve                                  |
    +----------+-----------------------------------------------------------+
    | CLOSEPOLY|draw a line segment to the start point of the current path |
    +----------+-----------------------------------------------------------+
|#

(cffi:defcstruct vertex-t
  (x :double)
  (y :double))

(cffi:defcfun ("gr_drawpath" gr-drawpath) :void
  (n :int)
  (vertices :pointer)
  (codes (:pointer :unsigned-char))
  (fill :int))

(defun drawpath (points codes fill)
  (let ((c (string-alloc codes)))
    (cffi:with-foreign-object (v '(:struct vertex-t))
      (setf (cffi:foreign-slot-value v '(:struct vertex-t) 'x) (car points)
            (cffi:foreign-slot-value v '(:struct vertex-t) 'y) (cadr points))
      (gr-drawpath (length codes)
                   v
                   c
                   fill))
    (string-free c)))


#|
    setarrowstyle(style::Int)

Set the arrow style to be used for subsequent arrow commands.

`style` :
    The arrow style to be used

setarrowstyle defines the arrow style for subsequent arrow primitives.
The default arrow style is 1.

    +---+----------------------------------+
    |  1|simple, single-ended              |
    +---+----------------------------------+
    |  2|simple, single-ended, acute head  |
    +---+----------------------------------+
    |  3|hollow, single-ended              |
    +---+----------------------------------+
    |  4|filled, single-ended              |
    +---+----------------------------------+
    |  5|triangle, single-ended            |
    +---+----------------------------------+
    |  6|filled triangle, single-ended     |
    +---+----------------------------------+
    |  7|kite, single-ended                |
    +---+----------------------------------+
    |  8|filled kite, single-ended         |
    +---+----------------------------------+
    |  9|simple, double-ended              |
    +---+----------------------------------+
    | 10|simple, double-ended, acute head  |
    +---+----------------------------------+
    | 11|hollow, double-ended              |
    +---+----------------------------------+
    | 12|filled, double-ended              |
    +---+----------------------------------+
    | 13|triangle, double-ended            |
    +---+----------------------------------+
    | 14|filled triangle, double-ended     |
    +---+----------------------------------+
    | 15|kite, double-ended                |
    +---+----------------------------------+
    | 16|filled kite, double-ended         |
    +---+----------------------------------+
    | 17|double line, single-ended         |
    +---+----------------------------------+
    | 18|double line, double-ended         |
    +---+----------------------------------+
|#

(cffi:defcfun ("gr_setarrowstyle" gr-setarrowstyle) :void
  (style :int))

(defun setarrowstyle (style)
  (gr-setarrowstyle style))


#|
    setarrowsize(size::Real)

Set the arrow size to be used for subsequent arrow commands.

size :
    The arrow size to be used

setarrowsize defines the arrow size for subsequent arrow primitives.
The default arrow size is 1.
|#

(cffi:defcfun ("gr_setarrowsize" gr-setarrowsize) :void
  (size :double))

(defun setarrowsize (size)
  (gr-setarrowsize (coerce size 'double-float)))


#|
    drawarrow(x1::Real, y1::Real, x2::Real, y2::Real)

Draw an arrow between two points.

x1, y1 :
    Starting point of the arrow (tail)
x2, y2 :
    Head of the arrow

Different arrow styles (angles between arrow tail and wing, optionally filled
heads, double headed arrows) are available and can be set with the `setarrowstyle`
function.
|#

(cffi:defcfun ("gr_drawarrow" gr-drawarrow) :void
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double))

(defun drawarrow (x1 y1 x2 y2)
  (gr-drawarrow (coerce x1 'double-float)
                (coerce y1 'double-float)
                (coerce x2 'double-float)
                (coerce y2 'double-float)))


(cffi:defcfun ("gr_readimage" gr-readimage) :void
  (path (:pointer :char))
  (width (:pointer :int))
  (height (:pointer :int))
  (data (:pointer (:pointer :int))))

(defun readimage (path)
  (let ((path-data (string-alloc path))
        (w (data-alloc '(0) :int))
        (h (data-alloc '(0) :int))
        (data (cffi:foreign-alloc :pointer
                                  :initial-element
                                  (data-alloc '(0) :int))))
    (gr-readimage path-data
                  w
                  h
                  data)
    (let ((-w (arr-aref w :int 0))
          (-h (arr-aref h :int 0))
          (img (loop for i below (arr-aref h :int 0)
                     collect (loop for j below (arr-aref w :int 0)
                                   collect (arr-aref (arr-aref data :pointer 0)
                                                     :int
                                                     (+ j
                                                        (* i (arr-aref w :int 0))))))))
      (string-free path-data)
      (free w h data)
      (list -w -h img))))


#||
    drawimage(xmin::Real, xmax::Real, ymin::Real, ymax::Real, width::Int, height::Int, data, model::Int = 0)

Draw an image into a given rectangular area.

xmin, ymin :
    First corner point of the rectangle
xmax, ymax :
    Second corner point of the rectangle
width, height :
    The width and the height of the image
data :
    An array of color values dimensioned width by height
model :
    Color model (default=0)

The available color models are:

    +-----------------------+---+-----------+
    |MODEL_RGB              |  0|   AABBGGRR|
    +-----------------------+---+-----------+
    |MODEL_HSV              |  1|   AAVVSSHH|
    +-----------------------+---+-----------+

The points (xmin, ymin) and (xmax, ymax) are world coordinates defining
diagonally opposite corner points of a rectangle. This rectangle is divided into
`width` by `height` cells. The two-dimensional array `data` specifies colors
for each cell.

|#

(cffi:defcfun ("gr_drawimage" gr-drawimage) :void
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double)
  (width :int)
  (height :int)
  (data (:pointer :int))
  (model :int))

(defun drawimage (xmin xmax ymin ymax width height data model)
  (let ((img (data-alloc (flatten data) :int)))
    (gr-drawimage (coerce xmin 'double-float)
                  (coerce xmax 'double-float)
                  (coerce ymin 'double-float)
                  (coerce ymax 'double-float)
                  width
                  height
                  img
                  model)
    (free img)))


(cffi:defcfun ("gr_importgraphics" gr-importgraphics) :int
  (path (:pointer :char)))

(defun importgraphics (path)
  (let* ((path-data (string-alloc path))
         (ret (gr-importgraphics path-data)))
    (string-free path-data)
    ret))


#|
    setshadow(offsetx::Real, offsety::Real, blur::Real)

setshadow allows drawing of shadows, realized by images painted underneath,
and offset from, graphics objects such that the shadow mimics the effect of a light
source cast on the graphics objects.

offsetx :
    An x-offset, which specifies how far in the horizontal direction the
    shadow is offset from the object
offsety :
    A y-offset, which specifies how far in the vertical direction the shadow
    is offset from the object
blur :
    A blur value, which specifies whether the object has a hard or a diffuse
    edge

|#

(cffi:defcfun ("gr_setshadow" gr-setshadow) :void
  (offsetx :double)
  (offsety :double)
  (blur :double))

(defun setshadow (offsetx offsety blur)
  (gr-setshadow (coerce offsetx 'double-float)
                (coerce offsety 'double-float)
                (coerce blur 'double-float)))


#|
    settransparency(alpha::Real)

Set the value of the alpha component associated with GR colors.

alpha :
    An alpha value (0.0 - 1.0)

|#

(cffi:defcfun ("gr_settransparency" gr-settransparency) :void
  (alpha :double))

(defun settransparency (alpha)
  (gr-settransparency (coerce alpha 'double-float)))


#|
    setcoordxform(mat)

Change the coordinate transformation according to the given matrix.

mat[3][2] :
    2D transformation matrix
|#

(cffi:defcfun ("gr_setcoordxform" gr-setcoordxform) :void
  (mat (:pointer :double)))

(defun setcoordxform (mat)
  (assert (= (length (flatten mat)) 6))
  (let ((mat-data (data-alloc (flatten mat) :double)))
    (gr-setcoordxform mat-data)
    (free mat-data)))


#|
    begingraphics(path)

Open a file for graphics output.

path :
    Filename for the graphics file.

begingraphics allows to write all graphics output into a XML-formatted file until
the `endgraphics` functions is called. The resulting file may later be imported with
the `importgraphics` function.

|#

(cffi:defcfun ("gr_begingraphics" gr-begingraphics) :void
  (path (:pointer :char)))

(defun begingraphics (path)
  (let ((path-data (string-alloc path)))
    (gr-begingraphics path-data)
    (string-free path-data)))


(cffi:defcfun ("gr_endgraphics" gr-endgraphics) :void)

(defun endgraphics ()
  (gr-endgraphics))


(cffi:defcfun ("gr_getgraphics" gr-getgraphics) (:pointer :char))

(defun getgraphics ()
  (cffi:foreign-string-to-lisp (gr-getgraphics)))


(cffi:defcfun ("gr_drawgraphics" gr-drawgraphics) :int
  (str (:pointer :char)))

(defun drawgraphics (str)
  (let* ((str-data (string-alloc str))
         (ret (gr-drawgraphics str-data)))
    (string-free str-data)
    ret))


#|
    mathtex(x::Real, y::Real, string)

Generate a character string starting at the given location. Strings can be defined
to create mathematical symbols and Greek letters using LaTeX syntax.

x, y :
    Position of the text string specified in world coordinates
string :
    The text string to be drawn

|#

(cffi:defcfun ("gr_mathtex" gr-mathtex) :void
  (x :double)
  (y :double)
  (str (:pointer :char)))

(defun mathtex (x y str)
  (let ((str-data (string-alloc str)))
    (gr-mathtex (coerce x 'double-float)
                (coerce y 'double-float)
                str-data)
    (string-free str-data)))


(cffi:defcfun ("gr_inqmathtex" gr-inqmathtex) :void
  (x :double)
  (y :double)
  (str (:pointer :char))
  (tbx (:pointer :double))
  (tby (:pointer :double)))

(defun inqmathtex (x y str)
  (let ((str-data (string-alloc str))
        (tbx-data (data-alloc '(0 0 0 0) :double))
        (tby-data (data-alloc '(0 0 0 0) :double)))
    (gr-inqmathtex (coerce x 'double-float)
                   (coerce y 'double-float)
                   str-data
                   tbx-data
                   tby-data)
    (let ((tbx (loop for i below 4
                     collect (arr-aref tbx-data :double i)))
          (tby (loop for j below 4
                     collect (arr-aref tby-data :double j))))
      (string-free str-data)
      (free tbx-data tby-data)
      (list tbx tby))))


(cffi:defcfun ("gr_setregenflags" gr-setregenflags) :void
  (flags :int))

(defun setregenflags (&rest flags)
  (gr-setregenflags (if (null flags)
                        flags
                        0)))


(cffi:defcfun ("gr_inqregenflags" gr-inqregenflags) :int)

(defun inqregenflags ()
  (gr-inqregenflags))


(cffi:defcfun ("gr_savestate" gr-savestate) :void)

(defun savestate ()
  (gr-savestate))


(cffi:defcfun ("gr_restorestate" gr-restorestate) :void)

(defun restorestate ()
  (gr-restorestate))


(cffi:defcfun ("gr_selectcontext" gr-selectcontext) :void
  (context :int))

(defun selectcontext (context)
  (gr-selectcontext context))


(cffi:defcfun ("gr_destroycontext" gr-destroycontext) :void
  (context :int))

(defun destroycontext (context)
  (gr-selectcontext context))


(cffi:defcfun ("gr_uselinespec" gr-uselinespec) :int
  (linespec (:pointer :char)))

(defun uselinespec (linespec)
  (let* ((linespec-data (string-alloc linespec))
         (ret (gr-uselinespec linespec-data)))
    (string-free linespec-data)
    ret))


(cffi:defcfun ("gr_delaunay" gr-delaunay) :void
  (npoints :int)
  (x (:pointer :double))
  (y (:pointer :double))
  (ntri (:pointer :int))
  (triangles (:pointer (:pointer :int))))

(defun delaunay (x y)
  (assert (= (length x)
             (length y)))
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double))
        (dim-data (data-alloc '(3) :int))
        (ntri-data (data-alloc '(0) :int))
        (triangles-data (cffi:foreign-alloc :poiter
                                            :initial-element
                                            (data-alloc '(0) :int))))
    (gr-delaunay (length x)
                 x-data
                 y-data
                 ntri-data
                 triangles-data)
    (let* ((-ntri (arr-aref ntri-data :int 0))
           (-tri (loop for i below (arr-aref dim-data :int 0)
                       collect (loop for j below -ntri
                                     collect (arr-aref (arr-aref triangles-data :pointer 0)
                                                       :int
                                                       (+ j
                                                          (* i -ntri)))))))
      (free x-data
            y-data
            dim-data
            ntri-data
            triangles-data)
      (list -ntri
            -tri))))


(cffi:defcfun ("gr_reducepoints" gr-reducepoints) :void
  (n :int)
  (x (:pointer :double))
  (y (:pointer :double))
  (points :int)
  (x-array (:pointer :double))
  (y-array (:pointer :double)))

(defun reducepoints (xd yd n)
  (assert (= (length xd)
             (length yd)))
  (let* ((nd (length xd))
         (xd-data (data-alloc xd :double))
         (yd-data (data-alloc yd :double))
         (x (data-alloc (loop for i from 1 to n
                              collect i)
                        :double))
         (y (data-alloc (loop for i from 1 to n
                              collect i)
                        :double)))
    (gr-reducepoints nd
                     xd-data
                     yd-data
                     n
                     x
                     y)
    (let ((-x (loop for i below n
                    collect (arr-aref x :double i)))
          (-y (loop for i below n
                    collect (arr-aref y :double i))))
      (free xd-data
            yd-data
            x
            y)
      (list -x -y))))


#|
    trisurface(x, y, z)

Draw a triangular surface plot for the given data points.

x :
    A list containing the X coordinates
y :
    A list containing the Y coordinates
z :
    A list containing the Z coordinates

|#

(cffi:defcfun ("gr_trisurface" gr-trisurface) :void
  (n :int)
  (px (:pointer :double))
  (py (:pointer :double))
  (pz (:pointer :double)))

(defun trisurface (x y z)
  (let ((n (min (length x)
                (length y)
                (length z)))
        (x-data (data-alloc x :double))
        (y-data (data-alloc y :double))
        (z-data (data-alloc z :double)))
    (gr-trisurface n
                   x-data
                   y-data
                   z-data)
    (free x-data
          y-data
          z-data)))


(cffi:defcfun ("gr_gradient" gr-gradient) :void
  (nx :int)
  (ny :int)
  (x (:pointer :double))
  (y (:pointer :double))
  (z (:pointer :double))
  (u (:pointer :double))
  (v (:pointer :double)))

(defun gradient (x y z)
  (assert (= (length (flatten z))
             (* (length x) (length y))))
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double))
        (z-data (data-alloc (flatten z) :double))
        (u-data (data-alloc (loop for i from 1 to (* (length x)
                                                     (length y))
                                  collect i)
                            :double))
        (v-data (data-alloc (loop for i from 1 to (* (length x)
                                                     (length y))
                                  collect i)
                            :double)))
    (gr-gradient (length x)
                 (length y)
                 x-data
                 y-data
                 z-data
                 u-data
                 v-data)
    (let ((u (loop for i below (length y)
                   collect (loop for j below (length x)
                                 collect (arr-aref u-data
                                                   :double
                                                   (+ j
                                                      (* i (length x)))))))
          (v (loop for i below (length y)
                   collect (loop for j below (length x)
                                 collect (arr-aref v-data
                                                   :double
                                                   (+ j
                                                      (* i (length x))))))))
      (free x-data
            y-data
            z-data
            u-data
            v-data)
      (list u v))))


(cffi:defcfun ("gr_quiver" gr-quiver) :void
  (nx :int)
  (ny :int)
  (x (:pointer :double))
  (y (:pointer :double))
  (u (:pointer :double))
  (v (:pointer :double))
  (color :int))

(defun quiver (x y u v &key (color t))
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double))
        (u-data (data-alloc (flatten u) :double))
        (v-data (data-alloc (flatten v) :double))
        (c (if color 1 0)))
    (gr-quiver (length x)
               (length y)
               x-data
               y-data
               u-data
               v-data
               c)
    (free x-data
          y-data
          u-data
          v-data)))


#|
    interp2(x y z xq yq method=0 extrapval=0 )

Interpolation in two dimensions using one of four different methods.
The input points are located on a grid, described by `nx`, `ny`, `x`, `y` and `z`.
The target grid ist described by `nxq`, `nyq`, `xq` and `yq` and the output
is written to `zq` as a field of `nxq * nyq` values.

nx :
    The number of the input grid's x-values
ny :
    The number of the input grid's y-values
x :
    Pointer to the input grid's x-values
y :
    Pointer to the input grid's y-values
z : 
    Pointer to the input grid's z-values (num. of values: nx * ny)
nxq :
    The number of the target grid's x-values
nyq :
    The number of the target grid's y-values
xq :
    Pointer to the target grid's x-values
yq :
    Pointer to the target grid's y-values
zq :
    Pointer to the target grids's z-values, used for output
method :
    Used method for interpolation
extrapval :
    The extrapolation value

The available methods for interpolation are the following:

+-----------------+---+-------------------------------------------+
| INTERP2_NEAREST | 0 | Nearest neighbour interpolation           |
+-----------------+---+-------------------------------------------+
| INTERP2_LINEAR  | 1 | Linear interpolation                      |
+-----------------+---+-------------------------------------------+
| INTERP_2_SPLINE | 2 | Interpolation using natural cubic splines |
+-----------------+---+-------------------------------------------+
| INTERP2_CUBIC   | 3 | Cubic interpolation                       |
+-----------------+---+-------------------------------------------+


|#

(cffi:defcfun ("gr_interp2" gr-interp2) :void
  (nx :int)
  (ny :int)
  (x (:pointer :double))
  (y (:pointer :double))
  (z (:pointer :double))
  (nxq :int)
  (nyq :int)
  (xq (:pointer :double))
  (yq (:pointer :double))
  (zq (:pointer :double))
  (method :int)
  (extrapval :double))

(defun interp2 (x y z xq yq &key (method 0) (extrapval 0))
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double))
        (z-data (data-alloc z :double))
        (xq-data (data-alloc xq :double))
        (yq-data (data-alloc yq :double))
        (zq-data (data-alloc (loop for i from 1 to (* (length x)
                                                      (length y))
                                   collect i)
                             :double)))
    (gr-interp2 (length x)
                (length y)
                x-data
                y-data
                z-data
                (length xq)
                (length yq)
                xq-data
                yq-data
                zq-data
                method
                (coerce extrapval 'double-float))
    (free x-data
          y-data
          z-data
          xq-data
          yq-data
          zq-data)))


(cffi:defcfun ("gr_version" gr-version) (:pointer :char))

(defun version ()
  (cffi:foreign-string-to-lisp (gr-version)))


#|
    shade()

+-----------------+---+
| XFORM_BOOLEAN   | 0 |
+-----------------+---+
| XFORM_LINEAR    | 1 |
+-----------------+---+
| XFORM_LOG       | 2 |
+-----------------+---+
| XFORM_LOGLOG    | 3 |
+-----------------+---+
| XFORM_CUBIC     | 4 |
+-----------------+---+
| XFORM_EQUALIZED | 5 |
+-----------------+---+

|#

(cffi:defcfun ("gr_shade" gr-shade) :void
  (n :int)
  (x (:pointer :double))
  (y (:pointer :double))
  (lines :int)
  (xform :int)
  (roi (:pointer :double))
  (w :int)
  (h :int)
  (bins (:pointer :int)))

(defun shade (x y roi w h &key (lines 0) (xform 0))
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double))
        (roi-data (data-alloc roi :double))
        (bins-data (data-alloc (loop for i below (* w h)
                                     collect 0)
                               :int)))
    (gr-shade (min (length x) (length y))
              x-data
              y-data
              lines
              xform
              roi-data
              w
              h
              bins-data)
    (free x-data
          y-data
          roi-data
          bins-data)))


#|
    shadepoints(x y dims=[1200 1200] xform=1)

Display a point set as a aggregated and rasterized image.

x :
    A pointer to the X coordinates
y :
    A pointer to the Y coordinates
w :
    The width of the grid used for rasterization
h :
    The height of the grid used for rasterization
xform :
    The transformation type used for color mapping

The values for `x` and `y` are in world coordinates.

The available transformation types are:

+----------------+---+--------------------+
|XFORM_BOOLEAN   |  0|boolean             |
+----------------+---+--------------------+
|XFORM_LINEAR    |  1|linear              |
+----------------+---+--------------------+
|XFORM_LOG       |  2|logarithmic         |
+----------------+---+--------------------+
|XFORM_LOGLOG    |  3|double logarithmic  |
+----------------+---+--------------------+
|XFORM_CUBIC     |  4|cubic               |
+----------------+---+--------------------+
|XFORM_EQUALIZED |  5|histogram equalized |
+----------------+---+--------------------+
|#


(cffi:defcfun ("gr_shadepoints" gr-shadepoints) :void
  (n :int)
  (x (:pointer :double))
  (y (:pointer :double))
  (xform :int)
  (w :int)
  (h :int))

(defun shadepoints (x y w h &key (xform 1))
  (assert (= (length x)
             (length y)))
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double)))
    (gr-shadepoints (length x)
                    x-data
                    y-data
                    xform
                    w
                    h)
    (free x-data
          y-data)))


(cffi:defcfun ("gr_shadelines" gr-shadelines) :void
  (n :int)
  (x (:pointer :double))
  (y (:pointer :double))
  (xform :int)
  (w :int)
  (h :int))

(defun shadelines (x y w h &key (xform 1))
  (assert (= (length x)
             (length y)))
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double)))
    (gr-shadelines (length x)
                   x-data
                   y-data
                   xform
                   w
                   h)
    (free x-data
          y-data)))


(cffi:defcfun ("gr_panzoom" gr-panzoom) :void
  (x :double)
  (y :double)
  (xzoom :double)
  (yzoom :double)
  (xmin (:pointer :double))
  (xmax (:pointer :double))
  (ymin (:pointer :double))
  (ymax (:pointer :double)))

(defun panzoom (x y zoom)
  (let ((xmin-data (data-alloc '(0) :double))
        (xmax-data (data-alloc '(0) :double))
        (ymin-data (data-alloc '(0) :double))
        (ymax-data (data-alloc '(0) :double)))
    (gr-panzoom (coerce x 'double-float)
                (coerce y 'double-float)
                (coerce zoom 'double-float)
                (coerce zoom 'double-float)
                xmin-data
                xmax-data
                ymin-data
                ymax-data)
    (free xmin-data
          xmax-data
          ymin-data
          ymax-data)))


#|
    path(x, y, codes)

Draw paths using the given vertices and path codes.

`x` :
    A list containing the X coordinates
`y` :
    A list containing the Y coordinates
`codes` :
    A list containing the path codes

The values for `x` and `y` are in world coordinates.
The `codes` describe several path primitives that can be used to create compound paths.
The following path codes are recognized:

+----------+---------------------------------+-------------------+-------------------+
|   Code   |          Description            |         X         |         Y         |
+----------+---------------------------------+-------------------+-------------------+
|     M, m | move                            | x                 | y                 |
+----------+---------------------------------+-------------------+-------------------+
|     L, l | line                            | x                 | y                 |
+----------+---------------------------------+-------------------+-------------------+
|     Q, q | quadratic Bezier                | x1, x2            | y1, y2            |
+----------+---------------------------------+-------------------+-------------------+
|     C, c | cubic Bezier                    | x1, x2, x3        | y1, y2, y3        |
+----------+---------------------------------+-------------------+-------------------+
|     A, a | arc                             | rx, a1, reserved  | ry, a2, reserved  |
+----------+---------------------------------+-------------------+-------------------+
|        Z | close path                      |                   |                   |
+----------+---------------------------------+-------------------+-------------------+
|        S | stroke                          |                   |                   |
+----------+---------------------------------+-------------------+-------------------+
|        s | close path and stroke           |                   |                   |
+----------+---------------------------------+-------------------+-------------------+
|        f | close path and fill             |                   |                   |
+----------+---------------------------------+-------------------+-------------------+
|        F | close path, fill and stroke     |                   |                   |
+----------+---------------------------------+-------------------+-------------------+

- Move: `M`, `m`
   Moves the current position to (`x`, `y`). The new position is either absolute (`M`) or relative to the current
   position (`m`). The initial position of :code:`path` is (0, 0).

   Example:

   >>> (path '(0.5 -0.1) '(0.2 0.1) "Mm")

   The first move command in this example moves the current position to the absolute coordinates (0.5, 0.2). The
   second move to performs a movement by (-0.1, 0.1) relative to the current position resulting in the point
   (0.4, 0.3).

- Line: `L`, `l`
   Draws a line from the current position to the given position (`x`, `y`). The end point of the line is either
   absolute (`L`) or relative to the current position (`l`). The current position is set to the end point of the
   line.

   Example:

   >>> (path '(0.1 0.5 0.0) '(0.1 0.1 0.2) "MLlS")

   The first line to command draws a straight line from the current position (0.1, 0.1) to the absolute position
   (0.5, 0.1) resulting in a horizontal line. The second line to command draws a vertical line relative to the
   current position resulting in the end point (0.5, 0.3).

- Quadratic Bezier curve: `Q`, `q`
   Draws a quadratic bezier curve from the current position to the end point (`x2`, `y2`) using (`x1`, `y1`) as the
   control point. Both points are either absolute (`Q`) or relative to the current position (`q`). The current
 
   position is set to the end point of the bezier curve.

   Example:

   >>> (path '(0.1 0.3 0.5 0.2 0.4) '(0.1 0.2 0.1 0.1 0.0) "MQqS")

   This example will generate two bezier curves whose start and end points are each located at y=0.1. As the control
   points are horizontally in the middle of each bezier curve with a higher y value both curves are symmetrical
   and bend slightly upwards in the middle. The current position is set to (0.9, 0.1) at the end.

- Cubic Bezier curve: `C`, `c`
   Draws a cubic bezier curve from the current position to the end point (`x3`, `y3`) using (`x1`, `y1`) and
   (`x2`, `y2`) as the control points. All three points are either absolute (`C`) or relative to the current position
   (`c`). The current position is set to the end point of the bezier curve.

   Example:

   >>> (path '(0.1 0.2 0.3 0.4 0.1 0.2 0.3)
   ...       '(0.1 0.2 0.0 0.1 0.1 -0.1 0.0)
   ...       "MCcS")

   This example will generate two bezier curves whose start and end points are each located at y=0.1. As the control
   points are equally spaced along the x-axis and the first is above and the second is below the start and end
   points this creates a wave-like shape for both bezier curves. The current position is set to (0.8, 0.1) at the
   end.

- Ellipctical arc: `A`, `a`
   Draws an elliptical arc starting at the current position. The major axis of the ellipse is aligned with the x-axis
   and the minor axis is aligned with the y-axis of the plot. `rx` and `ry` are the ellipses radii along the major
   and minor axis. `a1` and `a2` define the start and end angle of the arc in radians. The current position is set
   to the end point of the arc. If `a2` is greater than `a1` the arc is drawn counter-clockwise, otherwise it is
   drawn clockwise. The `a` and `A` commands draw the same arc. The third coordinates of the `x` and `y` array are
   ignored and reserved for future use.

   Examples:

   >>> (path '(0.1 0.2 (/ -3.14159 2) 0.0) '(0.1 0.4 (/ 3.14159 2) 0.0) "MAS")

   This example draws an arc starting at (0.1, 0.1). As the start angle -pi/2 is smaller than the end angle pi/2 the
   arc is drawn counter-clockwise. In this case the right half of an ellipse with an x radius of 0.2 and a y radius
   of 0.4 is shown. Therefore the current position is set to (0.1, 0.9) at the end.
   >>> path([0.1, 0.2, 3.14159 / 2, 0.0], [0.9, 0.4, -3.14159 / 2, 0.0], "MAS")
   This examples draws the same arc as the previous one. The only difference is that the starting point is now at
   (0.1, 0.9) and the start angle pi/2 is greater than the end angle -pi/2 so that the ellipse arc is drawn
   clockwise. Therefore the current position is set to (0.1, 0.1) at the end.

- Close path: `Z`
   Closes the current path by connecting the current position to the target position of the last move command
   (`m` or `M`) with a straight line. If no move to was performed in this path it connects the current position to
   (0, 0). When the path is stroked this line will also be drawn.

- Stroke path: `S`, `s`
   Strokes the path with the current border width and border color (set with :code:`gr.setborderwidth` and
   :code:`gr.setbordercolorind`). In case of `s` the path is closed beforehand, which is equivalent to `ZS`.

- Fill path: `F`, `f`
   Fills the current path using the even-odd-rule using the current fill color. Filling a path implicitly closes
   the path. The fill color can be set using :code:`gr.setfillcolorind`. In case of `F` the path is also
   stroked using the current border width and color afterwards.

|#

(cffi:defcfun ("gr_path" gr-path) :void
  (n :int)
  (x (:pointer :double))
  (y (:pointer :double))
  (codes (:pointer :char)))

(defun path (x y codes)
  (assert (= (length x)
             (length y)))
  (let ((x-data (data-alloc x :double))
        (y-data (data-alloc y :double))
        (code-data (string-alloc codes)))
    (gr-path (length x)
             x-data
             y-data
             code-data)
    (free x-data y-data)
    (string-free code-data)))


#|
    setborderwidth(width::Real)

Define the border width of subsequent path output primitives.

`width` :
    The border width scale factor

|#

(cffi:defcfun ("gr_setborderwidth" gr-setborderwidth) :void
  (width :double))

(defun setborderwidth (width)
  (gr-setborderwidth (coerce width 'double-float)))


#|
    setbordercolorind(color::Int)

Define the color of subsequent path output primitives.

`color` :
    The border color index (COLOR < 1256)

|#

(cffi:defcfun ("gr_setbordercolorind" gr-setbordercolorind) :void
  (color :int))

(defun setbordercolorind (color)
  (gr-setbordercolorind color))


#|
    setprojectiontype(int flag)

Set the projection type with this flag.

flag :
    projection type

The available options are:

+---------------------------+---+--------------+
|GR_PROJECTION_DEFAULT      |  0|default       |
+---------------------------+---+--------------+
|GR_PROJECTION_ORTHOGRAPHIC |  1|orthographic  |
+---------------------------+---+--------------+
|GR_PROJECTION_PERSPECTIVE  |  2|perspective    |
+---------------------------+---+--------------+

|#

(cffi:defcfun ("gr_setprojectiontype" gr-setprojectiontype) :void
  (flag :int))

(defun setprojectiontype (flag)
  (gr-setprojectiontype flag))


(cffi:defcfun ("gr_setperspectiveprojection" gr-setperspectiveprojection) :void
  (near-plane :double)
  (far-plane :double)
  (fov :double))

(defun setperspectiveprojection (near far fov)
  (gr-setperspectiveprojection (coerce near 'double-float)
                               (coerce far 'double-float)
                               (coerce fov 'double-float)))


(cffi:defcfun ("gr_settransformationparameters" gr-settransformationparameters) :void
  (camera-pos-x :double)
  (camera-pos-y :double)
  (camera-pos-z :double)
  (up-x :double)
  (up-y :double)
  (up-z :double)
  (focus-point-x :double)
  (focus-point-y :double)
  (focus-point-z :double))

(defun settransformationparameters (camera-x camera-y camera-z
                                    up-x up-y up-z
                                    focus-x focus-y focus-z)
  (gr-settransformationparameters (coerce camera-x 'double-float)
                                  (coerce camera-y 'double-float)
                                  (coerce camera-z 'double-float)
                                  (coerce up-x 'double-float)
                                  (coerce up-y 'double-float)
                                  (coerce up-z 'double-float)
                                  (coerce focus-x 'double-float)
                                  (coerce focus-y 'double-float)
                                  (coerce focus-z 'double-float)))


(cffi:defcfun ("gr_setorthographicprojection" gr-setorthographicprojection) :void
  (left :double)
  (right :double)
  (bottom :double)
  (top :double)
  (near :double)
  (far :double))

(defun setorthographicprojection (left right bottom top
                                  near far)
  (gr-setorthographicprojection (coerce left 'double-float)
                                (coerce right 'double-float)
                                (coerce bottom 'double-float)
                                (coerce top 'double-float)
                                (coerce near 'double-float)
                                (coerce far 'double-float)))


(cffi:defcfun ("gr_setwindow3d" gr-setwindow3d) :void
  (xmin :double)
  (xmax :double)
  (ymin :double)
  (ymax :double)
  (zmin :double)
  (zmax :double))

(defun setwindow3d (xmin xmax ymin ymax zmin zmax)
  (gr-setwindow3d (coerce xmin 'double-float)
                  (coerce xmax 'double-float)
                  (coerce ymin 'double-float)
                  (coerce ymax 'double-float)
                  (coerce zmin 'double-float)
                  (coerce zmax 'double-float)))


