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
(defpackage :kai.interface
  (:use :cl)
  (:import-from :kai.util
                :convert-data
                :symbol-downcase
                :check-file-exist
                :make-kai-cache)
  (:import-from :kai.plotly.generate
                :download-plotlyjs
                :save-html
                :save-js)
  (:import-from :kai.plotly.launch
                :open-browser)
  (:export :*state*
           :*style*
           :reset!
           :scatter
           :pie
           :sunburst
           :box
           :heatmap
           :contour
           :scatter3d
           :surface
           :style
           :show))
(in-package :kai.interface)



;;;; State
;;;
;;; To make it able to plot multiple graph, we have a state as list.

(defparameter *state* '())

(defparameter *style* '())

(defun reset! ()
  (setf *state* '())
  (setf *style* '()))


;;;; Color
;;;
;;; List of color code

(defparameter *colors* (make-hash-table))

(setf (gethash (intern "ALICEBLUE") *colors*) "#F0F8FF"
      (gethash (intern "ANTIQUEWHITE") *colors*) "#FAEBD7"
      (gethash (intern "AQUA") *colors*) "#00FFFF"
      (gethash (intern "AQUAMARINE") *colors*) "#7FFFD4"
      (gethash (intern "AZURE") *colors*) "#F0FFFF"
      (gethash (intern "BEIGE") *colors*) "#F5F5DC"
      (gethash (intern "BISQUE") *colors*) "#FFE4C4"
      (gethash (intern "BLACK") *colors*) "#000000"
      (gethash (intern "BLANCHEDALMOND") *colors*) "#FFEBCD"
      (gethash (intern "BLUE") *colors*) "#0000FF"
      (gethash (intern "BLUEVIOLET") *colors*) "#8A2BE2"
      (gethash (intern "BROWN") *colors*) "#A52A2A"
      (gethash (intern "BURLYWOOD") *colors*) "#DEB887"
      (gethash (intern "CADETBLUE") *colors*) "#5F9EA0"
      (gethash (intern "CHARTREUSE") *colors*) "#7FFF00"
      (gethash (intern "CHOCOLATE") *colors*) "#D2691E"
      (gethash (intern "CORAL") *colors*) "#FF7F50"
      (gethash (intern "CORNFLOWERBLUE") *colors*) "#6495ED"
      (gethash (intern "CORNSILK") *colors*) "#FFF8DC"
      (gethash (intern "CRIMSON") *colors*) "#DC143C"
      (gethash (intern "CYAN") *colors*) "#00FFFF"
      (gethash (intern "DARKBLUE") *colors*) "#00008B"
      (gethash (intern "DARKCYAN") *colors*) "#008B8B"
      (gethash (intern "DARKGOLDENROD") *colors*) "#B8860B"
      (gethash (intern "DARKGRAY") *colors*) "#A9A9A9"
      (gethash (intern "DARKGREEN") *colors*) "#006400"
      (gethash (intern "DARKGREY") *colors*) "#A9A9A9"
      (gethash (intern "DARKKHAKI") *colors*) "#BDB76B"
      (gethash (intern "DARKMAGENTA") *colors*) "#8B008B"
      (gethash (intern "DARKOLIVEGREEN") *colors*) "#556B2F"
      (gethash (intern "DARKORANGE") *colors*) "#FF8C00"
      (gethash (intern "DARKORCHID") *colors*) "#9932CC"
      (gethash (intern "DARKRED") *colors*) "#8B0000"
      (gethash (intern "DARKSALMON") *colors*) "#E9967A"
      (gethash (intern "DARKSEAGREEN") *colors*) "#8FBC8F"
      (gethash (intern "DARKSLATEBLUE") *colors*) "#483D8B"
      (gethash (intern "DARKSLATEGRAY") *colors*) "#2F4F4F"
      (gethash (intern "DARKSLATEGREY") *colors*) "#2F4F4F"
      (gethash (intern "DARKTURQUOISE") *colors*) "#00CED1"
      (gethash (intern "DARKVIOLET") *colors*) "#9400D3"
      (gethash (intern "DEEPPINK") *colors*) "#FF1493"
      (gethash (intern "DEEPSKYBLUE") *colors*) "#00BFFF"
      (gethash (intern "DIMGRAY") *colors*) "#696969"
      (gethash (intern "DIMGREY") *colors*) "#696969"
      (gethash (intern "DODGERBLUE") *colors*) "#1E90FF"
      (gethash (intern "FIREBRICK") *colors*) "#B22222"
      (gethash (intern "FLORALWHITE") *colors*) "#FFFAF0"
      (gethash (intern "FORESTGREEN") *colors*) "#228B22"
      (gethash (intern "FUCHSIA") *colors*) "#FF00FF"
      (gethash (intern "GAINSBORO") *colors*) "#DCDCDC"
      (gethash (intern "GHOSTWHITE") *colors*) "#F8F8FF"
      (gethash (intern "GOLD") *colors*) "#FFD700"
      (gethash (intern "GOLDENROD") *colors*) "#DAA520"
      (gethash (intern "GRAY") *colors*) "#808080"
      (gethash (intern "GREEN") *colors*) "#008000"
      (gethash (intern "GREENYELLOW") *colors*) "#ADFF2F"
      (gethash (intern "GREY") *colors*) "#808080"
      (gethash (intern "HONEYDEW") *colors*) "#F0FFF0"
      (gethash (intern "HOTPINK") *colors*) "#FF69B4"
      (gethash (intern "INDIANRED") *colors*) "#CD5C5C"
      (gethash (intern "INDIGO") *colors*) "#4B0082"
      (gethash (intern "IVORY") *colors*) "#FFFFF0"
      (gethash (intern "KHAKI") *colors*) "#F0E68C"
      (gethash (intern "LAVENDER") *colors*) "#E6E6FA"
      (gethash (intern "LAVENDERBLUSH") *colors*) "#FFF0F5"
      (gethash (intern "LAWNGREEN") *colors*) "#7CFC00"
      (gethash (intern "LEMONCHIFFON") *colors*) "#FFFACD"
      (gethash (intern "LIGHTBLUE") *colors*) "#ADD8E6"
      (gethash (intern "LIGHTCORAL") *colors*) "#F08080"
      (gethash (intern "LIGHTCYAN") *colors*) "#E0FFFF"
      (gethash (intern "LIGHTGOLDENRODYELLOW") *colors*) "#FAFAD2"
      (gethash (intern "LIGHTGRAY") *colors*) "#D3D3D3"
      (gethash (intern "LIGHTGREEN") *colors*) "#90EE90"
      (gethash (intern "LIGHTGREY") *colors*) "#D3D3D3"
      (gethash (intern "LIGHTPINK") *colors*) "#FFB6C1"
      (gethash (intern "LIGHTSALMON") *colors*) "#FFA07A"
      (gethash (intern "LIGHTSEAGREEN") *colors*) "#20B2AA"
      (gethash (intern "LIGHTSKYBLUE") *colors*) "#87CEFA"
      (gethash (intern "LIGHTSLATEGRAY") *colors*) "#778899"
      (gethash (intern "LIGHTSLATEGREY") *colors*) "#778899"
      (gethash (intern "LIGHTSTEELBLUE") *colors*) "#B0C4DE"
      (gethash (intern "LIGHTYELLOW") *colors*) "#FFFFE0"
      (gethash (intern "LIME") *colors*) "#00FF00"
      (gethash (intern "LIMEGREEN") *colors*) "#32CD32"
      (gethash (intern "LINEN") *colors*) "#FAF0E6"
      (gethash (intern "MAGENTA") *colors*) "#FF00FF"
      (gethash (intern "MAROON") *colors*) "#800000"
      (gethash (intern "MEDIUMAQUAMARINE") *colors*) "#66CDAA"
      (gethash (intern "MEDIUMBLUE") *colors*) "#0000CD"
      (gethash (intern "MEDIUMORCHID") *colors*) "#BA55D3"
      (gethash (intern "MEDIUMPURPLE") *colors*) "#9370DB"
      (gethash (intern "MEDIUMSEAGREEN") *colors*) "#3CB371"
      (gethash (intern "MEDIUMSLATEBLUE") *colors*) "#7B68EE"
      (gethash (intern "MEDIUMSPRINGGREEN") *colors*) "#00FA9A"
      (gethash (intern "MEDIUMTURQUOISE") *colors*) "#48D1CC"
      (gethash (intern "MEDIUMVIOLETRED") *colors*) "#C71585"
      (gethash (intern "MIDNIGHTBLUE") *colors*) "#191970"
      (gethash (intern "MINTCREAM") *colors*) "#F5FFFA"
      (gethash (intern "MISTYROSE") *colors*) "#FFE4E1"
      (gethash (intern "MOCCASIN") *colors*) "#FFE4B5"
      (gethash (intern "NAVAJOWHITE") *colors*) "#FFDEAD"
      (gethash (intern "NAVY") *colors*) "#000080"
      (gethash (intern "OLDLACE") *colors*) "#FDF5E6"
      (gethash (intern "OLIVE") *colors*) "#808000"
      (gethash (intern "OLIVEDRAB") *colors*) "#6B8E23"
      (gethash (intern "ORANGE") *colors*) "#FFA500"
      (gethash (intern "ORANGERED") *colors*) "#FF4500"
      (gethash (intern "ORCHID") *colors*) "#DA70D6"
      (gethash (intern "PALEGOLDENROD") *colors*) "#EEE8AA"
      (gethash (intern "PALEGREEN") *colors*) "#98FB98"
      (gethash (intern "PALETURQUOISE") *colors*) "#AFEEEE"
      (gethash (intern "PALEVIOLETRED") *colors*) "#DB7093"
      (gethash (intern "PAPAYAWHIP") *colors*) "#FFEFD5"
      (gethash (intern "PEACHPUFF") *colors*) "#FFDAB9"
      (gethash (intern "PERU") *colors*) "#CD853F"
      (gethash (intern "PINK") *colors*) "#FFC0CB"
      (gethash (intern "PLUM") *colors*) "#DDA0DD"
      (gethash (intern "POWDERBLUE") *colors*) "#B0E0E6"
      (gethash (intern "PURPLE") *colors*) "#800080"
      (gethash (intern "REBECCAPURPLE") *colors*) "#663399"
      (gethash (intern "RED") *colors*) "#FF0000"
      (gethash (intern "ROSYBROWN") *colors*) "#BC8F8F"
      (gethash (intern "ROYALBLUE") *colors*) "#4169E1"
      (gethash (intern "SADDLEBROWN") *colors*) "#8B4513"
      (gethash (intern "SALMON") *colors*) "#FA8072"
      (gethash (intern "SANDYBROWN") *colors*) "#F4A460"
      (gethash (intern "SEAGREEN") *colors*) "#2E8B57"
      (gethash (intern "SEASHELL") *colors*) "#FFF5EE"
      (gethash (intern "SIENNA") *colors*) "#A0522D"
      (gethash (intern "SILVER") *colors*) "#C0C0C0"
      (gethash (intern "SKYBLUE") *colors*) "#87CEEB"
      (gethash (intern "SLATEBLUE") *colors*) "#6A5ACD"
      (gethash (intern "SLATEGRAY") *colors*) "#708090"
      (gethash (intern "SLATEGREY") *colors*) "#708090"
      (gethash (intern "SNOW") *colors*) "#FFFAFA"
      (gethash (intern "SPRINGGREEN") *colors*) "#00FF7F"
      (gethash (intern "STEELBLUE") *colors*) "#4682B4"
      (gethash (intern "TAN") *colors*) "#D2B48C"
      (gethash (intern "TEAL") *colors*) "#008080"
      (gethash (intern "THISTLE") *colors*) "#D8BFD8"
      (gethash (intern "TOMATO") *colors*) "#FF6347"
      (gethash (intern "TURQUOISE") *colors*) "#40E0D0"
      (gethash (intern "VIOLET") *colors*) "#EE82EE"
      (gethash (intern "WHEAT") *colors*) "#F5DEB3"
      (gethash (intern "WHITE") *colors*) "#FFFFFF"
      (gethash (intern "WHITESMOKE") *colors*) "#F5F5F5"
      (gethash (intern "YELLOW") *colors*) "#FFFF00"
      (gethash (intern "YELLOWGREEN") *colors*) "#9ACD32")

(defun get-color (c)
  (gethash (intern (string-upcase c))
           *colors*))


;;;; Scatter and Line
;;;
;;; This covers scatter and line plotting and their options.

;; 2D scatter
(defun scatter (&rest data)
  (push (apply #'-scatter (apply #'convert-data data))
        *state*)
  T)

(defun -scatter (x
                 y
                 &key
                   (mode "lines")
                   (name "")
                   (text '())
                   (error-x '())
                   (error-y '())
                   (fill "")
                   (fillcolor "")
                   (line '())
                   (marker '()))
  (symbol-downcase
   `( :x ,x
      :y ,y
      :type "scatter"
      :mode ,mode
      ,@(if (not (string= name ""))
            (list :name name))
      ,@(if (not (null text))
            (list :text text))
      ,@(if (not (null error-x))
            (list :error_x error-x))
      ,@(if (not (null error-y))
            (list :error_y error-y))
      ,@(if (not (string= fill ""))
            (list :fill fill))
      ,@(if (not (string= fillcolor ""))
            (list :fillcolor fillcolor))
      ,@(if (not (null line))
            (list :line (symbol-downcase line)))
      ,@(if (not (null marker))
            (list :marker (symbol-downcase marker))))))


;; Bar plot
(defun bar (&rest data)
  (push (apply #'-bar (apply #'convert-data data))
        *state*)
  T)

(defun -bar (x
             y
             &key
               (name "")
               (text '())
               (error-x '())
               (error-y '())
               (fill "")
               (fillcolor "")
               (marker '()))
  (symbol-downcase
   `( :x ,x
      :y ,y
      :type "bar"
      ,@(if (not (string= name ""))
            (list :name name))
      ,@(if (not (null text))
            (list :text text))
      ,@(if (not (null error-x))
            (list :error_x error-x))
      ,@(if (not (null error-y))
            (list :error_y error-y))
      ,@(if (not (string= fill ""))
            (list :fill fill))
      ,@(if (not (string= fillcolor ""))
            (list :fillcolor fillcolor))
      ,@(if (not (null marker))
            (list :marker (symbol-downcase marker))))))


;; Pie chart
(defun pie (&rest data)
  (push (apply #'-pie data)
        *state*)
  T)

(defun -pie (values
             labels
             &key
               (name ""))
  (symbol-downcase
   `(:values ,values
     :labels ,labels
     ,@(if (not (string= name ""))
           (list :name name)))))


;; Sunburst
(defun sunburst (&rest data)
  (push (apply #'-sunburst data)
        *state*)
  T)

(defun -sunburst (values
                  labels
                  parents
                  &key
                    (marker '()))
  (symbol-downcase
   `(:value ,values
     :label ,labels
     :parents ,parents
     :type "sunburst"
     ,@(if (not (null marker))
           (list :marker (symbol-downcase marker))))))


;; Box plots
(defun box (&rest data)
  (push (apply #'-box data)
        *state*)
  T)

(defun -box (y
             &key
               (name "")
               (marker '())
               (boxmean t)
               (boxpoints :false))
  (symbol-downcase
   `(:y ,y
     :type "box"
     :boxmean ,boxmean
     :boxpoints ,boxpoints
     ,@(if (not (string= name ""))
           (list :name name))
     ,@(if (not (null marker))
           (list :marker (symbol-downcase marker))))))


;; Heatmap
(defun heatmap (&rest data)
  (push (apply #'-heatmap data)
        *state*)
  T)

(defun -heatmap (z
                 &key
                   (colorscale '())
                   (showscale :false))
  (symbol-downcase
   `(:z ,z
     :type "heatmap"
     :showscale ,showscale
     ,@(if (not (null colorscale))
           (list :marker (symbol-downcase colorscale))))))


;; Contour
(defun contour (&rest data)
  (push (apply #'-contour data)
        *state*)
  T)

(defun -contour (z
                 &key
                   (colorscale '())
                   (showscale :false)
                   (autocontour :false)
                   (contours '()))
  (symbol-downcase
   `(:z ,z
     :type "contour"
     :showscale ,showscale
     :autocontour ,autocontour
     ,@(if (not (null colorscale))
           (list :marker (symbol-downcase colorscale)))
     ,@(if (not (null contours))
           (list :marker (symbol-downcase contours))))))


;; Scatter3D
(defun scatter3d (&rest data)
  (push (apply #'-scatter3d data)
        *state*)
  T)

(defun -scatter3d (x
                   y
                   z
                   &key
                     (mode "markers")
                     (name "")
                     (text '())
                     (marker '())
                     (line '()))
  (symbol-downcase
   `(:x ,x
     :y ,y
     :z ,z
     :type "scatter3d"
     :mode ,mode
     ,@(if (not (string= name ""))
           (list :name name))
     ,@(if (not (null text))
           (list :text text))
     ,@(if (not (null line))
           (list :line (symbol-downcase line)))
     ,@(if (not (null marker))
           (list :marker (symbol-downcase marker))))))


;; Surface
(defun surface (&rest data)
  (push (apply #'-surface data)
        *state*)
  T)

(defun -surface (z
                 &key
                   (name ""))
  (symbol-downcase
   `(:z ,z
     :type "surface"
     ,@(if (not (string= name ""))
           (list :name name)))))



;;;; Layout
;;;
;;; To attach title or axis options to the graph.

(defun style (&key
                (title "")
                (xaxis '())
                (yaxis '()))
  (setf *style*
        `(:title ,title
                 ,@(if (not (null xaxis))
                       (list :xaxis (symbol-downcase xaxis)))
                 ,@(if (not (null yaxis))
                       (list :yaxis (symbol-downcase yaxis)))))
  T)


;;;; Plot
;;;
;;; Launch viewer and draw traces and styles.

(defun show ()
  (ensure-directories-exist
   (make-kai-cache "plotly"))
  (if (not (check-file-exist "plotly"
                             "kai.html"))
      (save-html))
  (if (not (check-file-exist "plotly"
                             "plotly-latest.min.js"))
      (download-plotlyjs))
  (save-js *state* *style*)
  (open-browser)
  (reset!)
  T)
