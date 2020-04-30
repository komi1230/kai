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
(defpackage :kai.converter
  (:use :cl)
  (:export :to-json))
(in-package :kai.converter)



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


;;;; Ploly converter
;;;
;;; Plotly accepts data just like json, so here we provide
;;; convert latent expression generated by interface.lisp
;;; to json-like style.


(defun plotly-line (data)
  (symbol-downcase
   `(:x ,(cdr (assoc :x data))
     :y ,(cdr (assoc :y data))
     :type "scatter"
     :mode "lines"
     :line ,(symbol-downcase
             `(:color ,(get-color (cdr (assoc :color data)))
               :width ,(cdr (assoc :width data)))))))


(defun plotly-marker (data)
  (symbol-downcase
   `(:x ,(cdr (assoc :x data))
     :y ,(cdr (assoc :y data))
     :type "scatter"
     :mode "markers"
     :marker ,(symbol-downcase
               `(:color ,(get-color (cdr (assoc :color data)))
                 :size ,(cdr (assoc :size data)))))))


(defun plotly-fill (data)
  (list
   (symbol-downcase
    `(:x ,(cdr (assoc :x data))
      :y ,(cdr (assoc :y0 data))
      :type "scatter"
      :mode "lines"
      :fill "tonexty"
      :line (:color ,(get-color (cdr (assoc :color data))))))
   (symbol-downcase
    `(:x ,(cdr (assoc :x data))
      :y ,(cdr (assoc :y1 data))
      :type "scatter"
      :mode "lines"
      :fill "tonexty"
      :line ,(symbol-downcase
              `(:color ,(get-color (cdr (assoc :color data)))))))))


(defun plotly-errorbar (data)
  (symbol-downcase
   `(:x ,(cdr (assoc :x data))
     :y ,(cdr (assoc :y data))
     :type "scatter"
     :mode "marker"
     :fill "tonexty"
     :marker (:color ,(get-color (cdr (assoc :color data))))
     ,@(let ((errx (assoc :error-x data)))
         (if errx
             (symbol-downcase
              `(:error_x
                ,(symbol-downcase
                  `(:type "data"
                    :symmetric :false
                    :color ,(get-color (cdr (assoc :color data)))
                    ,@(if (consp (cadr errx))
                          (list :array (caddr errx)
                                :arrayminus (cadr errx))
                          (list :array (cdr errx)))))))))
     ,@(let ((erry (assoc :error-y data)))
         (if erry
             (symbol-downcase
              `(:error_y
                (symbol-downcase
                 `(:type "data"
                   :symmetric :false
                   :color ,(get-color (cdr (assoc :color data)))
                   ,@(if (consp (cadr erry))
                         (list :array (caddr erry)
                               :arrayminus (cadr erry))
                         (list :array (cdr erry))))))))))))


(defun to-json (param)
  (let ((jonathan:*false-value* :false)
        (jonathan:*null-value* :null)
        (jonathan:*empty-array-value* :empty-array)
        (jonathan:*empty-object-value* :empty-object))
    (jonathan:to-json param)))





;;;; GR regularizer
;;;
;;; Accordings in GR is expressed with relative values between
;;; 0 and 1. Here we provide accordings regularizer.

(defun min-max (lst)
  (cons (apply #'min lst)
        (apply #'max lst)))


(defun regularize (lst)
  (let* ((tmp-min-max (min-max lst))
         (range (- (cdr tmp-min-max)
                   (car tmp-min-max))))
    (mapcar #'(lambda (x)
                (/ (- x (car tmp-min-max))
                   range))
            lst)))



;;;; Sort argument data
;;;
;;; To plot sorted data, provide sort functions for Multiple
;;; arguments.

(defun sort-data (&rest data)
  (labels ((concat (ls)
             (if (every #'null ls)
                 nil
                 (cons (mapcar #'car ls)
                       (concat (mapcar #'cdr ls)))))
           (sort-l (l)
             (sort (copy-list l)
                   #'(lambda (x y)
                      (< (car x) (car y)))))
           (separate (l)
             (case (length (car l))
               (2 (list (mapcar #'first l)
                        (mapcar #'second l)))
               (3 (list (mapcar #'first l)
                        (mapcar #'second l)
                        (mapcar #'third l))))))
    (separate (sort-l (concat data)))))
