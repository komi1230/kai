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
  (:import-from :kai.util
                :symbol-downcase)
  (:export :plotly-code))
(in-package :kai.converter)



;;;; Color
;;;
;;; List of color code

(defparameter *colors* (make-hash-table))

(setf (gethash (intern "ALICEBLUE" "KEYWORD") *colors*) '(240 248 255)
      (gethash (intern "ANTIQUEWHITE" "KEYWORD") *colors*) '(250 235 215)
      (gethash (intern "AQUA" "KEYWORD") *colors*) '(0 255 255)
      (gethash (intern "AQUAMARINE" "KEYWORD") *colors*) '(127 255 212)
      (gethash (intern "AZURE" "KEYWORD") *colors*) '(240 255 255)
      (gethash (intern "BEIGE" "KEYWORD") *colors*) '(245 245 220)
      (gethash (intern "BISQUE" "KEYWORD") *colors*) '(255 228 196)
      (gethash (intern "BLACK" "KEYWORD") *colors*) '(0 0 0)
      (gethash (intern "BLANCHEDALMOND" "KEYWORD") *colors*) '(255 235 205)
      (gethash (intern "BLUE" "KEYWORD") *colors*) '(0 0 255)
      (gethash (intern "BLUEVIOLET" "KEYWORD") *colors*) '(138 43 226)
      (gethash (intern "BROWN" "KEYWORD") *colors*) '(165 42 42)
      (gethash (intern "BURLYWOOD" "KEYWORD") *colors*) '(222 184 135)
      (gethash (intern "CADETBLUE" "KEYWORD") *colors*) '(95 158 160)
      (gethash (intern "CHARTREUSE" "KEYWORD") *colors*) '(127 255 0)
      (gethash (intern "CHOCOLATE" "KEYWORD") *colors*) '(210 105 30)
      (gethash (intern "CORAL" "KEYWORD") *colors*) '(255 127 80)
      (gethash (intern "CORNFLOWERBLUE" "KEYWORD") *colors*) '(100 149 237)
      (gethash (intern "CORNSILK" "KEYWORD") *colors*) '(255 248 220)
      (gethash (intern "CRIMSON" "KEYWORD") *colors*) '(220 20 60)
      (gethash (intern "CYAN" "KEYWORD") *colors*) '(0 255 255)
      (gethash (intern "DARKBLUE" "KEYWORD") *colors*) '(0 0 139)
      (gethash (intern "DARKCYAN" "KEYWORD") *colors*) '(0 139 139)
      (gethash (intern "DARKGOLDENROD" "KEYWORD") *colors*) '(184 134 11)
      (gethash (intern "DARKGRAY" "KEYWORD") *colors*) '(169 169 169)
      (gethash (intern "DARKGREEN" "KEYWORD") *colors*) '(0 100 0)
      (gethash (intern "DARKGREY" "KEYWORD") *colors*) '(169 169 169)
      (gethash (intern "DARKKHAKI" "KEYWORD") *colors*) '(189 183 107)
      (gethash (intern "DARKMAGENTA" "KEYWORD") *colors*) '(139 0 139)
      (gethash (intern "DARKOLIVEGREEN" "KEYWORD") *colors*) '(85 107 47)
      (gethash (intern "DARKORANGE" "KEYWORD") *colors*) '(255 140 0)
      (gethash (intern "DARKORCHID" "KEYWORD") *colors*) '(153 50 204)
      (gethash (intern "DARKRED" "KEYWORD") *colors*) '(139 0 0)
      (gethash (intern "DARKSALMON" "KEYWORD") *colors*) '(233 150 122)
      (gethash (intern "DARKSEAGREEN" "KEYWORD") *colors*) '(143 188 143)
      (gethash (intern "DARKSLATEBLUE" "KEYWORD") *colors*) '(72 61 139)
      (gethash (intern "DARKSLATEGRAY" "KEYWORD") *colors*) '(47 79 79)
      (gethash (intern "DARKSLATEGREY" "KEYWORD") *colors*) '(47 79 79)
      (gethash (intern "DARKTURQUOISE" "KEYWORD") *colors*) '(0 206 209)
      (gethash (intern "DARKVIOLET" "KEYWORD") *colors*) '(148 0 211)
      (gethash (intern "DEEPPINK" "KEYWORD") *colors*) '(255 20 147)
      (gethash (intern "DEEPSKYBLUE" "KEYWORD") *colors*) '(0 191 255)
      (gethash (intern "DIMGRAY" "KEYWORD") *colors*) '(105 105 105)
      (gethash (intern "DIMGREY" "KEYWORD") *colors*) '(105 105 105)
      (gethash (intern "DODGERBLUE" "KEYWORD") *colors*) '(30 144 255)
      (gethash (intern "FIREBRICK" "KEYWORD") *colors*) '(178 34 34)
      (gethash (intern "FLORALWHITE" "KEYWORD") *colors*) '(255 250 240)
      (gethash (intern "FORESTGREEN" "KEYWORD") *colors*) '(34 139 34)
      (gethash (intern "FUCHSIA" "KEYWORD") *colors*) '(255 0 255)
      (gethash (intern "GAINSBORO" "KEYWORD") *colors*) '(220 220 220)
      (gethash (intern "GHOSTWHITE" "KEYWORD") *colors*) '(248 248 255)
      (gethash (intern "GOLD" "KEYWORD") *colors*) '(255 215 0)
      (gethash (intern "GOLDENROD" "KEYWORD") *colors*) '(218 165 32)
      (gethash (intern "GRAY" "KEYWORD") *colors*) '(128 128 128)
      (gethash (intern "GREEN" "KEYWORD") *colors*) '(0 128 0)
      (gethash (intern "GREENYELLOW" "KEYWORD") *colors*) '(173 255 47)
      (gethash (intern "GREY" "KEYWORD") *colors*) '(128 128 128)
      (gethash (intern "HONEYDEW" "KEYWORD") *colors*) '(240 255 240)
      (gethash (intern "HOTPINK" "KEYWORD") *colors*) '(255 105 180)
      (gethash (intern "INDIANRED" "KEYWORD") *colors*) '(205 92 92)
      (gethash (intern "INDIGO" "KEYWORD") *colors*) '(75 0 130)
      (gethash (intern "IVORY" "KEYWORD") *colors*) '(255 255 240)
      (gethash (intern "KHAKI" "KEYWORD") *colors*) '(240 230 140)
      (gethash (intern "LAVENDER" "KEYWORD") *colors*) '(230 230 250)
      (gethash (intern "LAVENDERBLUSH" "KEYWORD") *colors*) '(255 240 245)
      (gethash (intern "LAWNGREEN" "KEYWORD") *colors*) '(124 252 0)
      (gethash (intern "LEMONCHIFFON" "KEYWORD") *colors*) '(255 250 205)
      (gethash (intern "LIGHTBLUE" "KEYWORD") *colors*) '(173 216 230)
      (gethash (intern "LIGHTCORAL" "KEYWORD") *colors*) '(240 128 128)
      (gethash (intern "LIGHTCYAN" "KEYWORD") *colors*) '(224 255 255)
      (gethash (intern "LIGHTGOLDENRODYELLOW" "KEYWORD") *colors*) '(250 250 210)
      (gethash (intern "LIGHTGRAY" "KEYWORD") *colors*) '(211 211 211)
      (gethash (intern "LIGHTGREEN" "KEYWORD") *colors*) '(144 238 144)
      (gethash (intern "LIGHTGREY" "KEYWORD") *colors*) '(211 211 211)
      (gethash (intern "LIGHTPINK" "KEYWORD") *colors*) '(255 182 193)
      (gethash (intern "LIGHTSALMON" "KEYWORD") *colors*) '(255 160 122)
      (gethash (intern "LIGHTSEAGREEN" "KEYWORD") *colors*) '(32 178 170)
      (gethash (intern "LIGHTSKYBLUE" "KEYWORD") *colors*) '(135 206 250)
      (gethash (intern "LIGHTSLATEGRAY" "KEYWORD") *colors*) '(119 136 153)
      (gethash (intern "LIGHTSLATEGREY" "KEYWORD") *colors*) '(119 136 153)
      (gethash (intern "LIGHTSTEELBLUE" "KEYWORD") *colors*) '(176 196 222)
      (gethash (intern "LIGHTYELLOW" "KEYWORD") *colors*) '(255 255 224)
      (gethash (intern "LIME" "KEYWORD") *colors*) '(0 255 0)
      (gethash (intern "LIMEGREEN" "KEYWORD") *colors*) '(50 205 50)
      (gethash (intern "LINEN" "KEYWORD") *colors*) '(250 240 230)
      (gethash (intern "MAGENTA" "KEYWORD") *colors*) '(255 0 255)
      (gethash (intern "MAROON" "KEYWORD") *colors*) '(128 0 0)
      (gethash (intern "MEDIUMAQUAMARINE" "KEYWORD") *colors*) '(102 205 170)
      (gethash (intern "MEDIUMBLUE" "KEYWORD") *colors*) '(0 0 205)
      (gethash (intern "MEDIUMORCHID" "KEYWORD") *colors*) '(186 85 211)
      (gethash (intern "MEDIUMPURPLE" "KEYWORD") *colors*) '(147 112 219)
      (gethash (intern "MEDIUMSEAGREEN" "KEYWORD") *colors*) '(60 179 113)
      (gethash (intern "MEDIUMSLATEBLUE" "KEYWORD") *colors*) '(123 104 238)
      (gethash (intern "MEDIUMSPRINGGREEN" "KEYWORD") *colors*) '(0 250 154)
      (gethash (intern "MEDIUMTURQUOISE" "KEYWORD") *colors*) '(72 209 204)
      (gethash (intern "MEDIUMVIOLETRED" "KEYWORD") *colors*) '(199 21 133)
      (gethash (intern "MIDNIGHTBLUE" "KEYWORD") *colors*) '(25 25 112)
      (gethash (intern "MINTCREAM" "KEYWORD") *colors*) '(245 255 250)
      (gethash (intern "MISTYROSE" "KEYWORD") *colors*) '(255 228 225)
      (gethash (intern "MOCCASIN" "KEYWORD") *colors*) '(255 228 181)
      (gethash (intern "NAVAJOWHITE" "KEYWORD") *colors*) '(255 222 173)
      (gethash (intern "NAVY" "KEYWORD") *colors*) '(0 0 128)
      (gethash (intern "OLDLACE" "KEYWORD") *colors*) '(253 245 230)
      (gethash (intern "OLIVE" "KEYWORD") *colors*) '(128 128 0)
      (gethash (intern "OLIVEDRAB" "KEYWORD") *colors*) '(107 142 35)
      (gethash (intern "ORANGE" "KEYWORD") *colors*) '(255 165 0)
      (gethash (intern "ORANGERED" "KEYWORD") *colors*) '(255 69 0)
      (gethash (intern "ORCHID" "KEYWORD") *colors*) '(218 112 214)
      (gethash (intern "PALEGOLDENROD" "KEYWORD") *colors*) '(238 232 170)
      (gethash (intern "PALEGREEN" "KEYWORD") *colors*) '(152 251 152)
      (gethash (intern "PALETURQUOISE" "KEYWORD") *colors*) '(175 238 238)
      (gethash (intern "PALEVIOLETRED" "KEYWORD") *colors*) '(219 112 147)
      (gethash (intern "PAPAYAWHIP" "KEYWORD") *colors*) '(255 239 213)
      (gethash (intern "PEACHPUFF" "KEYWORD") *colors*) '(255 218 185)
      (gethash (intern "PERU" "KEYWORD") *colors*) '(205 133 63)
      (gethash (intern "PINK" "KEYWORD") *colors*) '(255 192 203)
      (gethash (intern "PLUM" "KEYWORD") *colors*) '(221 160 221)
      (gethash (intern "POWDERBLUE" "KEYWORD") *colors*) '(176 224 230)
      (gethash (intern "PURPLE" "KEYWORD") *colors*) '(128 0 128)
      (gethash (intern "REBECCAPURPLE" "KEYWORD") *colors*) '(102 51 153)
      (gethash (intern "RED" "KEYWORD") *colors*) '(255 0 0)
      (gethash (intern "ROSYBROWN" "KEYWORD") *colors*) '(188 143 143)
      (gethash (intern "ROYALBLUE" "KEYWORD") *colors*) '(65 105 225)
      (gethash (intern "SADDLEBROWN" "KEYWORD") *colors*) '(139 69 19)
      (gethash (intern "SALMON" "KEYWORD") *colors*) '(250 128 114)
      (gethash (intern "SANDYBROWN" "KEYWORD") *colors*) '(244 164 96)
      (gethash (intern "SEAGREEN" "KEYWORD") *colors*) '(46 139 87)
      (gethash (intern "SEASHELL" "KEYWORD") *colors*) '(255 245 238)
      (gethash (intern "SIENNA" "KEYWORD") *colors*) '(160 82 45)
      (gethash (intern "SILVER" "KEYWORD") *colors*) '(192 192 192)
      (gethash (intern "SKYBLUE" "KEYWORD") *colors*) '(135 206 235)
      (gethash (intern "SLATEBLUE" "KEYWORD") *colors*) '(106 90 205)
      (gethash (intern "SLATEGRAY" "KEYWORD") *colors*) '(112 128 144)
      (gethash (intern "SLATEGREY" "KEYWORD") *colors*) '(112 128 144)
      (gethash (intern "SNOW" "KEYWORD") *colors*) '(255 250 250)
      (gethash (intern "SPRINGGREEN" "KEYWORD") *colors*) '(0 255 127)
      (gethash (intern "STEELBLUE" "KEYWORD") *colors*) '(70 130 180)
      (gethash (intern "TAN" "KEYWORD") *colors*) '(210 180 140)
      (gethash (intern "TEAL" "KEYWORD") *colors*) '(0 128 128)
      (gethash (intern "THISTLE" "KEYWORD") *colors*) '(216 191 216)
      (gethash (intern "TOMATO" "KEYWORD") *colors*) '(255 99 71)
      (gethash (intern "TURQUOISE" "KEYWORD") *colors*) '(64 224 208)
      (gethash (intern "VIOLET" "KEYWORD") *colors*) '(238 130 238)
      (gethash (intern "WHEAT" "KEYWORD") *colors*) '(245 222 179)
      (gethash (intern "WHITE" "KEYWORD") *colors*) '(255 255 255)
      (gethash (intern "WHITESMOKE" "KEYWORD") *colors*) '(245 245 245)
      (gethash (intern "YELLOW" "KEYWORD") *colors*) '(255 255 0)
      (gethash (intern "YELLOWGREEN" "KEYWORD") *colors*) '(154 205 50))


(defun get-color (c)
  (gethash c *colors*))


(defun plotly-get-color (c)
  (let ((color (get-color c)))
    (format nil "rgb(~A, ~A, ~A)"
            (first color)
            (second color)
            (third color))))


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
     ,@(if (assoc :name data)
           (symbol-downcase
            (list :name (cdr (assoc :name data)))))
     :line ,(symbol-downcase
             `(:color ,(plotly-get-color (cdr (assoc :color data)))
               ,@(if (assoc :width data)
                     (symbol-downcase
                      (list :width (cdr (assoc :width data))))))))))


(defun plotly-marker (data)
  (symbol-downcase
   `(:x ,(cdr (assoc :x data))
     :y ,(cdr (assoc :y data))
     :type "scatter"
     :mode "markers"
     ,@(if (assoc :name data)
           (symbol-downcase
            (list :name (cdr (assoc :name data)))))
     :marker ,(symbol-downcase
               `(:color ,(plotly-get-color (cdr (assoc :color data)))
                 ,@(if (assoc :width data)
                     (symbol-downcase
                      (list :width (cdr (assoc :width data))))))))))


(defun plotly-fill (data)
  (list
   (symbol-downcase
    `(:x ,(cdr (assoc :x data))
      :y ,(cdr (assoc :y0 data))
      :type "scatter"
      :mode "lines"
      :line ,(symbol-downcase
              `(:color ,(plotly-get-color (cdr (assoc :color data)))))))
   (symbol-downcase
    `(:x ,(cdr (assoc :x data))
      :y ,(cdr (assoc :y1 data))
      :type "scatter"
      :mode "lines"
      :fill "tonexty"
      :fillcolor (plotly-get-color (cdr (assoc :color data)))
      :line ,(symbol-downcase
              `(:color ,(plotly-get-color (cdr (assoc :color data)))))))))


(defun plotly-errorbar (data)
  (symbol-downcase
   `(:x ,(cdr (assoc :x data))
     :y ,(cdr (assoc :y data))
     :type "scatter"
     :mode "marker"
     :fill "tonexty"
     :marker (:color ,(plotly-get-color (cdr (assoc :color data))))
     ,@(let ((errx (assoc :error-x data)))
         (if errx
             (symbol-downcase
              `(:error_x
                ,(symbol-downcase
                  `(:type "data"
                    :symmetric :false
                    :color ,(plotly-get-color (cdr (assoc :color data)))
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
                   :color ,(plotly-get-color (cdr (assoc :color data)))
                   ,@(if (consp (cadr erry))
                         (list :array (caddr erry)
                               :arrayminus (cadr erry))
                         (list :array (cdr erry))))))))))))


(defun plotly-bar (data)
  (symbol-downcase
   `(:x ,(cdr (assoc :x data))
     :y ,(cdr (assoc :y data))
     :type "bar"
     ,@(if (assoc :name data)
           (symbol-downcase
            (list :name (cdr (assoc :name data))))))))


(defun plotly-pie (data)
  (symbol-downcase
   `(:values ,(cdr (assoc :values data))
     :labels ,(cdr (assoc :labels data))
     :type "pie"
     ,@(if (assoc :name data)
           (symbol-downcase
            (list :name (cdr (assoc :name data))))))))


(defun plotly-box (data)
  (symbol-downcase
   `(:y ,(cdr (assoc :y data))
     :type "box"
     ,@(if (assoc :name data)
           (symbol-downcase
            (list :name (cdr (assoc :name data)))))
     :boxmean ,(cdr (assoc :boxmean data))
     :boxpoints ,(cdr (assoc :boxpoints data))
     :marker (:color ,(plotly-get-color (cdr (assoc :color data)))))))


(defun plotly-heatmap (data)
  (symbol-downcase
   `(:z ,(cdr (assoc :z data))
     :type "heatmap"
     :showscale ,(cdr (assoc :showscale data)))))


(defun plotly-contour (data)
  (symbol-downcase
   `(:z ,(cdr (assoc :z data))
     :type "contour"
     :showscale ,(cdr (assoc :showscale data))
     :autocontour ,(cdr (assoc :autocontour data)))))


(defun plotly-line3d (data)
  (symbol-downcase
   `(:x ,(cdr (assoc :x data))
     :y ,(cdr (assoc :y data))
     :z ,(cdr (assoc :z data))
     :type "scatter3d"
     :mode "lines"
     ,@(if (assoc :name data)
           (symbol-downcase
            (list :name (cdr (assoc :name data)))))
     :line ,(symbol-downcase
             `(:color ,(plotly-get-color (cdr (assoc :color data)))
               ,@(if (assoc :width data)
                     (symbol-downcase
                      (list :width (cdr (assoc :width data))))))))))


(defun plotly-marker3d (data)
  (symbol-downcase
   `(:x ,(cdr (assoc :x data))
     :y ,(cdr (assoc :y data))
     :z ,(cdr (assoc :z data))
     :type "scatter3d"
     :mode "markers"
     ,@(if (assoc :name data)
           (symbol-downcase
            (list :name (cdr (assoc :name data)))))
     :marker ,(symbol-downcase
               `(:color ,(plotly-get-color (cdr (assoc :color data)))
                 ,@(if (assoc :width data)
                     (symbol-downcase
                      (list :width (cdr (assoc :width data))))))))))


(defun plotly-surface (data)
  (symbol-downcase
   `(:z ,(cdr (assoc :z data))
     :type "surface"
     ,@(if (assoc :name data)
           (symbol-downcase
            (list :name (cdr (assoc :name data))))))))


(defun plotly-convert (data)
  (let ((chart-type (cdr (assoc :type data))))
    (cond 
      ((equal chart-type "line") (list (plotly-line data)))
      ((equal chart-type "marker") (list (plotly-marker data)))
      ((equal chart-type "fill") (plotly-fill data))
      ((equal chart-type "errorbar") (list (plotly-errorbar data)))
      ((equal chart-type "bar") (list (plotly-bar data)))
      ((equal chart-type "pie") (list (plotly-pie data)))
      ((equal chart-type "box") (list (plotly-box data)))
      ((equal chart-type "heatmap") (list (plotly-heatmap data)))
      ((equal chart-type "contour") (list (plotly-contour data)))
      ((equal chart-type "line3d") (list (plotly-line3d data)))
      ((equal chart-type "marker3d") (list (plotly-marker3d data)))
      ((equal chart-type "surface") (list (plotly-surface data))))))


(defun to-json (param)
  (let ((jonathan:*false-value* :false)
        (jonathan:*null-value* :null)
        (jonathan:*empty-array-value* :empty-array)
        (jonathan:*empty-object-value* :empty-object))
    (jonathan:to-json param)))


(defun plotly-code (states)
  (mapcar #'to-json
          (apply #'append
                 (mapcar #'plotly-convert states))))



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
