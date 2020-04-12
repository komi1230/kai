;;;; Setup
;;;
;;; We have to setup and confirm that installed binaries and
;;; shared files work fine.


(cffi:load-foreign-library
   ;;(merge-pathnames "gr/lib/libGR.so"
   ;;                 (make-kai-cache "gr"))
 "/Users/komi/.cache/kai/gr/gr/lib/libGR.so")

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

(cffi:defcfun ("gr_initgr" initgr) :void)

(cffi:defcfun ("gr_opengks" opengks) :void)

(cffi:defcfun ("gr_closegks" closegks) :void)
