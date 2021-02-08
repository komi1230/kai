;;;; launch.lisp --- File opener
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file handles opening js files with system commands.

(in-package :cl-user)

(defpackage :kai.plotly.launch
  (:use :cl)
  (:import-from :kai.util
                :check-file-exist)
  (:export :open-browser))
(in-package :kai.plotly.launch)


;;; Open browser
(defun open-browser ()
  (plot-from-file "plotly/kai.html"))

;; TODO: remove package prefixes
(defun plot-from-file (filename &key (browser :default) (browser-options cl-user::*default-browser-options*))
  "Open plot specification FILENAME located in the KAI:CACHE directory."
  (let ((plot-file (namestring (merge-pathnames filename (translate-logical-pathname "KAI:CACHE;")))))
    #+windows (setf plot-file (concatenate 'string "file:///" plot-file))
    (uiop:launch-program `(,(alexandria:assoc-value cl-user::*browser-commands* browser)
			   ,@(case browser
			     (:chrome (if (assoc "app" browser-options :test 'string=)
					  (setf (cdr (assoc "app" browser-options :test 'string=)) plot-file))
			      (cl-user::encode-chrome-options browser-options))
			     (:default plot-file)))
			 :ignore-error-status t)))

