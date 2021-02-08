;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
(in-package :cl-user)			; Should really be :kai

;;; Helper functions to launch browers

;;; Note: This experiment has shown that there is too much variability
;;; in browser behaviour to use the command line switches to control
;;; behaviour. The best solution is to control as much of the browser
;;; behaviour in JavaScript, where the behaviour should be the same
;;; across platforms.

;;; Some things, like launching in app mode, must be done at the
;;; command line and for that you can use what's in this file. It is
;;; for the above reasons that there is an argument encoding function
;;; specific to each browser that is selected in the arguments to
;;; uiop:launch-program.

;;;
;;; Functions and data for all browsers
;;;
(defparameter *browser-commands*
  (list (cons :chrome #+windows "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe"
		      #+macos "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
	(cons :firefox #+windows "C:\\Program Files (x86)\\Mozilla Firefox\\firefox.exe"
		       #+macos "/Applications/Firefox.app/Contents/MacOS/firefox")
	(cons :default #+windows "explorer"
		       #+(or macos darwin) "open"
		       #-(or macos darwin windows) "xdg-open")
  "Maps browser names to system paths"))

(defparameter *default-browser-options* nil)

(defun %print-alist (stream data &rest args)
  (declare (ignore args)
	   (special arg-format))
  (destructuring-bind (head . tail) data
    (format stream arg-format head (if (consp tail) (first tail) tail))))

(defun encode-application-options (options arg-format)
  "Turns OPTIONS, an alist, into a command line argument list according to ARG-FORMAT"
  ;; Chrome arg-format: "--~A=~A"
  ;; Firefox: "-~A ~A"
  (declare (special arg-format))
  (format nil "~{~/%print-alist/~^ ~}" options))


;;;
;;; Chrome
;;;

(defun encode-chrome-options (options)
  "Encode options for Chrome"
  ;; We want to add a --user-data-directory so that --windows-size is honoured
  ;; --app, if present, will have been set by the caller
  (let ((chrome-options (push (cons "user-data-dir"
				      (merge-pathnames (format nil "chrome-data-~A" (princ-to-string (gensym)))
						       (translate-logical-pathname #P"KAI:CACHE;")))
			     options)))
    (encode-application-options chrome-options "--~A=~A")))

(defun set-chrome-size (size)
  "Set the windows size in *default-browser-options*"
  (setf (cdr (assoc "window-size" *default-browser-options* :test 'string=)) size))

(defparameter *default-chrome-options*
  (list (cons "window-size" "800,600")	; This should probably be set in the plot JavaScript
	(cons "app" "foo")))            ; Run without tabs, menus, etc. "foo" ignored

