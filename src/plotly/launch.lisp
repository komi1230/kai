;;;; launch.lisp --- File opener
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file handles opening js files with system commands.

(in-package :cl-user)

(defpackage :kai.plotly.launch
  (:use :cl)
  (:import-from :kai.plotly.generate
                :check-file-exist))
(in-package :kai.plotly.launch)



;;;; Check OS
;;;
;;; When launching js file in the browser, we use system command
;;; to open browser. Here we check user's OS.
;;; And use system command.
(defun system (cmd-str)
  (trivial-shell:shell-command cmd-str))

(defun open-browser ()
  (let ((path-to-html (check-file-exist "index.html")))
    #+darwin
    (system (format nil "open ~A" path-to-html))
    #+linux
    (system (format nil "xdg-open ~A" path-to-html))
    #+win32
    (system (format nil "start ~A" path-to-html))))
