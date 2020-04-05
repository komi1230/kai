;;;; build.lisp --- Donwloader of binaries of GR
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file is composed of download functions, which is branched to 
;;; OS types.
;;;
;;; see: https://github.com/jheinen/GR.jl/blob/master/deps/build.jl

(in-package :cl-user)
(defpackage :kai.gr.build
  (:use :cl)
  (:import-from :kai.util
                :download-file
                :get-os)
  (:import-from :kai.converter
                :make-kai-cache))
(in-package :kai.gr.build)


;;;; URL branches
;;;
;;; We can get GR binaries via network, but the URLs is
;;; branched depending on OS.
;;; Here we implement a function to provide a proper URL.

(defun get-url ()
  (let* ((base "https://gr-framework.com.org/downloads/gr-latest-")
         (os (get-os))
         (os-url (case os
                   ("windows" "Windows")
                   ("darwin" "Darwin")
                   ("ubuntu" "Ubuntu"))))


;;;; Donwload client
;;;
;;; When using GR, some sources is needed.
;;; Here is a set of file donwload client and file checker.

(defun download-gr ()
  (download-file (merge-pathnames "GR.tar.gz"
                                  (make-kai-cache "GR"))
                 (get-url)))

