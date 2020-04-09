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


;;;; URL branches and install
;;;
;;; We can get GR binaries via network, but the URLs is
;;; branched depending on OS.
;;; Here we implement a function to provide a proper URL.

;; macOS
(defun install-gr-mac ()
  (let* ((url "https://gr-framework.org/downloads/gr-latest-Darwin-x86_64.tar.gz")
         (register-cmd "/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -f")
         (tarball-path (merge-pathnames "gr.tar.gz"
                                        (make-kai-cache "gr")))
         (gksterm-path (merge-pathnames "Applications/GKSTerm.app"
                                        (make-kai-cache "gr"))))
    (download-file tarball-path url)
    (uiop:run-program (format nil "tar xvf ~A" tarball-path)
                      :output nil)
    (uiop:run-program (format nil "~A ~A" register-cmd gksterm-path)
                      :output nil)))




(defun get-url ()
  (let* ((base "https://gr-framework.com.org/downloads/gr-latest-")
         (os (get-os))
         (os-url (case os
                   ("windows" "Windows")
                   ("darwin" "Darwin")
                   ("ubuntu" "Ubuntu"))))))

