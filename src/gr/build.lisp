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


;;;; GR version
;;;
;;; There are many GR versions, so we decide a version to be installed.

(defparameter *gr-version* "0.48.0")


;;;; URL branches and install
;;;
;;; We can get GR binaries via network, but the URLs is
;;; branched depending on OS.
;;; Here we implement a function to provide a proper URL.

;; macOS
(defun install-gr-mac ()
  (let* ((url "https://github.com/sciapp/gr/releases/download/v0.48.0/gr-0.48.0-Darwin-x86_64.tar.gz")
         (register-cmd "/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -f")
         (kai-cache-dir (make-kai-cache "gr"))
         (tarball-path (merge-pathnames "gr.tar.gz"
                                        kai-cache-dir))
         (gksterm-path (merge-pathnames "Applications/GKSTerm.app"
                                        kai-cache-dir)))
    (download-file tarball-path url)
    (uiop:run-program (format nil "tar xvf ~A -C ~A"
                              tarball-path kai-cache-dir)
                      :output nil)
    (uiop:run-program (format nil "mv ~A/gr/* ~A/ && rm -fr ~A/gr"
                              kai-cache-dir kai-cache-dir kai-cache-dir))
    (uiop:run-program (format nil "~A ~A" register-cmd gksterm-path)
                      :output nil)))


;; Linux
(defun install-gr-linux ()
  (let* ((base-url "https://github.com/sciapp/gr/releases/download")
         (id (string-downcase (get-dist "ID")))
         (id-like (string-downcase (get-dist "ID_LIKE")))
         (os ((cond
                ((equal id "redhat")
                 (if (> (digit-char-p
                         (aref (redhat-version)
                               0))
                        7)
                     "Redhat"
                     (error "You should upgrade OS version")))
                ((or (equal id "ubuntu")
                     (equal id-like "ubuntu"))
                 "Ubuntu")
                ((or (equal id "debian")
                     (equal id-like "debian")
                     (equal_id "raspbian"))
                 "Debian")
                ((or (equal id "arch")
                     (equal id-like "arch"))
                 "ArchLinux")
                ((equal id "opensuse-tumbleweed")
                 "CentOS"))))
         (arch (cond
                 ((equal (machine-type) "X86-64")
                  "x86_64")
                 ((or (equal (machine-type) "ARM")
                      (equal (machine-type) "ARM64"))
                  "armhf")))
         (tarball-path (merge-pathnames "gr.tar.gz"
                                        kai-cache-dir))
         (ka-cache-dir (make-kai-cache "gr")))
    (donwload-file tarball-path
                   (format nil "~A/v~A/gr-~A-~A-~A.tar.gz"
                           base-url *gr-version*
                           *gr-version* os arch))
    (uiop:run-program (format nil "tar xvf ~A -C ~A"
                              tarball-path kai-cache-dir))
    (uiop:run-program (format nil "mv ~A/gr/* ~A/ && rm -fr ~A/gr"
                              kai-cache-dir kai-cache-dir kai-cache-dir))))


;; Windows
(defun install-gr-windows ()
  (error "Sorry, I am not familiar with Windows environment. I'm waiting for your PR."))



(defun install-gr ()
  #+(or win32 mswindows windows) ; Windows
  (install-gr-windows)
  #+(or macos darwin) ; macOS
  (install-gr-mac)
  #-(or win32 mswindows macos darwin windows) ;Linux
  (install-gr-linux))

