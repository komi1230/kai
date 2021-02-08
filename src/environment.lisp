;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-

;;; OS Environment for Kai

(setf (logical-pathname-translations "kai")
      `(("CACHE;**;*.*.*" #+windows ,(merge-pathnames "kai/**/*.*" (uiop:getenv-absolute-directory "TEMP"))
		          #-windows ,(merge-pathnames "kai/**/*.*" (uiop:getenv-absolute-directory "XDG_CACHE_HOME")))
	("DATA;**;*.*.*" #+windows ,(merge-pathnames "kai/**/*.*" (uiop:getenv-absolute-directory "LOCALAPPDATA"))
		         #-windows ,(merge-pathnames "kai/**/*.*" (uiop:getenv-absolute-directory "XDG_DATA_HOME")))
	("CONFIG;**;*.*.*" #+windows ,(merge-pathnames "kai/**/*.*" (uiop:getenv-absolute-directory "APPDATA"))
		           #-windows ,(merge-pathnames "kai/**/*.*" (uiop:getenv-absolute-directory "XDG_CONFIG_HOME")))))


