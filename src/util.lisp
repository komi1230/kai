;;;; util.lisp --- A collection of utility functions 
;;;
;;; This code has been placed in the Public Domain.  All warranties
;;; are disclaimed.
;;;
;;; This file is composed of a collection of utility functions for
;;; plotting. This checks type and shape of input data.


(in-package :cl-user)
(defpackage :kai.util
  (:use :cl)
  (:export :convert-data
           :check-shape-type
           :find-min-max
           :to-array
           :sort-input
           :flatten
           :data-alloc
           :free
           :string-alloc
           :string-free
           :arr-aref
           :make-kai-cache
           :check-file-exist))
(in-package :kai.util)

;; rationalise how implementations refer to MS Windows in *features*
#+(and (not windows) (or win32 mswindows))(pushnew :windows *features*)

;;;; Input style converter
;;;
;;; When getting input data, we accept variable length args.
;;; We cannot realize to accept one or two args with some options by
;;; standard style, so we papare such a function to convert args.

(defun convert-data (&rest data)
  (let ((x (car data))
        (y (cadr data)))
    (if (or (consp x)        ; check first data
            (vectorp x))
        (if (or (consp y)    ; check second data
                (vectorp y))
            data
            `(,(loop for i below (length x) collect i)
              ,x
              ,@(cdr data)))
        (error "Invalid input"))))


;;;; Type checker
;;;
;;; Here we will accept input data whose type is integer or float.
;;; Input data has to be one-dimensional array or list.

(defun type-check (data)
  (every #'numberp data))


;;;; Flatten
;;;
;;; To adjust dimensions of input data

(defun flatten (lst)
  (if (null lst)
      nil
      (if (atom lst)
          (list lst)
          (append (flatten (car lst))
                  (flatten (cdr lst))))))


;;;; Symbol Converter
;;;
;;; We will get input data as list.
;;; When using plotly as backend, we have to convert list data to JSON.
;;; In converting list data to JSON, symbols will be converted as capital texts.
;;; Here we provide a function to prevent this.

(defun make-keyword (name)
  (values (intern name "KEYWORD")))

(defun symbol-downcase (data)
  (mapcar #'(lambda (x)
              (if (keywordp x)
                  (make-keyword (string-downcase (symbol-name x)))
                  x))
          data))



;;;; Ensure directories and files
;;;
;;; When plotting, Kai depends on some files.
;;; Here we check file path.
;;; Check if .cache file exists in the home directory on UNIX-like
;;; system or local application cache/data directory on MS Windows.
;;; And create cache directory for Kai.

(defun make-kai-cache (dir-name)
  (ensure-directories-exist
   #+windows
   (merge-pathnames (pathname (format nil "kai/~A/" dir-name))
		    (uiop:getenv-absolute-directory "TEMP")) ; If plots are truly cache data

   ;; if plots are considered data and might be saved for later, this
   ;; directory is more appropiate
   #+nil
   (merge-pathnames (pathname (format nil "kai/plots/~A/" dir-name))
		    (uiop:getenv-absolute-directory "LOCALAPPDATA"))

   ;; UNIX should probably use something similar to the above, e.g.:
   ;; (uiop:getenv "XDG_CACHE_HOME") for cache and (uiop:getenv
   ;; "XDG_DATA_HOME") for plots that are output data.  I would make
   ;; the change but have no way to test it.
   #-windows
   (merge-pathnames (format nil ".cache/kai/~A/" dir-name)
                    (user-homedir-pathname))))

(defun check-file-exist (dir filename)
  (probe-file (merge-pathnames filename
                               (make-kai-cache dir))))



;;;; Download
;;;
;;; When setting up, we have to get some files via networks.
;;; Multiple backends need to download resources, so we implement
;;; a download client here.

(defun download-file (filename uri)
  (dex:fetch uri filename
             :if-exists :supersede))


;;;; OS Distribution
;;;
;;; SBCL can get an informatioin about OS, but it is very abstract.
;;; Because we can find whether the OS is Linux, but we cannot find
;;; which distribution the OS is.

(defun redhat-version ()
  (let* ((content (uiop:read-file-lines "/etc/redhat-release"))
         (trim-before (subseq content
                              (+ 8 (search "release" content))))
         (trim-after (subseq trim-before
                             0
                             (search " " trim-before))))
    trim-after))


(defun get-dist (key)
  (if (probe-file #P"/etc/redhat-release")
      "redhat"
      (loop :for file :in (uiop:directory-files "/etc/" "*-release")
            :do (loop :for line :in (uiop:read-file-lines file)
                      :if (uiop:string-prefix-p (format nil "~A=" key) line)
                      :do (return-from get-dist (subseq line (1+ (length key))))))))




;;;;    Memory allocation and free for Array
;;;
;;; When drawing graph, we have to provide data as array pointer
;;; to drawing function.

(defun data-alloc (lst type)
  (cffi:foreign-alloc type
                      :initial-contents
                      (mapcar #'(lambda (x)
                                  (case type
                                    (:int (coerce x 'integer))
                                    (:float (coerce x 'single-float))
                                    (:double (coerce x 'double-float))))
                              lst)))

(defun free (&rest vars)
  (loop for var in vars
        do (cffi:foreign-free var)))

(defun string-alloc (str)
  (cffi:foreign-string-alloc str))

(defun string-free (&rest strs)
  (loop for str in strs
        do (cffi:foreign-string-free str)))

(defun arr-aref (arr type index)
  (cffi:mem-aref arr type index))
