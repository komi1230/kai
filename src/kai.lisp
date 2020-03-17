(in-package :cl-user)

(defpackage :kai
  (:use :cl)
  (:import-from :kai.interface
                :scatter
                :style
                :show
                :reset!
                :*state*
                :*style*)
  (:export :scatter
           :style
           :show
           :reset!
           :*state*
           :*style*))
(in-package :kai)

