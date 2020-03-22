(in-package :cl-user)

(defpackage :kai
  (:use :cl)
  (:import-from :kai.interface
                :scatter
                :bar
                :pie
                :style
                :show
                :reset!
                :*state*
                :*style*)
  (:export :scatter
           :bar
           :pie
           :style
           :show
           :reset!
           :*state*
           :*style*))
(in-package :kai)

