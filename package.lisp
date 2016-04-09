(defpackage #:euler.utils
  (:use #:cl #:euler.quickutils)
  (:export
    #:dividesp))

(defpackage #:euler
  (:use #:cl #:euler.quickutils #:euler.utils))
