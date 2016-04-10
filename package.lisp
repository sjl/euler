(defpackage #:euler.utils
  (:use #:cl #:euler.quickutils)
  (:export
    #:random-exclusive
    #:dividesp))

(defpackage #:euler
  (:use #:cl #:5am
        #:euler.quickutils #:euler.utils))
