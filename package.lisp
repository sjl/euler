(defpackage :euler
  (:use
    :cl
    :iterate
    :losh
    :euler.quickutils)
  (:import-from :1am :is)
  (:shadowing-import-from :1am :test))

(defpackage :euler.poker
  (:use
    :cl
    :iterate
    :losh
    :euler
    :anaphora-basic
    :euler.quickutils))

(defpackage :euler.hungarian
  (:use
    :cl
    :iterate
    :losh
    :euler
    :euler.quickutils)
  (:export
    :find-minimal-assignment))
