(defpackage :euler
  (:use
    :cl
    :iterate
    :losh
    :5am
    :euler.quickutils)
  (:shadowing-import-from :5am :test))

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
