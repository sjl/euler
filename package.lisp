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
