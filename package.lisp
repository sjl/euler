(defpackage :euler
  (:use :cl :iterate :losh :euler.quickutils)
  (:export
    :*use*
    :run-tests

    :define-problem

    :irange
    :nth-prime
    :palindromep
    :prime-factorization
    :pythagorean-triplets-of-perimeter

    ))

(defpackage :euler.poker
  (:use
    :cl
    :iterate
    :losh
    :euler
    :anaphora-basic
    :euler.quickutils))

(defpackage :euler.hungarian
  (:use :cl :euler :iterate :losh :euler.quickutils)
  (:export
    :find-minimal-assignment))

(defparameter euler:*use* '(:use
                             :cl
                             :euler
                             :iterate
                             :losh
                             :euler.quickutils))
