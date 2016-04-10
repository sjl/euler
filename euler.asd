(asdf:defsystem #:euler
  :name "euler"
  :description "Project Euler solutions."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (#:defstar
               #:fiveam
               #:optima
               #:trivial-types
               #:cl-arrows
               #:fare-quasiquote-optima
               #:fare-quasiquote-readtable)

  :serial t
  :components ((:file "quickutils") ; quickutils package ordering crap
               (:file "package")
               (:file "utils")
               (:file "primes")
               (:file "euler")))

