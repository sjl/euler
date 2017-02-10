(asdf:defsystem #:euler
  :name "euler"
  :description "Project Euler solutions."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (

               :fare-quasiquote-optima
               :fare-quasiquote-readtable
               :fiveam
               :iterate
               :losh
               :optima
               :trivial-types

               )

  :serial t
  :components (
               (:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "primes")
                             (:file "euler")))))

