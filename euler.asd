(asdf:defsystem :euler
  :name "euler"
  :description "Project Euler solutions."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT"
  :version "0.0.1"

  :depends-on (

               :1am
               :anaphora
               :cl-pcg
               :cl-strings
               :iterate
               :local-time
               :losh
               :pileup

               )

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "primes")
                             (:file "math")
                             (:file "utils")
                             (:file "hungarian")
                             (:file "problems")
                             (:file "poker")))))

