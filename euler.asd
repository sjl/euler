(asdf:defsystem #:euler
  :name "euler"
  :description "Project Euler solutions."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (

               :fiveam
               :iterate
               :local-time
               :losh
               :cl-strings
               :anaphora

               )

  :serial t
  :components (
               (:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "primes")
                             (:file "euler")
                             (:file "poker")))))

