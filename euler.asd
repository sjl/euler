(asdf:defsystem :euler
  :name "euler"
  :description "Project Euler solutions."

  :author "Steve Losh <steve@stevelosh.com>"
  :maintainer "Steve Losh <steve@stevelosh.com>"

  :license "MIT"
  :version "0.0.1"

  :depends-on (

               :anaphora
               :cl-strings
               :fiveam
               :iterate
               :local-time
               :losh

               )

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "primes")
                             (:file "utils")
                             (:file "hungarian")
                             (:file "problems")
                             (:file "poker")))))

