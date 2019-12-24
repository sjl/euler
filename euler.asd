(defclass auto-module (module) ())

(defmethod component-children ((self auto-module))
  (mapcar (lambda (p) (make-instance 'cl-source-file :type "lisp"
                        :pathname p
                        :name (pathname-name p)
                        :parent (component-parent self)))
          (directory-files (component-pathname self)
                           (make-pathname :directory nil :name *wild* :type "lisp"))))


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
               :iterate
               :local-time
               :losh
               :lparallel
               :pileup
               :str

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
                             (:file "poker")
                             (:auto-module "problems")))))

