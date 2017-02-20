(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :define-constant
               :ensure-boolean
               :read-file-into-string
               :n-grams
               :range
               :rcurry
               :switch
               :with-gensyms

               )
  :package "EULER.QUICKUTILS")
