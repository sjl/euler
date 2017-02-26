(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :define-constant
               :ensure-boolean
               :map-permutations
               :maxf
               :minf
               :n-grams
               :range
               :rcurry
               :read-file-into-string
               :switch
               :with-gensyms

               )
  :package "EULER.QUICKUTILS")
