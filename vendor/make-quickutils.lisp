(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :copy-array
               :curry
               :define-constant
               :emptyp
               :ensure-boolean
               :ensure-function
               :ensure-gethash
               :equivalence-classes
               :map-combinations
               :map-permutations
               :maxf
               :minf
               :n-grams
               :range
               :rcurry
               :read-file-into-string
               :removef
               :switch
               :with-gensyms

               )
  :package "EULER.QUICKUTILS")
