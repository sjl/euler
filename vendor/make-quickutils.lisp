(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :define-constant
               :ensure-boolean
               :n-grams
               :range
               :rcurry
               :switch
               :with-gensyms

               )
  :package "EULER.QUICKUTILS")
