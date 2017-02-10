(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :define-constant
               :ensure-boolean
               :n-grams
               :range
               :switch
               :with-gensyms

               )
  :package "EULER.QUICKUTILS")
