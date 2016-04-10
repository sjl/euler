(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(:define-constant
               :switch
               :while
               :ensure-boolean
               :with-gensyms
               :n-grams
               )
  :package "EULER.QUICKUTILS")
