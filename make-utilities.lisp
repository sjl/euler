(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "utilities.lisp"
  :utilities '(:define-constant
               :switch
               :ensure-boolean
               :with-gensyms
               )
  :package "EULER.UTILITIES")
