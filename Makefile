.PHONY: 

utilities.lisp: make-utilities.lisp
	sbcl --noinform --load make-utilities.lisp  --eval '(quit)'
