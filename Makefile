.PHONY: test

quickutils.lisp: make-utilities.lisp
	sbcl --noinform --load make-utilities.lisp  --eval '(quit)'

test:
	sbcl --noinform --load run-tests.lisp  --eval '(quit)'
