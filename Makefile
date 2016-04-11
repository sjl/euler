.PHONY: test

quickutils.lisp: make-utilities.lisp
	sbcl-rlwrap --noinform --load make-utilities.lisp  --eval '(quit)'

test:
	sbcl-rlwrap --noinform --load run-tests.lisp  --eval '(quit)'
