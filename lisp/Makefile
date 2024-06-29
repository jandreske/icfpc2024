sources := $(shell echo src/*.lisp)
test_sources := $(shell echo tests/*.lisp)

icfpc: $(sources) main.lisp compile.lisp
	sbcl --control-stack-size 3000 --dynamic-space-size 9000 --load compile.lisp

test:
	sbcl --non-interactive --load run-tests.lisp

clean:
	$(RM) icfpc
