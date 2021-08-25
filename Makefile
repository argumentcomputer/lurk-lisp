.PHONY : always

image : bin/lurk.image

bin/lurk.image : $(shell find . -type f -name '*.lisp') $(shell find . -type f -name '*.asd')
	bin/cl -Q -sp lurk --dump bin/lurk.image

test : always
	bin/cl -Q -sp lurk -x "(asdf:test-system \"lurk\")"

repl :
	bin/cl -Q -sp lurk -p lurk.api.tooling -E run-repl
