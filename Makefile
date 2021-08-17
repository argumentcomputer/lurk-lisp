.PHONY : always

image : bin/rhododendron.image

bin/rhododendron.image : $(shell find . -type f -name '*.lisp') $(shell find . -type f -name '*.asd')
	bin/cl -Q -sp rhododendron --dump bin/rhododendron.image

test : always
	bin/cl -Q -sp rhododendron -x "(asdf:test-system \"rhododendron\")"

repl :
	bin/cl -Q -sp rhododendron -p rhododendron.api.tooling -E run-repl
