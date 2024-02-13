.PHONY : always

image : bin/lurk.image

bin/lurk.image : $(shell find . -type f -name '*.lisp') $(shell find . -type f -name '*.asd')
	bin/cl -Q -sp lurk --dump bin/lurk.image

test : always
	bin/cl -Q -sp lurk -x "(asdf:test-system \"lurk\")"

# Run repl after dumping image if needed.
# Faster startup than replx, except when dumping a new image.
repl : image
	@bin/lurk

# Run repl after loading Lurk.
# Slower startup than replx after image is dumped, but faster for one-off runs.
replx :
	@bin/lurkx
