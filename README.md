# lurk
Lisp counterpart to [lurk-rs](https://github.com/porcuquine/lurk-rs) -- providing Lurk language for Turing-complete recursive SNARKs.

# Installation

## sbcl

For simplicity, install sbcl with package manager, e.g.

```bash
> brew install sbcl
```

or
```bash
> apt-get install sbcl
```

or if daring, [from source](https://sourceforge.net/p/sbcl/sbcl/ci/master/tree/).

### QuickLisp & ASDF

Install [QuickLisp](https://www.quicklisp.org):

- Download the file for installation. (https://beta.quicklisp.org/quicklisp.lisp)
- Then run sbcl with that file loaded by this command.

```sh
sbcl --load path/of/quicklisp.lisp
```

After sbcl has launched, type in the command below.

```lisp
(quicklisp-quickstart:install)
```

Now Quicklisp has been installed. To ensure Quicklisp is loaded every time you start Lisp, type in the command below.

```lisp
(ql:add-to-init-file)
```

### Integrate the project with quicklisp

QuickLisp needs to find the project, so add a symlink:

```bash
> cd ~/quicklisp/local-projects
> ln -s ~/<installdir>/lurk/lurk.asd lurk.asd
```
# Test

Running tests should show output similar to the following.
```bash
(base) ➜  lurk git:(master) ✗ make test
bin/cl -Q -sp lurk -x "(asdf:test-system \"lurk\")"

Running test suite MASTER-SUITE
 Running test suite API-IMPL-SUITE
  Running test ATOM ..
  Running test EVAL-EXPR-FOR-P .....................................................
  Running test INVERSE ............................................................................................................................................................................................
 Running test suite API-TOOLING-SUITE
 Did 243 checks.
    Pass: 243 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
(base) ➜  lurk git:(master) ✗
```

# REPL

Run the REPL with `make repl`.

The example below shows loading [a library](example/lib.lurk), running [code using it](example/test.lurk), then clearing so the library is unavailable.

```bash
(base) ➜  lurk git:(master) ✗ make repl
bin/cl -Q -sp lurk -p lurk.api.tooling -E run-repl
PACKAGE => #<PACKAGE "LURK.USER">

Lurk REPL.
:help for help.

> :help

:HELP => Print this text.
:QUIT => :Quit REPL.
:ECHO <FORM> =>  Read one form and echo it.
:LOAD <PATH> => Load a library.
:CLEAR => Clear loaded libraries.
:RUN <PATH> => Evaluate expression from file.

> :load "example/lib.lurk"
Reading from example/lib.lurk.

> :run "example/test.lurk"
Reading from example/test.lurk.
Run: (SQUARE 8)

64
> :clear

> :run "example/test.lurk"
Reading from example/test.lurk.
Run: (SQUARE 8)
ERROR: Unbound var: LURK.USER::SQUARE
> (+ 1 (* 8 8))

65
> :quit
(base) ➜  lurk git:(master) ✗ 
```

# Documentation

TODO
