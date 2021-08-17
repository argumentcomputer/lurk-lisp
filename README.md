# rhododendron
Lisp counterpart to [dendron](https://github.com/porcuquine/dendron) -- providing a language (name TBD) for Turing-complete recursive SNARKs.

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
> ln -s ~/<installdir>/rhododendron/rhododendron.asd rhododendron.asd
```
# Test

Running tests should show output similar to the following.
```bash
(base) ➜  rhododendron git:(master) ✗ make test
bin/cl -Q -sp rhododendron -x "(asdf:test-system \"rhododendron\")"

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
(base) ➜  rhododendron git:(master) ✗
```

# REPL

Run the REPL with `make repl`.

The example below shows loading [a library](example/lib.rh), running [code using it](example/test.rh), then clearing so the library is unavailable.

```bash
(base) ➜  rhododendron git:(master) ✗ make repl
bin/cl -Q -sp rhododendron -p rhododendron.api.tooling -E run-repl
PACKAGE => #<PACKAGE "RHODODENDRON.USER">

Rhododendron REPL.
:help for help.

> :help

:HELP => Print this text.
:QUIT => :Quit REPL.
:ECHO <FORM> =>  Read one form and echo it.
:LOAD <PATH> => Load a library.
:CLEAR => Clear loaded libraries.
:RUN <PATH> => Evaluate expression from file.

> :load "example/lib.rh"
Reading from example/lib.rh.

> :run "example/test.rh"
Reading from example/test.rh.
Run: (SQUARE 8)

64
> :clear

> :run "example/test.rh"
Reading from example/test.rh.
Run: (SQUARE 8)
ERROR: Unbound var: RHODODENDRON.USER::SQUARE
> (+ 1 (* 8 8))

65
> :quit
(base) ➜  rhododendron git:(master) ✗ 
```

# Documentation

TODO
