# lurk
This repo is the Lisp counterpart to [lurk-rs](https://github.com/lurk-lang/lurk-rs). Lurk in Lisp allows for documentation and implementation of the API without the proof-related mechanisms of Lisp in Rust. The Lisp version of Lurk provides a spec that defines the semantics of Lurk and allows you to explore its capabilities.

# Installation

## sbcl

For simplicity, install sbcl with package manager, e.g.

```bash
> brew install sbcl
```

Or if daring, [from source](https://sourceforge.net/p/sbcl/sbcl/ci/master/tree/).

On Ubuntu it is necessary to install more recent version of sbcl, since `apt` seems to use too old version. Follow next instructions:

- download from github:
```bash
git clone https://github.com/sbcl/sbcl
``` 

- change directory:
```bash
cd sbcl
```

- compile (will take some time):
```bash
sh make.sh
```

- install (you may need use `sudo`):
```bash
sh install.sh
```

If you prefer a different configuration you can take a look on `README` and `INSTALL` files for more detailed information. 

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
# Submodules

Lurk source files used in tests are in the [lurk-lib](https://github.com/lurk-lang/lurk-lib) submodule. You must
initialize and update submodules before test will pass.

```bash
> git submodule init
Submodule 'lurk-lib' (git@github.com:lurk-lang/lurk-lib.git) registered for path 'lurk-lib'
> git submodule update
Cloning into '<installation-path>/lurk-lang/lurk/lurk-lib'...
Submodule path 'lurk-lib': checked out '<lurk-lib-head-commit>'
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

The example below shows loading [a library](lurk-lib/example/lib.lurk), running [code using it](lurk-lib/example/test.lurk), then clearing so the library is unavailable.

```bash
➜  lurk git:(master) ✗ make repl          
Lurk REPL [API].
:help for help.

API> :help

:HELP => Print this text.
:QUIT => :Quit REPL.
:ECHO <FORM> =>  Read one form and echo it.
:LOAD <PATH> => Load a library.
:CLEAR => Clear loaded libraries.
:RUN <PATH> => Evaluate expressions from file.

API> :load "lurk-lib/example/lib.lurk"
Read from lurk-lib/example/lib.lurk: (LETREC*
                                           ((SQUARE (LAMBDA (X) (* X X))))
                                           (CURRENT-ENV))

API> (square 8)
64

API> :clear

API> :run "lurk-lib/example/test.lurk"
Read from lurk-lib/example/test.lurk: (LURK.TOOLING.REPL::!
                                            (:LOAD "lib.lurk"))
Read from /Users/clwk/fil/lurk/lurk-lib/example/lib.lurk: (LETREC*
                                                           ((SQUARE
                                                             (LAMBDA (X)
                                                              (* X X))))
                                                           (CURRENT-ENV))
Read from lurk-lib/example/test.lurk: (LURK.TOOLING.REPL::!
                                       (:ASSERT-EQ (SQUARE 8) 64))
Read from lurk-lib/example/test.lurk: (LURK.TOOLING.REPL::!
                                       (:ASSERT-EQ (SQUARE 9) 81))
Read from lurk-lib/example/test.lurk: (LURK.TOOLING.REPL::!
                                       (:ASSERT (EQ (SQUARE 10) 100)))

API> :quit

➜  lurk git:(master) ✗ 
```
# Spec
[High-level Lurk Language Specification](spec/v0-1.md)

# Documentation

TODO

## License

MIT or Apache 2.0
