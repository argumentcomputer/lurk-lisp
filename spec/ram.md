# Lurk Language Specification: `:ram` subset

The `:ram` subset is activated as follows:
```./bin/lurk --subset ram```

The `:ram` subset provides a global memory (RAM), global definitions (`define`) and global macros (`defmacro`). It also supports quasiquotes as a library.

## RAM

The RAM is threaded through the program.
The threading is shallow: most non-tail recursive calls such as evaluation of arguments do not thread the RAM.
A notable exception: the `begin` form does thread the RAM through the evaluation of its sub-expressions.

This threading enables top-level definitions as well as macros generating definitions and macros.

The RAM is passed in to closures during evaluation, so closures have access to definitions created after them.

### `current-ram`

The expression `(current-ram)` evaluates to the current RAM represented as an association list.

## `define`

The syntax for `define` is only `(define <name> <expr>)`. It does not include sugar.

A `define` form extends the RAM with a binding for `name` and the evaluation of the expression `expr`.

## `defmacro`
The syntax for `defmacro` follows Common Lisp. It is `(defmacro <name> <formals> <body>)`.

Macroexpansion is a separate phase, performed before evaluation.
Being suitable for compilation, macros are erased during evaluation, except for the reflective special form `macroexpand`.

Macroexpansion traverses the structure of an expression.
When a head of an application matches the name of a macro, the macro is expanded to the evaluation of its body in the environment extending the formals with the (unevaluated) arguments of the macro application, and the result is expanded again.

Macros support expansion of _other_ macros within its body, because the body is expanded during traversal of a `defmacro`.
Recursive macros are supported indirectly by constructing a list whose head is a macro name, because the result of an expansion is also expanded.

## `macroexpand` special form

The form `(macroexpand <expr>)` evaluates to the expansion of of the evaluation of `<expr>`.

## Quasiquotes

The reader converts backquote to `quasi` and comma to `uq` (for unquote) and `,@` to `uqs` (for unquote-splicing).
Then, `quasi` can be implemented as a Lurk-level recursive macro.
