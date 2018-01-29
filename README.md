# Replic

`replic` is first a ready-to-use executable that reads your code from `~/.replic.lisp`.

* a function becomes a command in the command line interface. You get
    completion for functions.
* a variable becomes a variable, which is `set`able in the repl
    (`set` then comes with completion of variables).
* you can define how to complete a function's arguments. Examples below.

`replic` is also a library with which you can automatically build a
REPL from the (exported) functions and variables of a given package,
with the process described above.

More examples to come (basic ones follow).

This is an attempt at factoring out what I did several times with `cl-readline`.

`! very experimental !`


## Installation

Not in Quicklisp yet.

## Usage

    replic -h

### Use the executable and write your commands

Build with `make build`, run

    ./replic

try `hello <name>` (completion for `hello`) and `goodbye <name>`,
where <name> can be completed from what was given to `hello`.

Your `.replic.lisp` looks like this:

~~~lisp
(in-package :replic.user)

(defun duckduckgo (&rest words)
  (format t "searching for ~a...~&" words))

(export '(duckduckgo))
~~~


### Set variables

    set \*variable\*

(we kept earmuffs).

We get completion on the variables list.


### Define custom completion for arguments

Write a function or a variable, `export` it.

Associate a function name with a list of completion candidates or a
function (todo), add it to the `*args-completions*` alist:

~~~lisp
(push '("goodbye" . *names*) *args-completions*)
(push '("hello" . #'complete-hello) *args-completions*)
~~~


## How it works

The mecanism is an abstraction on `cl-readline`'s completion function.


## Resources


* https://github.com/mrkkrp/cl-readline
* https://github.com/vindarel/cl-readline-example

Learning:

* https://github.com/LispCookbook/cl-cookbook
* https://github.com/CodyReichert/awesome-cl#learning-and-tutorials

Getting started:

* [editors (Emacs, Portacle, Vim, Lem, Atom, Sublime), notebooks, REPLs](https://lispcookbook.github.io/cl-cookbook/editor-support.html
* https://lispcookbook.github.io/cl-cookbook/getting-started.html
