# Replic

Automatically build a REPL with completion of commands and of their
arguments from the exported symbols of a given package.

Define your functions and your completions in `~/.replic.lisp`.

Examples to come (basic ones in source).

## Installation

Not in Quicklisp yet.

## Usage

Build with `make build`, run

    ./replic

try `hello <name>` (completion for `hello`) and `goodbye <name>`,
where <name> can be completed from what was given to `hello`.


## Resources


* https://github.com/mrkkrp/cl-readline
* https://github.com/vindarel/cl-readline-example

Learning:

* https://github.com/LispCookbook/cl-cookbook
* https://github.com/CodyReichert/awesome-cl#learning-and-tutorials

Getting started:

* [editors (Emacs, Portacle, Vim, Lem, Atom, Sublime), notebooks, REPLs](https://lispcookbook.github.io/cl-cookbook/editor-support.html
* https://lispcookbook.github.io/cl-cookbook/getting-started.html
