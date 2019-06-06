#!/bin/sh

rlwrap sbcl --eval '(progn (load "wamcompiler.lisp") (repl))'
