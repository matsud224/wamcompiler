# README #

This is a prolog compiler written in Common Lisp which compiles prolog code to bytecode of Warren's abstract machine (WAM).

I recommend using Steel Bank Common Lisp (SBCL).
To start REPL, eval the following expression: 
```lisp
(load "wamcompiler.lisp")
(repl)
```

Input ';' to show the next solution, 'y' to stop finding solutions, or 'a' to show all solutions.

To show the compiled bytecode, eval the following expression:
```lisp
(show-wamcode "predicate-name" predicate-arity)
```

An article of this program (in Japanese):
「すごいPrologつくって学ぼう?!」 pp.23-37
http://www.kitcc.org/share/lime/lime56.pdf

# References #
* AN ABSTRACT PROLOG INSTRUCTION SET 日本語訳
http://www.takeoka.org/~take/ailabo/prolog/wam/wam.html

* Warren's Abstract Machine: A Tutorial Reconstruction
http://wambook.sourceforge.net/
