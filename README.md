# README #

Prologコンパイラ。CommonLispで書きました。Warren's abstract machine(WAM)のバイトコードへコンパイルします。


```lisp
(load "wamcompiler.lisp")
(repl)
```
 でREPLを起動できます。
;で次の解表示、yで打ち切り、aで全解表示です。
バイトコードは(show-wamcode "f" 3)で見れます。（述語f/3の場合）

atom/1,var/1,is/2,findall/3,=../2,call/1,op/3等の組み込み述語を使えます。

SBCLでの動作を確認しました。

# 参考文献 #
AN ABSTRACT PROLOG INSTRUCTION SET 日本語訳
http://www.takeoka.org/~take/ailabo/prolog/wam/wam.html

Warren's Abstract Machine: A Tutorial Reconstruction
http://wambook.sourceforge.net/
