current_op(1200,fx,?-).
current_op(1200,fx,:-).
current_op(1200,xfx,:-).
current_op(1200,xfx,-->).
current_op(1150,fx,public).
current_op(1150,fx,mode).
current_op(1150,fx,index).
current_op(1150,fx,extern).
current_op(1150,fx,dynamic).
current_op(1150,fx,bltin).
current_op(1150,fx,###).
current_op(1150,fy,module).
current_op(1150,fy,help).
current_op(1100,xfy,;).
current_op(1050,xfy,->).
current_op(900,fy,spy).
current_op(900,fy,nospy).
current_op(900,fy,\+).
current_op(700,xfx,is).
current_op(700,xfx,\==).
current_op(700,xfx,\=).
current_op(700,xfx,@>=).
current_op(700,xfx,@>).
current_op(700,xfx,@=<).
current_op(700,xfx,@<).
current_op(700,xfx,>=).
current_op(700,xfx,>).
current_op(700,xfx,=\=).
current_op(700,xfx,==).
current_op(700,xfx,=<).
current_op(700,xfx,=:=).
current_op(700,xfx,=/=).
current_op(700,xfx,=..).
current_op(700,xfx,=).
current_op(700,xfx,<).
current_op(700,xfx,:=).
current_op(700,xfx,/==).
current_op(700,xfx,#\=).
current_op(700,xfx,#>=).
current_op(700,xfx,#>).
current_op(700,xfx,#=<).
current_op(700,xfx,#=).
current_op(700,xfx,#<).
current_op(580,xfx,notin).
current_op(580,xfx,in).
current_op(580,xfy,::).
current_op(560,yfx,..).
current_op(550,xfy,:).
current_op(500,yfx,or).
current_op(500,yfx,and).
current_op(500,yfx,\/).
current_op(500,yfx,/\).
current_op(500,yfx,-).
current_op(500,yfx,+).
current_op(400,yfx,rem).
current_op(400,yfx,mod).
current_op(400,yfx,>>).
current_op(400,yfx,<<).
current_op(400,yfx,//).
current_op(400,yfx,/).
current_op(400,yfx,*).
current_op(200,fy,\).
current_op(200,fy,-).
current_op(200,fy,+).
current_op(200,xfx,**).
current_op(200,xfy,^).

:-builtin_lock(current_op,3).


repeat.
repeat:-repeat.
:-builtin_lock(repeat,0).

\+(P):-call(P),!,fail.
\+(_).
:-builtin_lock(\+,1).

=(X,X).
:-builtin_lock(=,2).

append([], Xs, Xs).
append([X | Ls], Ys, [X | Zs]) :- append(Ls, Ys, Zs).
:-builtin_lock(append,3).

member(X, [X|_]).
member(X, [_|Y]) :- member(X, Y).
:-builtin_lock(member,2).

subset([],_).
subset([A|R],B):-member(A,B),subset(R,B).
:-builtin_lock(subset,2).
