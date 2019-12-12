article(c, [the|S], S).
article(c, [a|S], S).

noun(c, [cat|S], S).
noun(c, [mouse|S], S).

np(S, S0) :- article(G, S, S1), noun(G, S1, S0).

tverb([caught|S], S).
iverb([fled|S], S).

vp(S, S0) :- tverb(S, S1), np(S1, S0).
vp(S, S0) :- iverb(S, S0).

phrase(S, S0) :- np(S, S1), vp(S1, S0).

