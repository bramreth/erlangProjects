-module(assessment1).
-export([fib/1, fib2/1, perimeter/1, overlap/2, occurs/2, odd/1, parity/1, game_score/1]).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

%fib(4).
%fib(4-1) + fib(4-2)
%fib(3) + fib(2)
%fib(3-1) + fib(3-2) + fib(2-1) + fib(2-2)
%fib(2) + fib(1) + fib(1) + fib(0)
%fib(2-1) + fib(2-2) + fib(1) + fib(1) + fib(0)
%fib(1) + fib(0) + fib(1) + fib(1) + fib(0)
%1 + 0 + 1 + 1 + 0
%3

fib2(0) -> {0, 1};
fib2(1) -> {1, 1};
fib2(N) -> X = fib(N-1), 
			Y = fib(N-2),
				{X+Y, X+X+Y}.

perimeter({circle, {_,_}, R})
  -> 2*math:pi()*R;
perimeter({rectangle, {_,_}, H, W}) 
  -> 2*(H+W).

overlap({circle, {Xa,Ya}, Ra},{circle, {Xb,Yb}, Rb}) -> checkOverlap(math:sqrt(math:pow(Xa-Xb,2)+math:pow(Ya-Yb,2)),math:sqrt(math:pow(Ra-Rb,2)), math:sqrt(math:pow(Ra+Rb,2))).
checkOverlap(L,Ra,Rb) when Ra < L, L < Rb -> true;
checkOverlap(_,_, _) -> false.

occurs([],_) -> 0;
occurs([X|Xs],X) -> 1 + occurs(Xs,X);
occurs([_|Xs],N) -> occurs(Xs,N).

odd([]) -> [];
odd([X|Xs]) when X rem 2 == 0 -> odd(Xs);
odd([X|Xs]) -> [X|odd(Xs)].

parity([]) -> 0;
parity([X|Xs]) when X rem 2 == 0 -> parity(Xs);
parity([_|Xs]) -> 1 + parity(Xs).

game_score([]) -> 0;
game_score([{Xa,Xb}|Xs]) -> result(Xa,Xb) + game_score(Xs).

result(A,A) -> 0;
result(A,B) -> equal(beat(A),B).

equal(A,A) -> -1;
equal(_,_) -> 1.

beat(rock) -> paper; 
beat(paper) -> scissors; 
beat(scissors) -> rock.