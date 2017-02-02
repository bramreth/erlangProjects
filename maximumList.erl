-module(maximumList).
-export([maximum/1]).

maximum([]) -> 0;
maximum([X|Xs]) -> maxTwo(X,maximum(Xs)).


maxTwo(M,N) when M>N -> M;
maxTwo(_,N) -> N;
maxTwo(M,[]) -> M.

