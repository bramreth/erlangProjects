-module(maxTwo).
-export([maxTwo/2]).

maxTwo(M,N) when M>N -> M;
maxTwo(M,N) -> N.


