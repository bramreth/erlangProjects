-module(maxThree).
-export([maxThree/3]).

maxThree(M,N,O) -> maxTwo(M,maxTwo(N,O)).

maxTwo(M,N) when M>N -> M;
maxTwo(M,N) when N>M -> N;
maxTwo(M,M) -> M.