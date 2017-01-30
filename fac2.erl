-module(fac2).
-export([fac2/2]).

fac2(N) -> fac2(N,1).

fac2(0,P) -> P
fac2(N,P) when N>0 -> fac2(N-1,P*N).

