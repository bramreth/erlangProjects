-module(howManyEqual).
-export([howManyEqual/3]).

howManyEqual(M,N,O) -> equal(M,N) + equal(N,O) + equal(M,O).

equal(M,M) -> 1;
equal(M,N) -> 0.