-module(listBuild).
-export([build/2]).

build(N,N) -> [N];
build(N,M) when N<M -> [N|build(N+1,M)].

