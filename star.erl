-module(star).
-export([star/1]).

star(0) -> io:format("");
star(M) when M>0 -> print(M), star(M-1).

print(0) -> io:format("~n");
print(N) when N>0 ->io:format("*"),print(N-1).
