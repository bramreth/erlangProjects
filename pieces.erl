-module(pieces).
-export([pieces/1]).

pieces(1) -> 2;
pieces(N) when N>0 -> N +pieces(N-1).