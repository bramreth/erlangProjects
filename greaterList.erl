-module(greaterList).
-export([greater/2]).

greater(X,[]) -> [];
greater(X, [Y|Ys]) when Y>X-> [Y|greater(X,Ys)];
greater(X, [Y|Ys]) -> greater(X,Ys).