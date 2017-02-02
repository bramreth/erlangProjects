-module(memberList).
-export([member/2]).

member(X,[]) -> false;
member(X, [Y|Ys]) when X==Y-> true;
member(X, [Y|Ys]) -> member(X,Ys).

