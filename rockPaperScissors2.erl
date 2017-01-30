-module(rockPaperScissors2).
-export([result/2]).

result(A,A) -> draw;
result(A,B) -> equal(beat(A),B).

equal(A,A) -> lose;
equal(A,B) -> win.

beat(rock) -> paper; 
beat(paper) -> scissors; 
beat(scissors) -> rock.