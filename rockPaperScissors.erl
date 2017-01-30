-module(rockPaperScissors).
-export([result/2]).

result(rock,paper) -> 'lose';
result(rock,scissors) -> 'win';
result(paper,scissors) -> 'lose';
result(paper,rock) -> 'win';
result(scissors,rock) -> 'lose';
result(scissors,paper) -> 'win';
result(A,A) -> 'draw'.