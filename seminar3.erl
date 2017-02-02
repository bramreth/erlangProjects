-module(seminar3).
-export([doubleAll/1, evens/1, append/2, index/2, merge/2, mergeSort/1]).

doubleAll([]) -> []; 
doubleAll([X|Xs]) -> [ 2*X | doubleAll(Xs) ]. 

evens([]) -> []; 
evens([X|Xs]) when X rem 2 == 0 -> [X | evens(Xs) ]; 
evens([_|Xs]) -> evens(Xs).

append([], Ys) -> Ys;
append([X|Xs],Ys) -> [X|append(Xs,Ys)].

index(0,[X|Xs]) -> X; 
index(M,[X|Xs]) -> index(M-1, Xs).

merge([],Ys) -> Ys;
merge(Xs,[]) -> Xs;
merge([X|Xs],[Y|Ys]) when X<Y-> [X|merge(Xs,[Y|Ys])];
merge([X|Xs],[Y|Ys])-> [Y|merge([X|Xs],Ys)].

%mergeSort([]) -> [];
%mergeSort([X]) -> [X];
%mergeSort(Xs) -> {Ys, Zs} = lists:split(length(Xs) div 2,Xs),
%sortedYs = mergeSort(Ys),
%sortedZs =mergeSort(Zs),
%merge(sortedYs,sortedZs).