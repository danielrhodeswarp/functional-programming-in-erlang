-module(better_sort).
-export([recursiveSort/1, order/1]).

recursiveSort([]) ->
	[];
recursiveSort([X]) ->
	[X];
recursiveSort([X|Xs]) ->
	[min(X, listMin(Xs))|recursiveSort(Xs)].	% nope

% by Daniel Aguirre
order([]) -> [];
order([X|Xs]) ->
	CurrentMin = listMin([X|Xs]),
	CurrentList = [X|Xs],
	[CurrentMin|order(CurrentList -- [CurrentMin])].



listMin([X]) ->
	X;
listMin([X|Xs]) ->
	min(X, listMin(Xs)).