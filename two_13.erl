-module(two_13).
-export([nub/1, nubVariant/1, isXInList/2]).

% Define a function nub to remove all the duplicate elements from a list.
% This could remove all repeated elements except the first instance,
% or all repeated elements except the final instance.
%
% nub([2,4,1,3,3,1]) = [2,4,1,3]
%
% nub([2,4,1,3,3,1]) = [2,4,3,1]

% this is for nub([2,4,1,3,3,1]) = [2,4,1,3]
nub([]) -> [];
nub([X|Xs]) ->
	nub([X|Xs], []).

nub([], AccList) ->
	lists:reverse(AccList);
nub([X|Xs], AccList) ->
	case isXInList(X, AccList) of
		true ->
			nub(Xs, AccList);
		false ->
			nub(Xs, [X|AccList])
	end.
% a direct recursion version doesn't spring to mind. hmmm

% this is for nub([2,4,1,3,3,1]) = [2,4,1,3]
nubVariant(X) -> ok.
	% ??

% ==== UTILITY ================

% from lesson 2.9
% how many times does the given value appear in a list?
	% direct recursion, seems to work!
howManyTimesIsXInList(_X, []) -> 0;
howManyTimesIsXInList(X, [Y|Ys]) ->
	case X of
		Y ->
			1 + howManyTimesIsXInList(X, Ys);
		_ ->
			howManyTimesIsXInList(X, Ys)
	end.

% based on the above
isXInList(_X, []) -> false;
isXInList(X, [Y|Ys]) ->
	case X of
		Y ->
			true;
		_ ->
			isXInList(X, Ys)
	end.