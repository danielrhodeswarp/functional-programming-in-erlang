-module(two_4).
-export([sumlist/1, sumlistDirect/1, patternMatchingFib/1, reverso/1, twoFirst/1]).

% ==== tail recursion (not seem work) ================
% interface for sumlist()
sumlist([]) ->
	0;
sumlist([X|Xs]) ->
	sumlistInternal([X|Xs], 0).

% sum of empty list is zero
sumlistInternal([], SumSoFar) ->
	SumSoFar;

% tail recursion for non empty lists
sumlistInternal([X|Xs], SumSoFar) ->
	
	sumlistInternal(Xs, SumSoFar + X).	% note [Xs] not seem work


% ==== direct recursion ================

sumlistDirect([]) ->
	0;
sumlistDirect([X|Xs]) ->
	
	%X + sumlistDirect([Xs]).	% nope, doesn't work
	X + sumlistDirect(Xs).	% yep, works!

% ==== pattern matching version of my fibonacci function
% ==== from lesson 1.19

% the original
%fib(1) ->
%	[0];
%fib(2) ->
%	[0,1];
%fib(X) ->
%	L = fib(X - 1),
%	L ++ [lists:nth(length(L) - 1, L) + lists:nth(length(L), L)].

% simplified via pattern matching
% not work as no opposite of [X|Xs]
patternMatchingFib(1) ->
	[0];
patternMatchingFib(2) ->
	[0,1];
patternMatchingFib(X) ->
	[FirstStuff,Penultimate,Last] = patternMatchingFib(X - 1),
	[FirstStuff,Penultimate,Last] ++ [Penultimate + Last].

% doesn't work
reverso([Xs|X]) ->
	io:format("Xs is ~p; X is ~p~n", [Xs, X]).

% works as expected
twoFirst([X,Y|Ys]) ->
	io:format("~p ~p ~p~n", [X, Y, Ys]).