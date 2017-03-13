-module(two_2).
-export([head/1, tail/1, second/1]).

head([X|_Xs]) ->
	X.

tail([_X|Xs]) ->
	Xs.

% this works
%second(Xs) ->
%	head(tail(Xs)).

% this works
%second([Xs]) ->
%	head(tail(Xs)).

% this works
second([_X, Y | _Zs]) ->
	Y.

