-module(two_11).
-export([take/2, take2/2, take3/2, take4/2]).

% Define a function take that takes the first N elements from a list.
% Here are some examples of take in action:
%
% take(0,"hello") = []
%
% take(4,"hello") = "hell"
%
% take(5,"hello") = "hello"
%
% take(9,"hello") = "hello"

% I see that this spec doesn't allow for empty list...
%-spec take(integer(), [T]) -> [T].

% ...unless we have this clause
	% which is a specific, shortcutting interface
take(_N, []) ->
	[];

% another specific, shortcutting interface
take(0, [_X|_Xs]) ->
	[];

% general interface
take(N, [X|Xs]) ->
	take(N, [X|Xs], 0, []).

% for when taken length == N
take(_N, [_X|_Xs], _Length, AccList) when _Length == _N ->
	AccList;
% for when we run out of input list
take(_N, [], _Length, AccList) ->
	AccList;
take(N, [X|Xs], Length, AccList) ->
	take(N, Xs, Length + 1, AccList ++ [X]).


% UPDATED VERSION without the dreaded '++' operator

% a specific, shortcutting interface
take2(_N, []) ->
	[];

% another specific, shortcutting interface
take2(0, [_X|_Xs]) ->
	[];

% general interface
take2(N, [X|Xs]) ->
	take2(N, [X|Xs], 0, []).	% no good as line 61 won't accept an empty list

% for when taken length == N
take2(_N, [_X|_Xs], _Length, [L|Ls]) when _Length == _N ->
	[L|Ls];
% for when we run out of input list
take2(_N, [], _Length, [L|Ls]) ->
	[L|Ls];
take2(N, [X|Xs], Length, [L|Ls]) ->
	take2(N, Xs, Length + 1, [X|[L|Ls]]).


% another bash


% a specific, shortcutting interface
take3(_N, []) ->
	[];

% another specific, shortcutting interface
take3(0, [_X|_Xs]) ->
	[];

% general interface
take3(N, [X|Xs]) ->
	take3(N, [X|Xs], 0, []).	% 

% for when taken length == N
take3(_N, [_X|_Xs], _Length, L) when _Length == _N and is_list(L) ->
	L;
% for when we run out of input list
take3(_N, [], _Length, L) when is_list(L) ->
	L;
take3(N, [X|Xs], Length, L) when is_list(L) ->
	take3(N, Xs, Length + 1, [X|L]).	% nope, doesn't work


% last bash THAT ACTUALLY WORKS!

% a specific, shortcutting interface
take4(_N, []) ->
	[];

% another specific, shortcutting interface
take4(0, [_X|_Xs]) ->
	[];

% general interface
take4(N, [X|Xs]) ->
	take4(N, [X|Xs], 0, []).	% 

% for when taken length == N
%take4(_N, [_X|_Xs], _Length, L) when _Length == _N and is_list(L) -> L;
take4(0, [_X|_Xs], _Length, L) when is_list(L) ->
	lists:reverse(L);
% for when we run out of input list
take4(_N, [], _Length, L) when is_list(L) ->
	lists:reverse(L);
take4(N, [X|Xs], Length, L) when is_list(L) ->
	take4(N - 1, Xs, Length + 1, [X|L]).	% builds the result list but in reverse