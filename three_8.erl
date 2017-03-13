-module(three_8).
-export([beat/1, lose/1, result/2, tournament/2]).

% ==== ROCK-PAPER-SCISSORS ================

% (from a previous lesson)
% zip/2 (the interface)
% enforce that we need two non-empty lists
zip([], []) -> [];
zip([X|Xs], []) -> [];
zip([], [X|Xs]) -> [];

zip([X|Xs], [Y|Ys]) ->
	zip([X|Xs], [Y|Ys], []).

% zip/3 (the implementation)
zip([X|Xs], [], Result) ->	% ie. we've run out of second list
	lists:reverse(Result);
zip([], [Y|Ys], Result) ->	% ie. we've run out of first list
	lists:reverse(Result);
% the meat and bones of it now
zip([X|Xs], [Y|Ys], Result) ->
	zip(Xs, Ys, [{X, Y} | Result]).


% (from a previous lesson)
% interface
% enforce that we need two non-empty lists
zip_with(Fun, [], []) -> [];
zip_with(Fun, [X|Xs], []) -> [];
zip_with(Fun, [], [X|Xs]) -> [];
zip_with(Fun, [X|Xs], [Y|Ys]) ->
	zip_with(Fun, [X|Xs], [Y|Ys], []).

% implementation
zip_with(Fun, [], [Y|Ys], Result) ->	% ie. we've run out of first list
	lists:reverse(Result);
zip_with(Fun, [X|Xs], [], Result) ->	% ie. we've run out of second list
	lists:reverse(Result);
zip_with(Fun, [], [], Result) ->	% both lists run out at same time!
	lists:reverse(Result);
% main event
zip_with(Fun, [X|Xs], [Y|Ys], Result) ->
	zip_with(Fun, Xs, Ys, [Fun(X, Y) | Result]).


% which move beats the specified move?
beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock.


% which move loses against the specified move?
lose(rock) -> scissors;
lose(paper) -> rock;
lose(scissors) -> paper.


% result of a round (from player 1's perspective)
% my approach is to use -1, 0 and 1 for lose, draw and win respectively
% which will be useful in totalling multi-round games up.
% another approach is boolean but that won't give us 'draw'.
% we could also use win, lose and draw as atoms but then multi-round
% totalling might be tricky
result(Move, Move) -> 0;	% same move so draw
result(Move1, Move2) ->
	case lose(Move1) of
		Move2 ->
			1;	% win
		_ ->
			-1	% lose
	end.


% result of a tournament (which is a series of rounds)
tournament(LeftPlays, RightPlays) when length(LeftPlays) =/= length(RightPlays) ->
	argumentsnotgood;
tournament(Left, Right) ->
	%lists:map(fun ({X, Y}) -> result(X, Y) end, zip([X|Xs], [Y|Ys])).	% nope
	Scorecard = zip_with(fun (X, Y) -> result(X, Y) end, Left, Right),
	lists:sum(Scorecard).