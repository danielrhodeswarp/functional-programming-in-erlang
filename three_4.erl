-module(three_4).
-export([specificCircles/1, genericCircles/1]).
-export([specificSum/1, genericSum/1, lambdaSum/1]).

% ==== from specific to generic ================
% ==============================================

% here is a specific *filter* function (to keep only the 'circle' tuples):
%                    ========

% circles([]) -> [];
% circles([{circle, {X,Y}, R} | Xs]) ->
%	[{circle, {X,Y}, R} | circles(Xs)];
% circles([{rectangle, {_, _}, _, _} | Xs]) ->
%	circles(Xs).

% the only specific thing here is the logical test
% of "is it a circle or not" (which we achieve with pattern matching)
% everything else is boiler plate.

% how to generify? We have a decider function returning true or false
% (in our case true if it is a circle)

% filter(P, []) -> [];

% filter(P, [X|Xs]) ->
%	case P(X) of
%		true ->
%			[X | filter(P, Xs)];
%		false ->
%			filter(P, Xs)
%	end.


% ==== A SAMPLE TO MESS ABOUT WITH ================

% generic filter pattern
filter(_P, []) -> [];
filter(P, [X|Xs]) ->
	case P(X) of
		true ->
			[X | filter(P, Xs)];
		false ->
			filter(P, Xs)
	end.

% decider function
is_circle( {circle, {_, _}, _} ) ->
	true;
is_circle( {rectangle, {_, _}, _, _} ) ->
	false.

% get only the circles in a specific way
specificCircles([]) -> [];
specificCircles([{circle, {X,Y}, R} | Xs]) ->
	[{circle, {X,Y}, R} | specificCircles(Xs)];
specificCircles([{rectangle, {_, _}, _, _} | Xs]) ->
	specificCircles(Xs).

% get only the circles by using a filtering pattern
genericCircles(Shapes) -> filter(fun is_circle/1, Shapes).


% here is a specific *reduce* function (to sum the elements of a list):
%                    =======

% sum([]) -> 0;
% sum([X|Xs]) -> X + sum(Xs).

% the only specific things here are the STARTING VALUE (0)
% and the combining function (+)
% here is a generic pattern

% Combine is a function
% reduce(Combine, Start, []) ->
% 	Start;
% reduce(Combine, Start, [X|Xs]) ->
%	Combine(X, reduce(Combine, Start, Xs)).


% ==== A SAMPLE TO MESS ABOUT WITH ================

% specific sum
specificSum([]) -> 0;
specificSum([X|Xs]) -> X + specificSum(Xs).

% generic reduce pattern
% Combine is a function
reduce(_Combine, Start, []) ->
	Start;
reduce(Combine, Start, [X|Xs]) ->
	Combine(X, reduce(Combine, Start, Xs)).

% helper for generic summer
plus(X, Y) -> X + Y.

% generic summer using the reducer
genericSum(Xs) -> reduce(fun plus/2, 0, Xs).

% lambda stylee
lambdaSum(Xs) -> reduce(fun (X, Y) -> X + Y end, 0, Xs).
