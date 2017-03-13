-module(three_3).
-export([specificDoubler/1, genericDoubler/1]).

% with this:

% all_areas([]) -> [];
% all_areas([X|Xs]) ->
% 	[area(X) | all_areas(Xs)].

% the pattern is generic and the only specific thing is the
% call to area/1
%
% So, we can actually pass the function into the generic pattern as an argument:

% map(F, []) -> [];
% map(F, [X|Xs]) ->
% 	[F(X) | map(F, Xs)].

% so here's the final definition:
%
% all_areas(Shapes) -> map(fun area/1, Shapes).



% ==== A SAMPLE TO MESS ABOUT WITH ================

% generic mapper
map(_F, []) -> [];
map(F, [X|Xs]) ->
	[F(X) | map(F, Xs)].

% double anything
double(X) -> X * 2.

% double all elements of a list (*without* using a mapping pattern)
specificDoubler([]) -> [];
specificDoubler([X|Xs]) ->
	[double(X) | specificDoubler(Xs)].

% double all elements of a list using a mapping pattern
genericDoubler(Things) -> map(fun double/1, Things).
