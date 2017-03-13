% we can start with a comment
-module(first).
-export([double / 1, mult / 2]).
%-export([mult / 2]).
-export([area / 3, square / 1, treble / 1]).

mult(X, Y) ->
	X * Y.

double(X) ->
	mult(X, 2).

area(A, B, C) ->
	S = (A + B + C) / 2,
	math:sqrt(S * (S - A) * (S - B) * (S - C)).

% recycle 'mult'
square(A) ->
	mult(A, A).

% recycle 'mult'
treble(A) ->
	mult(A, 3).

