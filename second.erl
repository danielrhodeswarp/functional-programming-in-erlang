-module(second).
-export([summat / 1]).
-export([hypotenuse / 2, perimeter / 2, area / 2]).

% it seems like, in this module, we can call functions in the 'first'
% module without needing to do anything special. I wonder if that is only
% because both modules are in the same folder?

% http://erlang.org/doc/man/math.html is a useful reference here!

% a test
summat(A) ->
	first:treble(A).

% get size of hypotenuse (of a right triangle) from sizes of other two sides
% recycle first:square() and use built-in math:sqrt()
hypotenuse(A, B) ->
	math:sqrt(first:square(A) + first:square(B)).

% get perimeter of a right triangle from sizes of the two short sides
% recycle hypotenuse()
perimeter(A, B) ->
	A + B + hypotenuse(A, B).

% get area of a right triangle from sizes of the two short sides
area(A, B) ->
	first:mult(A, B) / 2.