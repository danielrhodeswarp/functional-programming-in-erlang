-module(one_24).
-export([perimeter / 1, area / 1, enclose / 1]).	% shapes
-export([bits / 1]).	% summing the bits
-export([testAll / 0, testArea / 0, testPerimeter / 0, testEnclose / 0, testBits / 0]).	% tests for shapes AND bits

% ==== MASTER TESTER ================

% run all automated tests (and return "has everything passed?" boolean)
testAll() ->
	io:format("Running all tests~n"),
	true == (
		testArea()
		and testPerimeter()
		and testEnclose()
		% and testBits()
	).

% ==== SHAPES ================

% shape tuples used:
% {circle, {X, Y}, Radius} where {X, Y} is centre of circle
% {rectangle, {X, Y}, Height, Width} where {X, Y} is centre of rectangle
% {triangle, {X, Y}, SideLength1, SideLength2, SideLength3}	where {X, Y} is centre of triangle
%	(for sure, not so realistic! but to keep a bit of consistency with circle and rectangle)
%
% NOTE that I don't really pay attention to the {X, Y} coords of shapes!

% get perimeter of a shape
perimeter({circle, {_X, _Y}, Radius}) ->
	Diameter = 2 * Radius,
	math:pi() * Diameter;
perimeter({rectangle, {_X, _Y}, Height, Width}) ->
	(2 * Height) + (2 * Width);
perimeter({triangle, {_X, _Y}, SideLength1, SideLength2, SideLength3}) ->
	SideLength1 + SideLength2 + SideLength3.

% get area of a shape
area({circle, {_X, _Y}, Radius}) ->
	math:pi() * Radius * Radius;
area({rectangle, {_X, _Y}, Height, Width}) ->
	Height * Width;
area({triangle, {X, Y}, SideLength1, SideLength2, SideLength3}) ->
	% we use Heron's formula which is based on side lengths and the semiperimeter
	Semiperimeter = perimeter({triangle, {X, Y}, SideLength1, SideLength2, SideLength3}) / 2,
	math:sqrt(Semiperimeter * (Semiperimeter - SideLength1) * (Semiperimeter - SideLength2) * (Semiperimeter - SideLength3)).

% get smallest bounding box of a shape
% (returns a rectangle shape)
enclose({circle, {_X, _Y}, Radius}) ->
	Diameter = Radius * 2,
	{rectangle, {0, 0}, Diameter, Diameter};
enclose({rectangle, {X, Y}, Height, Width}) ->
	% return same rectangle that was input
	% (is there a shorthand way to do this?)
	{rectangle, {X, Y}, Height, Width};	% or reset X and Y to zero for consistency?
enclose({triangle, {_X, _Y}, SideLength1, SideLength2, SideLength3}) ->
	% oops. not sure I can do this accurately without the coords of each
	% triangle point.
	% http://www.mathopenref.com/coordtriangleareabox.html looks super
	% useful here

	% FTTB use a square of biggest side length
	MaxSideLength = max(SideLength1, max(SideLength2, SideLength3)),	% from lesson 1.15 ;-)
	{rectangle, {0, 0}, MaxSideLength, MaxSideLength}.

% test for perimeter (and return "passed?" boolean)
testPerimeter() ->
	io:format("Testing perimeter()~n"),
	(perimeter({circle, {10, 10}, 1}) == 6.283185307179586) and	% how to round / reduce the precision here?
	(perimeter({rectangle, {10, 10}, 10, 20}) == 60) and
	(perimeter({triangle, {10, 10}, 3, 4, 5}) == 12).

% test for area (and return "passed?" boolean)
testArea() ->
	io:format("Testing area()~n"),
	(area({circle, {10, 10}, 1}) == 3.141592653589793) and	% how to round / reduce the precision here?
	(area({rectangle, {10, 10}, 10, 20}) == 200) and
	(area({triangle, {10, 10}, 3, 4, 5}) == 6).	% result from area() calc will be 6.0 but a 6 here matches too

% test for enclose (and return "passed?" boolean)
testEnclose() ->
	io:format("Testing enclose()~n"),
	(enclose({circle, {10, 10}, 1}) == {rectangle, {0, 0}, 2, 2}) and
	(enclose({rectangle, {10, 10}, 10, 20}) == {rectangle, {10, 10}, 10, 20}) and
	(enclose({triangle, {10, 10}, 3, 4, 5}) == {rectangle, {0, 0}, 5, 5}).

% ==== SUMMING THE BITS ================

% "Define a function bits/1 that takes a positive integer N
% and returns the sum of the bits in the binary representation.
% For example bits(7) is 3 and bits(8) is 1."
%
% or, in Daniel language, "how many 1's in the binary representation?"
bits(N) ->
	bits(N, 128, 0).	% NOTE we only supporting 8 bits unsigned integers (ie. biggest column is 128 so biggest val is 255)

% not sure if the stop case is the CurrentColun reaching 1 or the
% remaining ValueToSpread reaching 0 (or both) :-/

bits(_N, 1, BitsSoFar) ->	% N should prob be zero here
	BitsSoFar;

bits(0, _CurrentColumn, BitsSoFar) ->
	BitsSoFar;

bits(ValueToSpread, CurrentColumn, BitsSoFar)  ->	%  or when N <= 255 ?
	io:format("col ~p, val ~p, bitsSoFar ~p~n", [CurrentColumn, ValueToSpread, BitsSoFar]),
	%BitSetForCurrentColumn = (ValueToSpread >= CurrentColumn),
	DecimalValueInColumn = max(-1, ValueToSpread - CurrentColumn),	% anything less than -1 is clipped to -1
	BitValueForCurrentColumn = min(1, DecimalValueInColumn),
	RemainingValueToSpread = ValueToSpread - (CurrentColumn * BitValueForCurrentColumn),
	
	% move one column right
	bits(RemainingValueToSpread, CurrentColumn / 2, BitsSoFar + BitValueForCurrentColumn).

% test for bits (and return "passed?" boolean)
testBits() ->
	io:format("Testing bits()~n"),
	(bits(7) == 3) and
	(bits(8) == 1).