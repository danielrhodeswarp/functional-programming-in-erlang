-module(two_9).
-export([double/1, doubleTail/1, evens/1, evensTail/1]).
-export([median/1, medianOfSortedList/1]).
-export([sortListAsc/1, minimumTail/1, minimumDirect/1, maximumTail/1, maximumDirect/1]).
-export([howManyTimesIsXInList/2, minimumWithFloor/2, getListWithXYs/2]).
-export([nth/2]).

% ==== DOUBLE ================

% doubles the elements in a list of numbers (a "map" function)
% This is direct recursion.
double([]) ->
	[];
double([X|Xs]) ->
	[X * 2 | double(Xs)].

% interface for tail version of double() [called 'doubleTail()']
doubleTail([]) ->
	[];
doubleTail([X|Xs]) ->
	doubleTail([X|Xs], []).

% implementation for tail version of double() [called 'doubleTail()']
doubleTail([], ListSoFar) ->
	ListSoFar;
doubleTail([X|Xs], ListSoFar) ->
	% doubleTail(Xs, [X|ListSoFar]).	% LOL this first attempt actually reverses the list!
	% doubleTail(Xs, [ListSoFar|(X * 2)]).	% nope, does weird things with the array (probably because ListSoFar isn't a list in my function definition)
	doubleTail(Xs, ListSoFar ++ [X * 2]).	% yep, seems to work

% ==== EVENS ================

% extract only even numbers from a list of integers (a "filter" function)
evens([]) ->
	[]; 

% nope, epic pattern matching fail here
%evens([X|Xs]) ->
%	case X of
%		%Xtemp = {circle,{_,_},_} ->
%		X rem 2 == 0 ->
%		    [X | evens(Xs)];
%		_ ->
%		    evens(Xs)
%	end.

% yep, a cheeky guard clause saves the day :D
evens([X|Xs]) when X rem 2 == 0 ->
	[X | evens(Xs)];
evens([_X|Xs]) ->
	evens(Xs).

% and now in a tail recursion style

% interface
evensTail([]) ->
	[];
evensTail([X|Xs]) ->
	evensTail([X|Xs], []).


% implementation
evensTail([], ListSoFar) ->	% how to specify that ListSoFar must be an empty *OR* non empty list??
	ListSoFar;
evensTail([X|Xs], ListSoFar) when X rem 2 == 0 ->
	%evensTail(Xs, [ListSoFar | X]);	% nope
	evensTail(Xs, ListSoFar ++ [X]);	% yep, seems to work
evensTail([_X|Xs], ListSoFar) ->
	evensTail(Xs, ListSoFar).


% ==== MEDIAN ================

% median is a "reduce" function

median([]) ->
	[];
median([X|Xs]) ->
	medianOfSortedList(sortListAsc([X|Xs])).

% assume list already sorted
medianOfSortedList([]) ->
	[];
			% er, I thought we weren't allowed to have functions in guard clauses?
medianOfSortedList([X|Xs]) when length([X|Xs]) rem 2 == 0 ->
	FirstMidway = length([X|Xs]) div 2,
	(lists:nth(FirstMidway, [X|Xs]) + lists:nth(FirstMidway + 1, [X|Xs])) / 2;
medianOfSortedList([X|Xs]) ->
	Midway = (length([X|Xs]) div 2) + 1,
	lists:nth(Midway, [X|Xs]).	% Erlang lists seem to index starting at '1'


% ==== UTILITY ================

% sort a list's values into ascending numerical order
% NOPE because this is only a one-pass 
% (it's basically bubble-sort which has to make a bunch of passes)
xsortListAsc([]) ->
	[];
xsortListAsc([X]) ->
	[X];
xsortListAsc([X,Y|Zs]) ->
	[min(X, Y), max(X, Y) | Zs].

ysortListAsc([]) ->
	[];
ysortListAsc([X]) ->
	[X];
ysortListAsc([X|Xs]) ->
	[min(X, minimumDirect(Xs)) | Xs].	% nope

% prob only supports positive integers
sortListAsc([]) ->
	[];
sortListAsc([X]) ->
	[X];
sortListAsc([X|Xs]) ->
	sortListAsc([X|Xs], 0, []).

%sortListAsc([], CurrentFloor, ResultListSoFar) ->
%	ResultListSoFar;
sortListAsc([X|Xs], _CurrentFloor, ResultListSoFar) when length([X|Xs]) == length(ResultListSoFar) ->
	ResultListSoFar;
sortListAsc([X|Xs], CurrentFloor, ResultListSoFar) ->
	%io:format("[X|Xs] = ~p, CurrentFloor = ~p, ResultListSoFart = ~p~n", [[X|Xs], CurrentFloor, ResultListSoFar]),
	Minimum = minimumWithFloor([X|Xs], CurrentFloor),
	%sortListAsc([X|Xs], Minimum, ResultListSoFar ++ [Minimum]).	% works well BUT not support repeating elements in the list
	sortListAsc([X|Xs], Minimum, ResultListSoFar ++ getListWithXYs(howManyTimesIsXInList(Minimum, [X|Xs]), Minimum)).
		% at first I though the above line might lengthen the result list but it doesn't and
		% it seems to work and this is probably due to how we are simply changing CurrentFloor and checking for length(input) == length(result)

% function to return a list with "seven sixes" (ie. [6,6,6,6,6,6,6]) or what-have-you
% (allows sortListAsc/1 to support repeating elements in the list)
getListWithXYs(X, Y) ->
	getListWithXYs(X, Y, []).

getListWithXYs(0, _Y, ResultListSoFar) ->
	ResultListSoFar;
getListWithXYs(X, Y, ResultListSoFar) ->
	getListWithXYs(X - 1, Y, ResultListSoFar ++ [Y]).
	

% maximum/1 as from lesson 2.6

% direct version
% maximumDirect([]) -> 0;	% supports calling with an empty list
maximumDirect([X]) ->
	X;
maximumDirect([X|Xs]) ->
	max(X, maximumDirect(Xs)).

% interface for tail version
maximumTail([X]) ->
	X;
maximumTail([X|Xs]) ->
	maximumTail([X|Xs], 0).

% tail version
maximumTail([], MaxSoFar) ->
	MaxSoFar;
maximumTail([X|Xs], MaxSoFar) ->
	maximumTail(Xs, max(X, MaxSoFar)).

% minimum/1 (opposite of maximum/1)

% direct version
% maximumDirect([]) -> 0;	% supports calling with an empty list
minimumDirect([X]) ->
	X;
minimumDirect([X|Xs]) ->
	min(X, minimumDirect(Xs)).

% interface for tail version
minimumTail([X]) ->
	X;
minimumTail([X|Xs]) ->
	minimumTail([X|Xs], 0).

% tail version
minimumTail([], MinSoFar) ->
	MinSoFar;
minimumTail([X|Xs], MinSoFar) ->
	minimumTail(Xs, min(X, MinSoFar)).

% minimum with floor
%yminimumWithFloor([X], Floor) -> X;	% or?
%yminimumWithFloor([X|Xs], Floor) ->
%	%Min = minimumDirect([X|Xs]),
%	case X of
%		>= Floor ->
%			Min;
%%			false	% or?
%	end.

% minimum with floor (ie. a floor of zero will just be a normal minimum() )
% full of holes this one :-( 

% interface for tail version
minimumWithFloor([X], _Floor) ->
	X;
minimumWithFloor([X|Xs], Floor) ->
	minimumWithFloor([X|Xs], Floor, maximumDirect([X|Xs])).	% what to seed with?

% tail version
minimumWithFloor([], _Floor, LowestAndClosestSoFar) ->
	LowestAndClosestSoFar;
minimumWithFloor([X|Xs], Floor, LowestAndClosestSoFar) ->
	%Minimum = minimumDirect([X|Xs]),	% nope! works (by accident) on asc sorted lists but not desc sorted lists
	Minimum = X,	% yep, this is what we mean!
	%io:format("[X|Xs] = ~p, Floor = ~p, LowestAndClosestSoFar = ~p, [calcd min = ~p]~n", [[X|Xs], Floor, LowestAndClosestSoFar, Minimum]),
	case Minimum of
		Minimum when (Minimum > Floor) and (Minimum < LowestAndClosestSoFar) ->
			%io:format("a case match~n"),
			minimumWithFloor(Xs, Floor, Minimum);
		_ ->
			minimumWithFloor(Xs, Floor, LowestAndClosestSoFar)
	end.
	


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


% Daniel's attempt at hand-rolling lists:nth
% (not quite there yet but seems to be possible)
nth(N, []) ->
	[];
nth(N, [X|Xs]) ->
	nth(N, [X|Xs], 0, X).

nth(N, [X|Xs], CurrentIndex, CurrentValue) when N == CurrentIndex ->
	io:format("match: ~n"),
	CurrentValue;
nth(N, [X|Xs], CurrentIndex, CurrentValue) ->
	io:format("recursing: N = ~p, [X|Xs] = ~p, CurrentINdex = ~p, CurrentValue = ~p ~n", [N, [X|Xs], CurrentIndex, CurrentValue]),
	nth(N, Xs, CurrentIndex + 1, X).









