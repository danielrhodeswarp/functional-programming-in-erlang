-module(one_19).
-export([fac / 1, test / 1, getFromList / 1, getLastTwo / 1, fib / 1]).

fac(0) ->
	1;
fac(N) when N > 0 ->
	fac(N - 1) * N.

% ==== FIBONACCI NUMBERS ================

% my bash at summat to do with Fibonacci

% ==== testing ================

% works
test(0) ->
	[];
test(X) ->
	test(X - 1) ++ [X].

getFromList(L) ->
	%lists:last(L).	% works
	length(L).	%works

% works
getLastTwo(L) ->

	[lists:nth(length(L) - 1, L), lists:nth(length(L), L)].

% ==== /end testing ================

% get a fibonacci sequence having X terms

% in english "fib list for X items is fib list for (X - 1) items plus
% sum of last two items in fib list for (X - 1)"

% note we start at zero (but could also start at 1)

fib(1) ->
	[0];
fib(2) ->
	[0,1];
fib(X) ->
	L = fib(X - 1),
	L ++ [lists:nth(length(L) - 1, L) + lists:nth(length(L), L)].

% manual evaluation:
% fib(4) =
% fib(3) ++ sum(fib(3):last, fib(3):penultimate)

% fib(3) =
% fib(2) ++ sum(fib(2):last, fib(2):penultimate)

% fib(2) = 
% [0, 1] (a hard-coded stop case)

% so, fib(3) is: [0, 1] ++ sum(0, 1) = [0, 1, 1]
% then fib(4) is: [0, 1, 1] ++ sum(1, 1) = [0, 1, 1, 2]   

