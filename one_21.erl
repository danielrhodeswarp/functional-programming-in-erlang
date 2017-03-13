-module(one_21).
-export([fib / 1, newFib / 1]).

% from lesson 1.20 ("is exponentially complex … ouch!")
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N - 2) + fib(N - 1).

% "Define an efficient Fibonacci function fib/3 using
% a tail recursion with two accumulating parameters
% that hold the last two Fibonacci numbers."

% I got this bit right!
newFib(N) ->
	newFib(N, 0, 1).

%newFib(N, Penultimate, Last) -> when N = 1
%	0;
%newFib(N, Penultimate, Last) -> when N = 2
%	1;
%newFib(N, Penultimate, Last) -> when N > 2
%	newFib(N - 1, )

%newFib(N, Penultimate, Last) -> when N = 1
%	0;
%newFib(N, Penultimate, Last) -> when N = 2
%	1;
%newFib(N, Penultimate, Last) -> when N > 2
%	newFib(N - 1, )

% from 1.22 lesson video
newFib(0, Previous, _Current) ->
	Previous;
newFib(N, Previous, Current) ->
	newFib(N - 1, Current, Previous + Current).