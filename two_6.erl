-module(two_6).
-export([productDirect/1, productTail/1, testProductFunctions/0]).
-export([maximumDirect/1, maximumTail/1, testMaximumFunctions/0]).

% ==== PRODUCT ================

% NOTE we need the product of an empty list to be 1 as this
% supports the product
% of a one element list being itself

% direct version
productDirect([]) ->
	1;
productDirect([X|Xs]) ->
	X * productDirect(Xs).

% interface for tail version
productTail([]) ->
	1;
productTail([X|Xs]) ->
	productTail([X|Xs], 1).

% tail version
productTail([], ProductSoFar) ->
	ProductSoFar;
productTail([X|Xs], ProductSoFar) ->
	productTail(Xs, ProductSoFar * X).

% test product functions
testProductFunctions() ->
	io:format("Testing product functions~n"),
	true == (
		(productDirect([]) == 1)
		and (productDirect([4]) == 4)
		and (productDirect([1,3,1,2,2]) == 12)
		and (productTail([]) == 1)
		and (productTail([4]) == 4)
		and (productTail([1,3,1,2,2]) == 12)
	).

% ==== MAXIMUM ================

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

% test maximum functions
testMaximumFunctions() ->
	io:format("Testing maximum functions~n"),
	true == (
		(maximumDirect([1]) == 1)
		and (maximumDirect([4,8]) == 8)
		and (maximumDirect([1,3,1,2,2]) == 3)
		and (maximumTail([1]) == 1)
		and (maximumTail([4,8]) == 8)
		and (maximumTail([1,3,1,2,2]) == 3)
	).
	