-module(three_5).
-export([doubleAll/1, evens/1, product/1]).
-export([zip/2, zip_with/3, zip_with_using_zip_and_map/3, zip_using_zip_with/2]).



% ==== USING HIGHER-ORDER FUNCTIONS ================

% @link http://erlang.org/doc/man/lists.html

% Define the functions doubleAll, evens, and product using the
% higher-order functions lists:map, lists:filter and lists:foldr.

% doubleAll([]) -> [];
% doubleAll([X|Xs]) ->
%     [ 2*X | doubleAll(Xs) ].

% evens([]) -> [];
% evens([X|Xs]) when X rem 2 == 0 ->
%     [X | evens(Xs) ];
% evens([_|Xs]) ->
%     evens(Xs).

% product([]) -> 1;
% product([X|Xs]) -> X * product(Xs).


% doubleAll is a MAP
%doubleAll([]) -> [];	% looks like lists:map/2 takes care of this for us :-)
doubleAll(Things) -> lists:map(fun (X) -> X * 2 end, Things).	% cheeky lambda

% evens is a FILTER
evens(Things) -> lists:filter(fun is_even/1, Things).

% for above (would look messy as a lambda [UPDATE actually probably not])
is_even(X) when X rem 2 == 0 ->
	true;
is_even(_X) ->
	false.

% product is a REDUCE
% mostly copied this from the Erlang docs.
% the lambda function takes two args and it seems
% the first arg is the current list item and the second arg is the 
% accumulated result so far. Simples!
product(Things) -> lists:foldr(fun (X, Prod) -> X * Prod end, 1, Things).

% ==== Zipping ================

% a)
% ==
%
% Define a function zip/2 that “zips together” pairs
% of elements from two lists like this:
%
% zip([1,3,5,7], [2,4]) = [ {1,2}, {3,4} ]
%
% where you can see that the elements from the longer list are lost.

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

% b)
% ==
% 
% Define a function zip_with/3 that “zips together” pairs of
% elements from two lists using the function in the first argument, like this:
%
% zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]) = [ 3, 7 ]


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

% c)
% ==
%
% Re-define the function zip_with/3 using zip and lists:map.
%

% My approach here is: run zip/2 and then map a summation function over
% each tuple in the resultset

% fun being, for example, fun ({X, Y}) -> X + Y end
zip_with_using_zip_and_map(Fun, [X|Xs], [Y|Ys]) ->
	lists:map(Fun, zip([X|Xs], [Y|Ys])).

% d)
% ==
%
% Re-define zip/2 using zip_with/3.

% My approach here is: simply pass a function into zip_with that makes a tuple
% out of the X and Y elements

zip_using_zip_with([X|Xs], [Y|Ys]) ->
	zip_with(fun (X, Y) -> {X, Y} end, [X|Xs], [Y|Ys]).