% valid module name
-module(one_15).
-export([x_or_1 / 2, x_or_2 / 2, x_or_3 / 2]).
-export([x_or_4 / 2, x_or_5 / 2, x_or_6 / 2]).
-export([maxThree / 3]).
-export([howManyEqual / 3]).

% ==== EXCLUSIVE OR ================

% first method (from vid)
% (note that x_or_1(7, 6) will give false)
x_or_1(true, false) ->
	true;
x_or_1(false, true) ->
	true;
x_or_1(X, Y) ->
	false.

% second method (from vid)
% (note that x_or_2(7, 6) will give false)
x_or_2(true, false) ->
	true;
x_or_2(false, true) ->
	true;
x_or_2(_, _) ->
	false.

% third method (from vid)
% (note that x_or_3(7, 6) will give true)
x_or_3(X, X) ->
	false;
x_or_3(_, _) ->
	true.

% and now at least three others from myself:

% first bash, gives "illegal pattern" error
%x_or_4(X, Y == X) ->
%	false;
%x_or_4(_, _) ->
%	true.

% second attempt, using inequality operator, seems to work!
% (note that x_or_4(7, 6) will give true)
x_or_4(X, Y) ->
	Y =/= X.

% third attempt, using equality operator and not() function, seems to work!
% (note that x_or_5(7, 6) will give "exception error: bad argument")
x_or_5(X, Y) ->
	Y == not(X).

% fourth attempt, using and / or logic, seems to work!
% (note that x_or_6(7, 6) will give "exception error: bad argument")
x_or_6(X, Y) ->
%	(X == true and Y == false) or (X == false and Y == true).	%nope, syntax error
%	(X = true and Y = false) or (X = false and Y = true).	%nope, illegal pattern
	(X and not(Y)) or (Y and not(X)).

% so presumably "one_15:x_or_1(true, false) == one_15:x_or_2(true, false) == ... ."
% is a simple way to check that all of these functions are giving equivalent results??

% ==== MAXIMUM OF THREE ================

% a "return the max of the maxes" type approach, seems to work!
maxThree(X, Y, Z) ->
	max(X, max(Y, Z)).

% I suspect there's also an approach involving one max() call and a
% greater than operation...

% ==== HOW MANY EQUAL? ================

% first attempt purely using combinations of argument pattern matching
% and a "don't care" case at the end
% (which will no doubt get silly for longer argument lists)
howManyEqual(X, X, X) ->
	3;
howManyEqual(X, X, Y) ->
	2;
howManyEqual(X, Y, X) ->
	2;
howManyEqual(Y, X, X) ->
	2;
howManyEqual(_, _, _) ->
	0.
