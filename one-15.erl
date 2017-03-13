% bad module name ("an error occurred when evaluating an arithmetic expression")
-module(one-15).
-export([exclusive_or / 2]).

exclusive_or(true, false) ->
	true;
exclusive_or(false, true) ->
	true;
exclusive_or(X, Y) ->
	false.