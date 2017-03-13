% bad module name ("{error,function_clause}")
-module(1-15).
-export([exclusive_or / 2]).

exclusive_or(true, false) ->
	true;
exclusive_or(false, true) ->
	true;
exclusive_or(X, Y) ->
	false.