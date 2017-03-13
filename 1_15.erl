% bad module name ("syntax error before: _15")
-module(1_15).
-export([exclusive_or / 2]).

exclusive_or(true, false) ->
	true;
exclusive_or(false, true) ->
	true;
exclusive_or(X, Y) ->
	false.