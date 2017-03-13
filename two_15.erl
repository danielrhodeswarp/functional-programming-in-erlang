-module(two_15).
-export([testPalindrome/0]).
-export([palindrome/1]).
-export([removeNonPhonemes/1, removeChars/2, toLowerCase/1, removeAllOneChar/2]).

% NOTE unicode not supported

% Define a function palindrome, that returns true or false
% depending on whether the list is a palindrome - the same when
% read right to left as when read left to right.
% (ignoring case, whitespace and non phonemic characters)
%
% eg. palindrome("Madam I\'m Adam") = true

% test-driven development ;-)
testPalindrome() ->
	io:format("Testing palindrome/1~n"),
	(palindrome("Madam I\'m Adam") == true) and
	(palindrome("noon") == true) and
	(palindrome("A man, a plan, a canal - Panama!") == true) and
	(palindrome("Never odd or even") == true) and
	(palindrome("This is not a palindrome") == false) and
	(palindrome("The quick brown fox jumped over a whatnot") == false).

% the main event
palindrome([]) -> false;
palindrome([X|Xs]) ->
	Normalised = removeNonPhonemes(toLowerCase([X|Xs])),
	Normalised == lists:reverse(Normalised).

% ==== UTILITY ================

% named wrapper for removeChars/2
removeNonPhonemes([]) -> [];
removeNonPhonemes([X|Xs]) ->
	removeChars([X|Xs], "?.,-+_=()*&^\"\'%$£").

% removeChars(inputString, charsToRemove)
removeChars([], []) -> [];
removeChars([X|Xs], []) -> [];
removeChars([], [X|Xs]) -> [];

% CAN'T GET THIS TO BLOO*DY WORK! (something to do with the above shortcuts for empty lists?)
removeChars([X|Xs], [X|Ys]) -> % first of inputString is same as first of charsToRemove
	%[Xs|removeChars(Xs, Ys)];
	io:format("first of input is same as first of charsToRemove~n"),
	removeChars(Xs, [X|Ys]);
removeChars([X|Xs], [Y|Ys]) -> 
	io:format("first of input is NOT same as first of charsToRemove~n"),
	% [X|removeChars(Xs, [Y|Ys])].	% so double recursion basically
	Removed = removeAllOneChar(Xs, Y),
	io:format("first char of result: ~p, before: ~p, after: ~p, Y:~p~n", [X, Xs, Removed, Y]),
	[X|removeChars(removeAllOneChar(Xs, [Y]), Ys)].	% so double recursion basically

% remove one specific character from a string
removeAllOneChar([], X) -> [];
removeAllOneChar([X|Xs], Y) when [X] == Y ->	% this when clause took me a while to figure out but looks dorky
	% io:format("oneChar same as head~n"),
	removeAllOneChar(Xs, Y);
removeAllOneChar([X|Xs], Y) ->
	% io:format("recursing: [X|Xs] is ~p, Y is ~p~n", [[X|Xs], Y]),
	[X|removeAllOneChar(Xs, Y)].

% transform any and all [A-Z] to [a-z]
toLowerCase([]) -> [].