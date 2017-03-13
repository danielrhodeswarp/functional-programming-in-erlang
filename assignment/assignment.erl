-module(assignment).
-export([checkFile/1, indexFile/1, tokeniseLine/1]).
-export([addToWordTuple/2, wordInWordTupleList/2]).
-export([rangifyList/1]).

% NOTE you'll need to compile index.erl in the Erlang shell first!

% Indexing a file
% The aim of this exercise is to index a text file, by line number.
% We can think of the input being a list of text strings,
% and below we’ve provided an outline Erlang module that reads text files into this format,
% as well as a couple of example files to process.
%
% The output of the main function should be a list of entries consisting
% of a word and a list of the ranges of lines on which it occurs.
%
% For example, the entry
%
% { "foo" , [{3,5},{7,7},{11,13}] }
%
% means that the word "foo" occurs on lines 3, 4, 5, 7, 11, 12 and 13 in the file.
%
% To take the problem further,
% you might like to think about these ways of refining the solution.
%
% Removing all short words (e.g. words of length less than 3) or
% all common words (you‘ll have to think about how to define these).
%
% Sorting the output so that the words occur in lexicographic order.
%
% Normalising the words so that capitalised ("Foo") and
% non capitalised versions ("foo") of a word are identified.
%
% Normalising so that common endings, plurals etc. identified.
%
% (Harder) Thinking how you could make the data representation more efficient
% than the one you first chose.
% This might be efficient for lookup only, or for both creation and lookup.
%
% Can you think of other ways that you might extend your solution?

% debugger
checkFile(File) ->
	index:show_file_contents(index:get_file_contents(File)).

% ==== the main event ================

indexFile(File) ->
	tokeniseLines(index:get_file_contents(File)).

% ====================================

%tokeniseLines([]) -> [];
%tokeniseLines([L|Ls]) ->
%	[tokeniseLine(L) | tokeniseLines(Ls)].

% tail version to accumulate the line number
tokeniseLines([]) -> [];
tokeniseLines([L|Ls]) ->
	tokeniseLines([L|Ls], [], 1).

tokeniseLines([], LineTokensAcc, _LineAcc) ->
	lists:reverse(LineTokensAcc);
tokeniseLines([L|Ls], LineTokensAcc, LineAcc) ->
	tokeniseLines(Ls, addToWordTupleList(LineAcc, tokeniseLine(L)) ++ LineTokensAcc, LineAcc + 1).
	

% a map / transform type o' function
addToWordTupleList(_X, []) -> [];
addToWordTupleList(X, [Y|Ys]) ->
	[addToWordTuple(X, Y) | addToWordTupleList(X, Ys)].

% WORKING
addToWordTuple(X, {Y}) ->
	{Y, [X]};
addToWordTuple(X, {Y, []}) ->
	{Y, [X]};
addToWordTuple(X, {Y, [Z|Zs]}) ->
	{Y, [X|[Z|Zs]]}.

%tokeniseLine(L) ->
%	% true.
%	{wordTuple, []}.

% split on " " (space)
% NOTE multiple whitespace support is a TODO
tokeniseLine([]) -> [];
tokeniseLine([X|Xs]) ->
	%io:format("in interface ~p~n", [[X|Xs]]),
	tokeniseLine([X|Xs], [], []).

% StreamAcc is "non-whitespace so far"
% TokensAcc is ultimate result
% TODO support unifying of duplicated words on same line
tokeniseLine([], StreamAcc, TokensAcc) when length(TokensAcc) > 1 ->	% don't know why the interface jumps straight to here (ie. the when clause is needed)
	%io:format("1~n"),
	%lists:reverse(TokensAcc);	%nope, this misses off the last stream accumuation!
	lists:reverse([{lists:reverse(StreamAcc)} | TokensAcc]);

tokeniseLine([X|Xs], StreamAcc, TokensAcc) ->
	%io:format("2~n"),
	case X == 32 of	% space character
	%case [X] of
		true ->
		%[" "] ->
			%io:format("true~n"),
			tokeniseLine(Xs, [], [{lists:reverse(StreamAcc)} | TokensAcc]);	% put stream on final result but reset stream too 
		_ ->
			%io:format("false~n"),
			tokeniseLine(Xs, [X|StreamAcc], TokensAcc)
	end.




% WORKING
% parms: WordToFind, WordTupleList
%wordInWordTupleList() -> false;
wordInWordTupleList([_X|_Xs], []) ->
	false;
wordInWordTupleList([X|Xs], [Y|Ys]) ->
	% breakdown the first tuple
	{Word, _NumberList} = Y,
	case Word of
		[X|Xs] ->
			true;
		_ ->
			wordInWordTupleList([X|Xs], Ys)
	end.



% WORKING
%
% run-length encode a list of (sorted, with no dublies) integers such that:
% [3, 4, 5, 7, 11, 12, 13]
% becomes:
% [{3,5},{7,7},{11,13}]
% (NOTE that even a single occurrence is still a range)
rangifyList([]) -> [];
rangifyList([X|Xs]) ->
	rangifyList([X|Xs], [X], []).	 %seeding StreamAcc with [X] here actually
									% introduces a duff initial element to the result
									% list which we have to chop off

% StreamAcc is "sequence so far"
% this function needed a lot of trial and error
rangifyList([], StreamAcc, ResultAcc) ->
	%ResultAcc;	%nope, misses out the last StreamAcc
	[StreamCurrent|_StreamRest] = StreamAcc,
	StreamStarter = lists:nth(1, lists:reverse(StreamAcc)),
	WithDuffInitial = ResultAcc ++ [{StreamStarter, StreamCurrent}],
	[_Duff|Want] = WithDuffInitial,
	Want;
rangifyList([X|Xs], StreamAcc, ResultAcc) ->
	[StreamCurrent|_StreamRest] = StreamAcc,
	
	%io:format("StreamAcc: ~p~n", [StreamAcc]),
	%io:format("Current: ~p, Rest: ~p~n", [StreamCurrent, StreamRest]),
	%io:format("X: ~p~n", [X]),
	case (X == StreamCurrent + 1) of
		% is our current input number equal to last input number + 1?
		true ->
			%io:format("building~n"),
			rangifyList(Xs, [X|StreamAcc], ResultAcc);	% actually need to add backwards to make line 142 simpler
		_ ->
			%io:format("adding~n"),
			StreamStarter = lists:nth(1, lists:reverse(StreamAcc)),
			rangifyList(Xs, [X], ResultAcc ++ [{StreamStarter, StreamCurrent}])
	end.