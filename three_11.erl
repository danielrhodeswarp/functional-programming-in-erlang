-module(three_11).
-export([compose/2, function_composer/1]).

% ==== Composition ================
%
% Composition is associative – meaning that it doesn’t matter how you
% bracket a composition of three functions – but it’s not commutative – so that
% F composed with G is not necessarily the same as G composed with F.
% Find examples of F and G to show cases when the two compositions are the same,
% and when they are different.

compose(F, G) ->
	fun (X) -> G(F(X)) end.

% with this:
% lists:map(three_11:compose(fun (X) -> X + 2 end, fun (Y) -> Y - 3 end), [1,2,3,4,5,6,7,8,9]).
% and
% lists:map(three_11:compose(fun (Y) -> Y - 3 end, fun (X) -> X + 2 end), [1,2,3,4,5,6,7,8,9]).
% both give: [0,1,2,3,4,5,6,7,8]
%
% however:
% lists:map(three_11:compose(fun (X) -> X + 1 end, fun (Y) -> Y / 2 end), [1,2,3,4,5,6,7,8,9]).
% and
% lists:map(three_11:compose(fun (Y) -> Y / 2 end, fun (X) -> X + 1 end), [1,2,3,4,5,6,7,8,9]).
% give differing results


% Define a function that takes a list of functions and composes them together.
% Can you use any of the functions in the lists module to help you?


function_composer([]) -> [];	% ??
function_composer([X|Xs]) ->
		% this works as I would expect
	lists:foldr(fun compose/2, fun (T) -> T end, [X|Xs]).	% what is good for "empty function"?
	%lists:foldr(fun compose/2, X, Xs).	% works but I feel like the last or first function is being missed

% can test this in shell like:
% Combo = three_11:function_composer([fun (X) -> X+1 end, fun (Y) -> Y*2 end, fun (Z) -> Z-1 end]).
% Combo(1).
% 3
% Combo(7).
% 15

%nope, didn't even test this - realised I just need compose/2
%add_function(Func1, Func2) ->
%	Func2(Func1).