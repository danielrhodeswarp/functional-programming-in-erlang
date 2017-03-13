-module(rps).
-export([play/1,echo/1,play_two/3,rock/1,no_repeat/1,const/1,enum/1,cycle/1,rand/1,val/1,tournament/2]).


%
% play one strategy against another, for N moves.
%

play_two(StrategyL,StrategyR,N) ->
    play_two(StrategyL,StrategyR,[],[],N).

% tail recursive loop for play_two/3
% 0 case computes the result of the tournament

% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

play_two(_,_,PlaysL,PlaysR,0) ->
   % compute result of tournament
   dummy;

play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
   % tail recurse. decrement N.
   dummy.

%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[]).

% tail recursive loop for play/1

play(Strategy,Moves) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
	stop ->
	    io:format("Stopped~n");
	_    ->
        OpponentsMove = Strategy(Moves),    % for better output and therefore debugging / testing
	    io:format("Opponent played: ~p~n", [OpponentsMove]),   % for better output and therefore debugging / testing
        Result = result(Play, OpponentsMove),
	    io:format("Result: ~p~n",[Result]),
	    play(Strategy,[Play|Moves])    % add current play onto moves history
    end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form
    
expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

% result of a tournament

tournament(PlaysL,PlaysR) ->
    lists:sum(
      lists:map(fun outcome/1,
		lists:zipwith(fun result/2,PlaysL,PlaysR))).

outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) ->
    rock;
enum(1) ->
    paper;
enum(2) ->
    scissors.

val(rock) ->
    0;
val(paper) ->
    1;
val(scissors) ->
    2.

% give the play which the argument beats.

beats(rock) ->
    scissors;
beats(paper) ->
    rock;
beats(scissors) ->
    paper.

%
% strategies.
%
echo([]) ->
     paper; % arbitrary starter
echo([Last|_]) ->
    Last.

rock(_) ->
    rock.



% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

% assume that your opponent never repeats herself:
% if you know this you can make a choice that will never lose (but might draw);
no_repeat([]) ->
    paper;  % arbitrary starter
no_repeat([Last|_]) ->
    % play a move that we know won't lose against not-Last
    % if:
    % Last == rock
    % then next will be:
    % (paper OR scissors)
    % and a not-losing move against (paper OR scissors) is:
    % scissors
    % why scissors? well, if it's - say - rock then opponent's paper will win.
    % if it's - say - paper then opponent's scissors will win.
    % so basically we play the WINNING or strongest move of the two non-Last moves

    % APPROACH
    % [1] remove Last from list of all possible moves
    % [2] return strongest of remaining two moves from [1]
    
    % [1]
    [Next1, Next2] = lists:filter(fun (X) when X == Last -> false; (X) -> true end, [rock, paper, scissors]),   % [rock, paper, scissors] feels a bit hard-codingy

    case result(Next1, Next2) of
        win ->
            Next1;
        lose ->
            Next2

    end.

const(Play) ->
    dummy.

cycle(Xs) ->
    enum(length(Xs) rem 3). % but I read somewhere that in Erlang is isn't "best practice" to rely on list lengths..??!??

% make a random choice each time;
% you may want to use the random:uniform/1 function so that random:uniform(N) returns
% a random choice of 1,2, … N with equal probability each time
rand(_) ->  % we no care if passed arg is empty or not
    enum(rand:uniform(3) - 1).
