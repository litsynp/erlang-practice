-module(pattern_matching).
-export([
    greet/2,
    head/1,
    second/1,
    same/2,
    valid_time/1,
    old_enough/1,
    right_age/1,
    wrong_age/1,
    % heh_fine/0,
    oh_god/1,
    help_me/1,
    insert/2,
    beach/1,
    beachf/1
]).

% Pattern Matching
greet(male, Name) ->
    io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) ->
    io:format("Hello, Ms. ~s!", [Name]);
greet(_, Name) ->
    io:format("Hello, ~s!", [Name]).

head([H | _]) -> H.

second([_, X | _]) -> X.

same(X, X) -> true;
same(_, _) -> false.

valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) ->
    io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n", [Date, Y, M, D]),
    io:format("The Time tuple (~p) says the time is: ~p:~p:~p.~n", [Time, H, Min, S]);
valid_time(_) ->
    io:format("Stop feeding me wrong data!~n").

% ====================================
% Guards
% Guard expression must return true or false.
old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.

% Comma acts like andalso.
% Both guard expressions need to succeed for the whole guard to pass.
right_age(X) when X >= 16, X =< 104 ->
    true;
right_age(_) ->
    false.

% Semicolon acts like orelse.
% If the first guard fails, the second guard is checked, and then the next one, until one of them succeeds.
wrong_age(X) when X < 16; x > 104 ->
    true;
wrong_age(_) ->
    false.

% ====================================
% If
% Ifs act like guards and share guards' syntax, but outside of a function clause's head.
% If clauses are called Guard Patterns.
% Erlang's ifs are different from other languages' ifs!
% heh_fine() ->
%     if
%         1 =:= 1 ->
%             works
%     end,
%     if
%         1 =:= 2; 1 =:= 1 ->
%             works
%     end,
%     if
%         1 =:= 2, 1 =:= 1 ->
%             fails;
%         % In Erlang, everything has to return something.
%         % If Erlang can't find a way to have a guard succeed, it will crash.
%         true ->
%             works
%     end.

% So we need to add a catch-all branch, which is 'else'.
% In Erlang, we use 'true'.
oh_god(N) ->
    if
        N =:= 2 -> might_succeed;
        true -> always_does
    end.

help_me(Animal) ->
    Talk =
        if
            Animal == cat -> "meow";
            Animal == beef -> "moo";
            Animal == dog -> "bark";
            Animal == tree -> "bark";
            true -> "fgdadfgna"
        end,
    {Animal, "says " ++ Talk ++ "!"}.

% Else or 'true' branches should be avoided altogether.

% ====================================
% case ... of
insert(X, []) ->
    [X];
insert(X, Set) ->
    case lists:member(X, Set) of
        true -> Set;
        false -> [X | Set]
    end.

beach(Temperature) ->
    case Temperature of
        {celsius, N} when N >= 20, N =< 45 ->
            'favorable';
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favorable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favorable in the US';
        _ ->
            'avoid beach'
    end.

% which is equivalent to:
beachf({celsius, N}) when N >= 20, N =< 45 -> 'favorable';
beachf({kelvin, N}) when N >= 293, N =< 318 -> 'scientifically favorable';
beachf({fahrenheit, N}) when N >= 68, N =< 113 -> 'favorable in the US';
beachf(_) -> 'avoid beach'.
