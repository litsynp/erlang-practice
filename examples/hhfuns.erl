-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

increment([]) -> [];
increment([H | T]) -> [H + 1 | increment(T)].

decrement([]) -> [];
decrement([H | T]) -> [H + 1 | decrement(T)].

% can be abstracted to:
% Usage: hhfuns:map(fun hhfuns:incr/1, [1, 2, 3]).
% Usage 2: hhfuns:map(fun (X) -> X + 1 end, [1, 2, 3]).
map(_, []) -> [];
map(F, [H | T]) -> [F(H) | map(F, T)].
% Use lists:map/2 instead.

incr(X) -> X + 1.
decr(X) -> X - 1.

a() ->
    Secret = "pony",
    fun() -> Secret end.

b(F) ->
    "a/0's password is " ++ F().

%% only keep even numbers
even(L) -> lists:reverse(even(L, [])).

even([], Acc) ->
    Acc;
even([H | T], Acc) when H rem 2 == 0 ->
    even(T, [H | Acc]);
even([_ | T], Acc) ->
    even(T, Acc).

%% only keep men older than 60
old_men(L) -> lists:reverse(old_men(L, [])).

old_men([], Acc) ->
    Acc;
old_men([Person = {male, Age} | People], Acc) when Age > 60 ->
    old_men(People, [Person | Acc]);
old_men([_ | People], Acc) ->
    old_men(People, Acc).

filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).

% can be abstracted to:
filter(_, [], Acc) ->
    Acc;
filter(Pred, [H | T], Acc) ->
    case Pred(H) of
        true -> filter(Pred, T, [H | Acc]);
        false -> filter(Pred, T, Acc)
    end.
% Use lists:filter/2 instead.

%% find the maxium of a list
max([H | T]) -> max2(T, H).

max2([], Max) -> Max;
max2([H | _], Max) when H > Max -> H;
max2([_ | T], Max) -> max2(T, Max).

%% find the minimum of a list
min([H | T]) -> min2(T, H).

min2([], Min) -> Min;
min2([H | _], Min) when H < Min -> H;
min2([_ | T], Min) -> min2(T, Min).

%% sum of all elements of a list
sum(L) -> sum(L, 0).

sum([], Sum) -> Sum;
sum([H | T], Sum) -> sum(T, H + Sum).

% can be abstracted to:
% Usage: hhfuns:fold(fun(A,B) when A > B -> A; (_,B) -> B end, H, T).
% Usage: hhfuns:fold(fun(A,B) when A < B -> A; (_,B) -> B end, H, T).
% Usage: hhfuns:fold(fun(A,B) -> A + B end, 0, lists:seq(1,6)).
fold(_, Start, []) -> Start;
fold(F, Start, [H | T]) -> fold(F, F(H, Start), T).

reduce(F, [H | T]) -> fold(F, H, T).
% Use lists:fold/3 or lists:foldr/3 instead.
