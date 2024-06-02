-module(multiproc).
-compile(export_all).

sleep(T) ->
    receive
    after T -> ok
    end.

flush() ->
    receive
        _ -> flush()
    after 0 ->
        ok
    end.

% selective receives are useful, but ignored messages will pile up.
% Eventually reading useful messages(by pattern matching from the first item)
% from mailbox will take longer and longer.
important() ->
    receive
        % Run this until no pattern matches
        {Priority, Message} when Priority > 10 ->
            [Message | important()]
    after 0 ->
        % If empty
        normal()
    end.

normal() ->
    receive
        {_, Message} ->
            [Message | normal()]
    after 0 ->
        []
    end.

% Optimized in R14A
optimized(Pid) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, hello},
    receive
        {Pid, Ref, Msg} ->
            io:format("~p~n", [Msg])
    end.
