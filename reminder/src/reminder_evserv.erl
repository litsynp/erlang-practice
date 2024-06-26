-module(reminder_evserv).
-compile(export_all).

-record(state, {
    %% list of #event{} records
    events,
    %% list of Pids
    clients
}).

-record(event, {
    name = "",
    description = "",
    pid,
    timeout = {{1970, 1, 1}, {0, 0, 0}}
}).

loop(S = #state{}) ->
    receive
        {Pid, MsgRef, {subscribe, Client}} ->
            Ref = erlang:monitor(process, Client),
            NewClients = orddict:store(Ref, Client, S#state.clients),
            Pid ! {MsgRef, ok},
            loop(S#state{clients = NewClients});
        {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
            case valid_datetime(TimeOut) of
                true ->
                    EventPid = event:start_link(Name, TimeOut),
                    NewEvents = orddict:store(
                        Name,
                        #event{
                            name = Name,
                            description = Description,
                            pid = EventPid,
                            timeout = TimeOut
                        },
                        S#state.events
                    ),
                    Pid ! {MsgRef, ok},
                    loop(S#state{events = NewEvents});
                false ->
                    Pid ! {MsgRef, {error, bad_timeout}},
                    loop(S)
            end;
        {Pid, MsgRef, {cancel, Name}} ->
            Events =
                case orddict:find(Name, S#state.events) of
                    {ok, E} ->
                        event:cancel(E#event.pid),
                        orddict:erase(Name, S#state.events);
                    error ->
                        S#state.events
                end,
            Pid ! {MsgRef, ok},
            loop(S#state{events = Events});
        {done, Name} ->
            case orddict:find(Name, S#state.events) of
                {ok, E} ->
                    send_to_clients({done, E#event.name, E#event.description}, S#state.clients),
                    NewEvents = orddict:erase(Name, S#state.events),
                    loop(S#state{events = NewEvents});
                error ->
                    %% This may happen if we cancel an event and
                    %% it fires at the same time
                    loop(S)
            end;
        shutdown ->
            % If you want to save state to disk, this could be the possible place to do it.
            exit(shutdown);
        {'DOWN', Ref, process, _Pid, _Reason} ->
            % Client died, remove it from the list
            loop(S#state{clients = orddict:erase(Ref, S#state.clients)});
        code_change ->
            % doing very basic reloading; from this point on, the 'new' code(external calls) becomes default
            ?MODULE:loop(S);
        Unknown ->
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(S)
    end.

init() ->
    %% Loading events from a static file could be done here.
    %% You would need to pass an argument to init telling where the
    %% resource to find the events is. Then load it from here.
    %% Another option is to just pass the events straight to the server
    %% through this function.
    loop(#state{events = orddict:new(), clients = orddict:new()}).

valid_datetime({Date, Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        %% not in {{Y,M,D},{H,Min,S}} format
        error:function_clause ->
            false
    end;
valid_datetime(_) ->
    false.

valid_time({H, M, S}) -> valid_time(H, M, S).
valid_time(H, M, S) when
    H >= 0,
    H < 24,
    M >= 0,
    M < 60,
    S >= 0,
    S < 60
->
    true;
valid_time(_, _, _) ->
    false.

send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

start() ->
    register(?MODULE, Pid = spawn(?MODULE, init, [])),
    Pid.

start_link() ->
    register(?MODULE, Pid = spawn_link(?MODULE, init, [])),
    Pid.

terminate() ->
    ?MODULE ! shutdown.

subscribe(Pid) ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    ?MODULE ! {self(), Ref, {subscribe, Pid}},
    receive
        {Ref, ok} ->
            {ok, Ref};
        {'DOWN', Ref, process, _Pid, Reason} ->
            {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

% Forwards the message that could be received to the client
add_event(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

% Crashes the client instead of forwarding the error
add_event2(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, {error, Reason}} -> erlang:error(Reason);
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

cancel(Name) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {cancel, Name}},
    receive
        {Ref, ok} -> ok
    after 5000 ->
        {error, timeout}
    end.

listen(Delay) ->
    receive
        M = {done, _Name, _Description} ->
            [M | listen(0)]
    after Delay * 1000 ->
        []
    end.
