-module(uns_room).
-export([start/1]).
-export([publish/3, join/2, leave/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name, clients = []}).

publish(Pid, Ref, Msg) ->
    gen_server:call(Pid, {send, Ref, Msg}).

join(RoomPid, Pid) ->
    gen_server:call(RoomPid, {join, Pid}).

leave(RoomPid, Pid) ->
    gen_server:call(RoomPid, {leave, Pid}).

start(Name) ->
    gen_server:start(?MODULE, [Name], []).

init([Name]) ->
    log(Name, "start"),
    {ok, #state{name = Name}}.

handle_call({send, Ref, Msg}, _From, #state{clients = Clients} = State) ->
    lists:foreach(fun(Pid) -> Pid ! {send, Ref, Msg} end, Clients),
    {reply, ok, State};

handle_call({join, Pid}, _From, #state{clients = Clients} = State) ->
    %log(State#state.name, "join"),
    {reply, ok, State#state{clients = [Pid | Clients]}};

handle_call({leave, Pid}, _From, #state{clients = Clients} = State) ->
    %log(State#state.name, "leave"),
    case lists:delete(Pid, Clients) of
        [] ->
            {stop, normal, ok, State#state{clients = []}};
        NewClients ->
            {reply, ok, State#state{clients = NewClients}}
    end.

handle_cast(_, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) -> uns_server:room_close(State#state.name), ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% =============================== Helpers ================================= %%

log(Name, Text) ->
    uns_log:log_print("room " ++ Name, Text).
