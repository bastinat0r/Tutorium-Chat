-module(uns_room).
-export([start/1]).
-export([publish/3, join/3, leave/2, get_all_clients/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name, clients = []}).

publish(Pid, Ref, Msg) ->
    gen_server:call(Pid, {send, Ref, Msg}).

join(RoomPid, Pid, Nick) ->
    gen_server:call(RoomPid, {join, Pid, Nick}).

leave(RoomPid, Pid) ->
    gen_server:call(RoomPid, {leave, Pid}).

get_all_clients(RoomPid) ->
    Clients = gen_server:call(RoomPid, get_all_clients),
    [Name || {_, Name} <- Clients].

start(Name) ->
    gen_server:start(?MODULE, [Name], []).

init([Name]) ->
    log(Name, "start"),
    {ok, #state{name = Name}}.

handle_call({send, Ref, Msg}, _From, #state{clients = Clients} = State) ->
    lists:foreach(fun({Pid, _}) -> uns_connection:send_message(Pid, {Ref, Msg}) end, Clients),
    {reply, ok, State};

handle_call({join, Pid, Name}, _From, #state{clients = Clients} = State) ->
    {reply, ok, State#state{clients = [{Pid, Name} | Clients]}};

handle_call({leave, Pid}, _From, #state{clients = Clients} = State) ->
    %log(State#state.name, "leave"),
    case proplists:delete(Pid, Clients) of
        [] ->
            {stop, normal, ok, State#state{clients = []}};
        NewClients ->
            {reply, ok, State#state{clients = NewClients}}
    end;

handle_call(get_all_clients, _From, #state{clients = Clients} = State) ->
    {reply, Clients, State}.

handle_cast(_, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) -> uns_server:room_close(State#state.name), ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% =============================== Helpers ================================= %%

log(Name, Text) ->
    uns_log:log_print("room " ++ Name, Text).
