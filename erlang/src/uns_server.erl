-module(uns_server).
-export([start_link/0]).
-export([started/1, get_room_pid/1, room_close/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {listen_socket}).
-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).
-define(MS_BATCH_LIMIT, 200).

started(Pid) -> gen_server:call(?SERVER, {started, Pid}).

get_room_pid(RoomName) -> gen_server:call(?SERVER, {room, RoomName}).

room_close(RoomName) -> gen_server:call(?SERVER, {room_close, RoomName}).

start_link() ->
    log("starting"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	ets:new(?TABLE, [protected, bag, named_table]),
    process_flag(trap_exit, true),
    DefaultOpts = [binary, {packet, line}, {active, true}, {reuseaddr, true}],
    case gen_tcp:listen(5555, DefaultOpts) of
        {ok, ListenSocket} ->
            {ok, Pid} = uns_connection:start(ListenSocket),
            link(Pid),
            log("started"),
            {ok, #state{listen_socket = ListenSocket}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({started, Pid}, _From, #state{listen_socket = ListenSocket} = State) ->
    unlink(Pid),
    uns_connection:start(ListenSocket),
    {reply, ok, State};

handle_call({room, Name}, _From, State) ->
    case ets:lookup(?TABLE, Name) of
        [] ->
            {ok, RoomPid} = uns_room:start(Name),
            ets:insert(?TABLE, {Name, RoomPid});
        [{Name, RoomPid}] ->
            ok
    end,
    {reply, RoomPid, State};

handle_call({room_close, Name}, _From, State) ->
    ets:delete(?TABLE, Name),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _From, _Reason}, #state{listen_socket = ListenSocket} = State) ->
    uns_connection:start(ListenSocket),
    log("connection died"),
    {noreply, State}.

terminate(_Reason, _State) ->
    log("server_terminating"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================== Helpers ================================= %%

log(Text) ->
    uns_log:log_print("server", Text).
