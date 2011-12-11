-module(uns_server).
-export([start_link/0]).
-export([start_server/1]).
-export([started/1, get_room_pid/1, room_close/1, get_all_rooms/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {listen_socket}).
-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

start_server(Port) -> gen_server:call(?SERVER, {start, Port}).

started(Pid) -> gen_server:call(?SERVER, {started, Pid}).

get_room_pid(RoomName) -> gen_server:call(?SERVER, {room, RoomName}).

room_close(RoomName) -> gen_server:call(?SERVER, {room_close, RoomName}).

get_all_rooms() -> gen_server:call(?SERVER, room_all).

start_link() ->
    log("starting"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	ets:new(?TABLE, [protected, bag, named_table]),
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({start, Port}, _From, State) ->
    DefaultOpts = [binary, {packet, line}, {active, true}, {reuseaddr, true}],
    case gen_tcp:listen(Port, DefaultOpts) of
        {ok, ListenSocket} ->
            {ok, Pid} = uns_connection:start(ListenSocket),
            link(Pid),
            log("started"),
            {reply, ok, State#state{listen_socket = ListenSocket}};
        {error, Reason} ->
            {stop, normal, error, Reason}
    end;


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
    {reply, ok, State};

handle_call(room_all, _From, State) ->
    Objects = ets:tab2list(?TABLE),
    {reply, Objects, State}.

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
