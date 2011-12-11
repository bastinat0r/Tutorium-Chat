-module(uns_connection).
-export([start/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
% Finite state machine
-export([unauthorised/2, authorised/2]).
-record(state, {socket, fsm = unauthorised, state}).
-record(user, {nick = undefined, room = "default", roompid, references = []}).

start(ListenSocket) ->
    gen_server:start(?MODULE, [ListenSocket], []).

init([ListenSocket]) ->
    log("starting listenning"),
    self() ! start_accepting,
    {ok, #state{socket = ListenSocket}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({log, _Message}, State) ->
    {noreply, State};

handle_info(start_accepting, #state{socket = Socket}= State) ->
    {ok, NewWorkingSocket} = gen_tcp:accept(Socket),
    uns_server:started(self()),
    {noreply, State#state{socket = NewWorkingSocket, state = #user{}}};

handle_info({tcp, _, Packet}, #state{fsm = Fsm, state = ClientState} = State) ->
    Message = binary_to_list(Packet),
    %log(Message),
    {NewFsm, NewClientState} = uns_connection:Fsm(Message, ClientState),
    {noreply, State#state{fsm = NewFsm, state = NewClientState}};

handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};

handle_info({send, Ref, Message}, #state{socket = Socket, state = ClientState} = State) ->
    SendFun = fun(MessageToSend) -> gen_tcp:send(Socket, list_to_binary(MessageToSend)) end,
    NewClientState = send(Ref, Message, ClientState, SendFun),
    {noreply, State#state{state = NewClientState}}.

terminate(_Reason, _State) ->
    log("connection terminate"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================== Helpers ================================= %%

log(Text) ->
    uns_log:log_print("connection", Text).

-include_lib("kernel/include/inet.hrl").
get_client_name(Socket) ->
    case inet:peername(Socket) of
        {error, _Reason} ->
            "?.?.?.?:?";
        {ok, {{A,B,C,D}, Port}} ->
            case inet:gethostbyaddr({A,B,C,D}) of
                {error, _Reason} ->
                    io_lib:fwrite("~B.~B.~B.~B:~B", [A,B,C,D, Port]);
                {ok, Hostent} ->
                    io_lib:fwrite("~s:~B", [Hostent#hostent.h_name, Port])
            end
    end.

%% ================================ FSM ==================================== %%
%-record(user, {nick = undefined, room = "default", roompid, references}).

unauthorised(Message, State) ->
    log("unauthorised" ++ Message),
    List = string:tokens(Message, "/ \n"),
    case tryauthorised(List) of
         {true, Name} ->
             Pid = uns_server:get_room_pid(State#user.room),
             uns_room:join(Pid, self()),
             {authorised, State#user{nick = Name, roompid = Pid}};
         false ->
             {unauthorised, State}
     end.

authorised(Message, State) ->
    log("authorised" ++ Message),
    case checkcmd(Message) of
        true ->
            NewState = parcecmd(Message, State),
            {authorised, NewState};
        false ->
            Reference = make_ref(),
            uns_room:publish(State#user.roompid, Reference, State#user.nick ++ ":" ++ Message),
            {authorised, State#user{references = [Reference | State#user.references]}}
    end.

send(Reference, Message, #user{references = References} = State, SendFun) ->
    case lists:member(Reference, References) of 
        true ->
            State#user{references = lists:delete(Reference, References)};
        false ->
            SendFun(Message),
            State
    end.

%% =============================== Helpers ================================= %%

checkcmd([47 | _Rest]) -> true;
checkcmd([_ | _Rest]) -> false.

tryauthorised(["nick", Name | _]) -> {true, Name};
tryauthorised(_) -> false.

parcecmd(Message, State) ->
    case string:tokens(Message, "/ \n") of
        ["nick", Name |_] ->
            State#user{nick = Name};
        ["room", Name |_] ->
            changeroom(State, Name)
    end.

changeroom(#user{roompid = OldRoomPid} = State, NewRoom) ->
    uns_room:leave(OldRoomPid, self()),
    NewRoomPid = uns_server:get_room_pid(NewRoom),
    uns_room:join(NewRoomPid, self()),
    State#user{room = NewRoom, roompid = NewRoomPid}.
