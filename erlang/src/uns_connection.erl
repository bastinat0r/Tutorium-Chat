-module(uns_connection).
-export([start/1]).
-export([get_nick/1, send_message/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
% Finite state machine
-export([unauthorised/2, authorised/2]).
-record(state, {socket, fsm = unauthorised, state}).
-record(user, {socket, nick = undefined, room = "default", roompid, references = []}).

start(ListenSocket) ->
    gen_server:start(?MODULE, [ListenSocket], []).

get_nick(Pid) ->
    gen_server:call(Pid, get_nick).

send_message(Pid, Message) ->
    gen_server:cast(Pid, {send, Message}).


init([ListenSocket]) ->
    log("starting listenning"),
    self() ! start_accepting,
    {ok, #state{socket = ListenSocket}}.

handle_call(get_nick, _From, #state{state = ClientState} = State) ->
    {reply, get_nick_name(ClientState), State}.

handle_cast({send, {Ref, Message}}, #state{state = ClientState} = State) ->
    NewClientState = send(Ref, Message, ClientState),
    {noreply, State#state{state = NewClientState}}.

handle_info(start_accepting, #state{socket = Socket}= State) ->
    {ok, NewWorkingSocket} = gen_tcp:accept(Socket),
    uns_server:started(self()),
    {noreply, State#state{state = inizialize(NewWorkingSocket)}};

handle_info({tcp, _, Packet}, #state{fsm = Fsm, state = ClientState} = State) ->
    Message = binary_to_list(Packet),
    %log(Message),
    {NewFsm, NewClientState} = uns_connection:Fsm(Message, ClientState),
    {noreply, State#state{fsm = NewFsm, state = NewClientState}};

handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    close_connection(State#state.state),
    log("closed"),
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
inizialize(Socket) ->
    #user{socket = Socket}.

get_nick_name(#user{nick = Nick}) ->
    Nick.

unauthorised(Message, State) ->
    List = string:tokens(Message, "/ \n"),
    case tryauthorised(List) of
         {true, Name} ->
             Pid = uns_server:get_room_pid(State#user.room),
             uns_room:join(Pid, self(), Name),
             {authorised, State#user{nick = Name, roompid = Pid}};
         false ->
             {unauthorised, State}
     end.

authorised(Message, State) ->
    case checkcmd(Message) of
        true ->
            NewState = parcecmd(Message, State),
            {authorised, NewState};
        false ->
            Reference = make_ref(),
            uns_room:publish(State#user.roompid, Reference, State#user.nick ++ ":" ++ Message),
            {authorised, State#user{references = [Reference | State#user.references]}}
    end.

send(Reference, Message, #user{socket = Socket, references = References} = State) ->
    case lists:member(Reference, References) of
        true ->
            State#user{references = lists:delete(Reference, References)};
        false ->
            gen_tcp:send(Socket, list_to_binary(Message)),
            State
    end.

close_connection(#user{roompid = Pid} = State) ->
    uns_room:leave(Pid, self()).

%% =============================== Helpers ================================= %%

checkcmd([47 | _Rest]) -> true;
checkcmd([_ | _Rest]) -> false.

tryauthorised(["nick", Name | _]) -> {true, Name};
tryauthorised(_) -> false.

parcecmd(Message, State) -> cmd(string:tokens(Message, "/ \n"), State).


cmd(["nick", Name |_], State) -> State#user{nick = Name};
cmd(["room", Name |_], State) -> changeroom(State, Name);
cmd(["list", "rooms" |_], State) -> listrooms(State);
cmd(["list", "users" |_], State) -> listusers(State);
cmd(_, State) -> State.

changeroom(#user{nick = Nick, roompid = OldRoomPid} = State, NewRoom) ->
    uns_room:leave(OldRoomPid, self()),
    NewRoomPid = uns_server:get_room_pid(NewRoom),
    uns_room:join(NewRoomPid, self(), Nick),
    State#user{room = NewRoom, roompid = NewRoomPid}.

listrooms(#user{socket = Socket} = State) ->
    Objects = uns_server:get_all_rooms(),
    gen_tcp:send(Socket, "Rooms:\n"),
    [gen_tcp:send(Socket, Room++"\n") || {Room, _} <- Objects],
    State.

listusers(#user{socket = Socket, room = Room, roompid = Pid} = State) ->
    Names = uns_room:get_all_clients(Pid),
    gen_tcp:send(Socket, Room++":\n"),
    [gen_tcp:send(Socket, Name++"\n") || Name <- Names],
    State.
