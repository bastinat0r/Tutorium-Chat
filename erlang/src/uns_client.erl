-module(uns_client).
-behaviour(gen_server).

-export([start_link/1, start_link/2, start_link/3]).
-export([print/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket, shell}).

print(Msg) ->
    gen_server:call(?SERVER, {send, Msg}).

start_link(Pid) ->
    start_link(Pid, {127,0,0,1}, 5555).
start_link(Pid, Port) ->
    start_link(Pid, {127,0,0,1}, Port).
start_link(Pid, Host, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Pid, Host, Port], []).

init([Pid, Host, Port]) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, true}]),
    {ok, #state{socket = Socket, shell = Pid}}.

handle_call({send, Msg}, _From, #state{socket = Socket} = State) ->
    gen_tcp:send(Socket, list_to_binary(Msg)),
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    State#state.shell ! Info,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
