-module(uns_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link([]) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    %LogWorker = ?CHILD(uns_log, [], worker),
    ServerWorker = ?CHILD(uns_server, [], worker),
    {ok, {{one_for_one, 10, 20}, [ServerWorker]}}.
