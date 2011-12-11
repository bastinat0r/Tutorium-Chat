-module(uns_chat_app).

-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    application:start(uns_chat).

% @private
start(_Type, _Args) ->
    uns_log:log_print("application", "start"),
    create_dir(),
    uns_log:log_print("supervisor", "start"),
    {ok, Sup} = uns_sup:start_link([]),
    uns_log:log_print("supervisor", "started"),
    {ok, Sup}.

create_dir() ->
    {ok, LogDir} = application:get_env(uns_chat, log_dir),
    ok           = filelib:ensure_dir(filename:join(LogDir, ".")).
 
% @private
stop(_State) ->
    ok.
