-module(uns_log).
% API:
-export([log/1, log_print/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(LOG_FILENAME, "log").

% API

log(Text) ->
    gen_server:call(?SERVER, {log, Text}).

log_print(Info, Text) ->
    io:format(user, "~p:~p:~p ~n ", [self(), Info, Text]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, LogDir}  = application:get_env(uns_chat, log_dir),
    LogFile       = filename:join(LogDir, ?LOG_FILENAME ++ ".log"),
    LogOptions    = [{name, tposs_charging_cdr},
                     {file, LogFile},
                     {format, external},
                     {type, halt}],
    {ok, DiskLog} = disk_log:open(LogOptions),
    {ok, DiskLog}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({log, Message}, DiskLog) ->
    disk_log:blog(DiskLog, Message),
    {noreply, DiskLog};

handle_info(_Info, DiskLog) ->
    {noreply, DiskLog}.

terminate(_Reason, DiskLog) ->
    disk_log:close(DiskLog).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
