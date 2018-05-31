%%%-------------------------------------------------------------------
%% @doc yaychat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(yaychat_sup).
-include("yaychat.hrl").
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    lager:info("yaychat_sup:start_link()"),
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Args) ->
    process_flag(trap_exit, true),
    lager:info("yaychat_sup: initializing, pid=~w", [self()]),
    RestartStrategy = one_for_one,
    MaxRetries = 5,
    DurationBetweenRestarts = 60,

    Restart = permanent,
    Shutdown = infinity,

    %Server supervisor definition
    YayChatServerSupSpec = {serverSup, {yaychat_server_sup, start_link, Args},
        Restart, Shutdown, supervisor, [yaychat_server_sup]},

    YayChatRouterSpec = {?ROUTER, {?ROUTER, start_link, []},
        permanent, 10, worker, [?ROUTER]},

    %%YayChatStorage = {yayChatStorage, {yaychat_storage, start_link, []},
                         %%  permanent, 10, worker, [yaychat_storage]},

    ChildSpecs = [YayChatServerSupSpec, YayChatRouterSpec],
    ok = supervisor:check_childspecs(ChildSpecs),
    SupSpecs = {{RestartStrategy, MaxRetries, DurationBetweenRestarts}, ChildSpecs},
    lager:info("mychat_sup:init() returning"),
    {ok, SupSpecs}.

%%====================================================================
%% Internal functions
%%====================================================================
