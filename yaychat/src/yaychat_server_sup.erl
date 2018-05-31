%%%-------------------------------------------------------------------
%%% @author nilanjanc
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. May 2018 3:50 PM
%%%-------------------------------------------------------------------
-module(yaychat_server_sup).
-author("nilanjanc").

-behaviour(supervisor).

%% API
-export([start_link/1, init/1]).

%% Supervisor callbacks
-export([init/1, start_server/0]).

-define(TCP_OPTIONS, [list, {packet, 4}, {active, false}, {reuseaddr, true}]).
-define(SERVER, yaychat_server).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
  lager:info("yaychat_server_sup: start_link()"),
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

start_server() ->
  supervisor:start_child(?MODULE, []).

invoke_advance_listeners() ->
  [start_server() || _ <- lists:seq(1, 20)].

%% An approach described in leanYouSomeErlang
init(Args) ->
  {port, ServerPort} = Args,
  {ok, ListenSocket} = gen_tcp:listen(ServerPort, ?TCP_OPTIONS),
  lager:info("yaychat_server_sup: listening on port ~w", [ServerPort]),
  spawn_link(fun invoke_advance_listeners/0),
  {ok, {{simple_one_for_one, 60, 3600},
    [{?SERVER,
      {?SERVER, start_link, [ListenSocket]}, % pass the socket!
      temporary, 5000, worker, [?SERVER]}
    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

