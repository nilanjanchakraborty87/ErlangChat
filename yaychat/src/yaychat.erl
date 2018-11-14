%%%-------------------------------------------------------------------
%% @doc yaychat public API
%% @end
%%%-------------------------------------------------------------------

-module(yaychat).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    lager:info("----------------------------------------------"),
    lager:info("Running yayChat Application"),
    lager:info("----------------------------------------------"),
    lager:info("Fastest, Simple & Reliable way to share"),
    lager:info("your moments with your buddies!"),
    lager:info("                                              "),
    lager:info("Copyright (C) 2018-2020 NilanjanC."),
    lager:info("All rights reserved."),
    lager:start(),
    %application:ensure_all_started(mysql_poolboy),
    %application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([
            {'_', [
                %{"/yaychat", index_handler, []},
                {"/yaychat", index_handler, []},
                {"/yaychat/user/register", user_handler, [register]}
            ]}
    ]),
    {ok, _} = cowboy:start_clear(yaychat_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch},
        middlewares => [cowboy_router, session_cowboy_middleware, cowboy_handler]
    }),

    case yaychat_sup:start_link(_StartArgs) of
      {ok, Pid} ->
        %% connect to nodes
        connect_nodes(),
        register_node(),
        %% init syn
        syn:init(),
        {ok, Pid};
      {error, Reason} ->
          {error, Reason}
    end.


connect_nodes() ->
    case yaychat_db:list_live_nodes() of
      [] -> ok;
      NodeList ->
        %% connect to nodes
  	    [net_kernel:connect_node(list_to_atom(binary_to_list(Node))) || Node <- NodeList];
      error -> connect_nodes()
    end.

register_node() ->
    case yaychat_db:register_node(node()) of
      ok -> void;
     error -> register_node()
   end.


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
