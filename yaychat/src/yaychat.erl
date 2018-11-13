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
                {"/yaychat", index_handler, []},
                {"/yaychat/user/register", user_registration_handler, []}
            ]}
    ]),
    {ok, _} = cowboy:start_clear(yaychat_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch},
        middlewares => [cowboy_router, session_cowboy_middleware, cowboy_handler]
    }),
    case yaychat_sup:start_link(_StartArgs) of
      {ok, Pid} ->
             {ok, Pid};
          Other ->
              {error, Other}
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
