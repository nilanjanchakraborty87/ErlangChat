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
    shell:strings(false),
    lager:start(),
    lager:info("----------------------------------------------"),
    lager:info("Running yayChat Application"),
    lager:info("----------------------------------------------"),
    lager:info("Fastest, Simple & Reliable way to share"),
    lager:info("your moments with your buddies!"),
    lager:info("                                              "),
    lager:info("Copyright (C) 2018-2020 NilanjanC."),
    lager:info("All rights reserved."),
    inets:start(),
    yaychat_sup:start_link(_StartArgs).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
