-module(index_handler).
-behavior(cowboy_rest).

%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

%% Callback Callbacks
-export([hello_from_json/2]).

init(Req, State) ->
  {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[
      {<<"application/json">>, hello_from_json}
  ], Req, State}.

hello_from_json(Req, State) ->
  case middleware:auth(Req) of
      {true, User, Req1} ->
          Fname = maps:get(fname, User),
          Lname = maps:get(lname, User),
          Message = [hello,  <<"Welcome ", Fname/binary, " ", Lname/binary, " to Yaychat">>];
      {false, Req1} ->
          Message = [hello, <<"Welcome to Yaychat">>]
  end,
  {jiffy:encode(Message), Req1, State}.
