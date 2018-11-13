-module(user_registration_handler).
-include("yaychat.hrl").
-behavior(cowboy_rest).


-import(helper, [get_body/2, get_model/3, reply/3, pwd2hash/1]).


%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).

%% Callback Callbacks
-export([register/2]).


init(Req, State) ->
  {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[
  {<<"application/json">>, register}
  ], Req, State }.

register(Req, State) ->
  {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req),
  %extract body and validate
  case get_body(Body, Req1) of
        {ok, Input, _Req} ->
            %% Validate body json and fields
            Model = [
                {<<"email">>, required, string, #dbuser.user_email, [non_empty,
                    fun(V) ->
                        validator:email(V)
                    end
                ]},
                {<<"password">>, required, string, #dbuser.password, [non_empty,
                    fun(V) ->
                        validator:min_length(6, V)
                    end
                ]},
                {<<"mobile">>, required, integer, #dbuser.user_mobile, [non_empty,
                    fun(V) ->
                        validator:min_length(10, V)
                    end
                ]},
                {<<"fname">>, required, string, #dbuser.user_fname, [non_empty]},
                {<<"lname">>, required, string, #dbuser.user_lname, [non_empty]}
            ],
            Emodel = get_model(Input, Model, Req1),

            %% Check model result
            case Emodel of
               {error, Reason} ->
                   Req3 = reply(412, {Reason}, Req1),
                   {false, Req3, State};
               {error, empty, Req4} ->
                   {false, Req4, State};
               {ok, _} ->

                   %% Perform Registration
                   case persist_user(Emodel, Req1) of
                       {ok, User, Req5} ->
                           {true, reply(200, User, Req5), State};
                       {error, Req6} ->
                           {false, Req6, State}
                   end

           end;

       {error, empty, Req2} ->
           {false, Req2, State}

   end.

 %% Registration functions
persist_user(Emodel, Req) ->
    %% Auth middleware
    {ok, Data} = Emodel,
    case yaychat_db:check_user(Data#dbuser.user_mobile) of
        false ->
            %Pass = maps:get(pass, Data),
            %Register_safe_pass = maps:update(pass, pwd2hash(Pass), Data),
            %Register = maps:put(token, random(64), Register_safe_pass),
            %{ok, Req2} = cowboy_session:set(<<"register">>, Register, Req1),
            {ok, #{}, Req};
        _ ->
            {error, reply(400, <<"User already exists">>, Req)}
    end.
