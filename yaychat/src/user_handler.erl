-module(user_handler).
-include("yaychat.hrl").
-behavior(cowboy_rest).


-import(helper, [get_body/2, get_model/4, reply/3, pwd2hash/1, create_emodel_error_message/1]).


%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-json({response, {string, "isSuccess"}, {string, "type"}, {string, "message"}, {record, "data"}}).

%% Callback Callbacks
-export([register/2]).

-record(state, {op}).

init(Req, Opts) ->
  [Op | _] = Opts,
  State = #state{op=Op},
  {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, publish}
   ], Req, State}.

content_types_accepted(Req, State) ->
  {[
  {<<"application/json">>, register}
  ], Req, State }.

resource_exists(Req, State) ->
    {true, Req, State}.

register(Req, State) ->
  {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req),
  %extract body and validate
  case get_body(Body, Req1) of
        {ok, Input, _Req} ->
          lager:info("Incoming registration request: [~p]", [Input]),
            %% Validate body json and fields
            Model = [
                {<<"email">>, required, string, #user.user_email, [non_empty,
                    fun(V) ->
                        validator:email(V)
                    end
                ]},
                {<<"password">>, required, string, #user.password, [non_empty,
                    fun(V) ->
                        validator:min_length(6, V)
                    end
                ]},
                {<<"mobile">>, required, integer, #user.user_mobile, [non_empty,
                    fun(V) ->
                        validator:min_length(10, integer_to_list(V))
                    end
                ]},
                {<<"fname">>, required, string, #user.user_fname, [non_empty]},
                {<<"lname">>, required, string, #user.user_lname, [non_empty]}
            ],
            Emodel = get_model(Input, Model, #user{}, _Req),

            %% Check model result
            case Emodel of
               {error, Reason} ->
                  % Req3 = reply(412, {Reason}, Req1),
                  Response = #response{success = "false", type = "registration", message = create_emodel_error_message(Reason)},
                  {ok, ResponseJson} = to_json(Response),
                  Req3 = cowboy_req:set_resp_body(ResponseJson, _Req),
                  {true, Req3, State};
                  %{Body, Req, State};
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
    lager:info("Register user details: [~p]", [Data]),
    case yaychat_db:check_user(Data#user.user_mobile) of
        false ->
            %Pass = maps:get(pass, Data),
            %Register_safe_pass = maps:update(pass, pwd2hash(Pass), Data),
            %Register = maps:put(token, random(64), Register_safe_pass),
            %{ok, Req2} = cowboy_session:set(<<"register">>, Register, Req1),
            {ok, #{}, Req};
        _ ->
            {error, reply(400, <<"User already exists">>, Req)}
    end.
