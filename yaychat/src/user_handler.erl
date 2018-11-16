-module(user_handler).
-include("yaychat.hrl").
-behavior(cowboy_rest).


-import(helper, [get_body/2, get_model/4, reply/3, pwd2hash/1, create_emodel_error_message/1]).


%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

-json({login, {number, "mobile"}, {string, "password"}}).
-json({request, {string, "type"}, {record, "data"}}).
-json({response, {string, "isSuccess"}, {string, "type"}, {string, "message"}, {record, "data"}}).

%% Callback Callbacks
-export([register/2, login/3]).

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
          lager:info("Registration request: [~p]", [Input]),
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
                        validator:min_length(10, integer_to_binary(V))
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
                       {ok, UserId, Req5} ->
                         Response = #response{success = "true", type = "registration", message = "Registration Successful"},
                         {ok, ResponseJson} = to_json(Response),
                         Req6 = cowboy_req:set_resp_body(ResponseJson, Req5),
                           {true, Req6, State};
                       {error, Reason, Req7} ->
                         Response = #response{success = "false", type = "registration", message = Reason},
                         {ok, ResponseJson} = to_json(Response),
                         Req8 = cowboy_req:set_resp_body(ResponseJson, Req7),
                           {false, Req8, State}
                   end

           end;

       {error, empty, Req2} ->
           {false, Req2, State}

   end.

 %% Registration functions
persist_user(Emodel, Req) ->
    %% Auth middleware
    {ok, Data} = Emodel,
    case yaychat_db:check_user(Data#user.user_mobile) of
        false ->
            %hash password
            NewData = Data#user{password = pwd2hash(Data#user.password)},
            lager:info("Encrypted password -> ~p", [NewData#user.password]),
            case yaychat_db:save_user(NewData) of
              undefined -> {error, <<"Internal error">>, Req};
             UserId -> {ok, UserId, Req}
           end;
        _ ->
            {error, "The Username is taken. Try another", Req}
    end.

login(Req, Socket, C2SPid) ->
  LoginDetail = Req#request.data,
  lager:info("Login request ~p", [LoginDetail]),
  lager:info("C2SPID ~p", [C2SPid]),
  case yaychat_db:find_user_by_username(LoginDetail#login.mobile) of
    User when User /= undefined ->
      %match password
      HashPassword = pwd2hash(LoginDetail#login.password),
      case string:equal(HashPassword, User#user.password) of
        true ->
                lager:info("login successful for Username: ~p", [User#user.username]),
                Ret = syn:register(binary_to_list(User#user.username), C2SPid),
                lager:info("Syn return: ~p", [Ret]),
                %create the login response in correspondence to the legacy code
                LoginTime = qdate:to_string("YmdHi", calendar:local_time()),
                Client = #client{name = binary_to_list(User#user.user_fname) ++ " " ++ binary_to_list(User#user.user_lname),
                                        email = binary_to_list(User#user.user_email), mobile = User#user.user_mobile,
                                        lastLogin = LoginTime},

                #response{success = "true", type = "login", message = "Login Successful", data = Client};
        false ->
                lager:info("Password don't match"),
                #response{success = "false", type = "login", message = "Password don't match"}
      end;
    record_not_found ->
      #response{success = "false",type = "login",  message = "User not found"};
    error ->
      #response{success = "false",type = "login",  message = "Sorry! something went wrong. Please retry"}
   end.
