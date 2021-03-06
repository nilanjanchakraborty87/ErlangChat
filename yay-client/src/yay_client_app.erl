%%%-------------------------------------------------------------------
%% @doc yay-client public API
%% @end
%%%-------------------------------------------------------------------

-module(yay_client_app).

-behaviour(gen_server).

%% Application callbacks
-export([start/2, stop/1, init/1]).

-record(registration, {name, mobile, email, password}).
-record(login, {mobile, password}).
-record(chat_detail, {from, to, message}).
-record(request, {type, data}).
-record(response, {success, type, message, data}).
-record(client, {name, mobile, email, password, lastLogin, friends}).

-json({registration, {string, "name"}, {number, "mobile"}, {string, "emailId"}, {string, "password"}}).
-json({login, {number, "mobile"}, {string, "password"}}).
-json({chat_detail, {number, "from"}, {number, "to"}, {string, "message"}}).
-json({request, {string, "type"}, {record, "data"}}).
-json({response, {string, "isSuccess"}, {string, "type"}, {string, "message"}, {record, "data"}}).

-record(friend, {fname, mobile, status}).
-record(state, {registered = false, loggedIn = false, user, socket, online_friends}).

-define(TCP_OPTIONS, [binary, {packet, 4}, {active, false}, {reuseaddr, true}]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	process_flag(trap_exit, true),
	lager:start(),
	lager:info("Yay-client is starting"),
	{ok, Sock} = gen_tcp:connect("localhost", 9755, ?TCP_OPTIONS),
	lager:info("Client [~w] connected successfully", [Sock]),
	State = #state{socket=Sock},
	spawn_link(?MODULE, init, [State]),
  timer:sleep(infinity).

init(State) ->
	register_client(State).

number_prompt(Message) ->
  case io:fread("YayChat> " ++ Message ++ ": ", "~10d") of
    {ok, Terms} -> hd(Terms);
    _ -> lager:error("Error getting the number"),
      number_prompt(Message)
  end.

prompt(Message) ->
  case io:get_line("YayChat> " ++ Message ++ ": ") of
    Data -> string:trim(Data);
    eof -> "";
    {error, ErrorDescription} ->
      lager:error("Error getting the answer. Reason : [~p]", [ErrorDescription]),
      prompt(Message)
  end.

send_chat(State) ->
  case State#state.loggedIn of
    true -> io:format("YayChat>> Chat Details >>"),
      mutex:wait(),
      Message = prompt("Message"),
      To = number_prompt("Recipient"),
      Chat = #chat_detail{from = (State#state.user)#client.mobile, to = To, message = Message},
      Request = #request{type = "chat", data = Chat},
      {ok, ChatJsonReq} = to_json(Request),
      gen_tcp:send(State#state.socket, ChatJsonReq),
      io:format("YayChat>> Message Sent. Please send again~n"),
      mutex:signal(),
      send_chat(State);
    false -> io:format("YayChat>> Please login first to send chat~n"),
      login_client(State)
  end.


login_client(State) ->
  io:format("YayChat>> Login Details >>~n"),
  Mobile = number_prompt("Mobile"),
  Password = prompt("Password"),
%%  if Mobile =/= State#state.user#client.mobile or
%%      Password =/= State#state.user#client.password ->
%%   lager:info("Your details doesn't match. Please retry"),
%%      login_client(State);
%%    false -> void
%%  end,

  LoginDetail = #login{mobile = Mobile,
       password = Password},

  %lager:info("Login Details [~p]", [LoginDetail]),
  TcpLoginReq = #request{type="login", data=LoginDetail},
  %lager:info("Login TCP Request in record format ~p", [TcpLoginReq]),

  {ok, JSONLoginReq} = to_json(TcpLoginReq),
  %lager:info("Login request in json format : ~s", JSONLoginReq),
  gen_tcp:send(State#state.socket, JSONLoginReq),
  case gen_tcp:recv(State#state.socket, 0) of
    {ok, LoginResponseStr} ->
      %lager:debug("Login response: [~p]", [LoginResponseStr]),
      {ok, JsonLoginResponse} = from_json(LoginResponseStr, response),
       case JsonLoginResponse#response.success of
         "true" -> io:format("YayChat>> Login Successful~n"),
                    io:format("YayChat>> Hey, now you can start chatting with your friends~n"),
                    mutex:start(),
                    yaychat_receiver:start(State#state.socket),
                   send_chat(#state{socket = State#state.socket, user = State#state.user, registered = true, loggedIn = true});
         "false" ->
                   io:format("Login failed. Reason: [~p~n]", JsonLoginResponse#response.message),
                   login_client(State)
       end;
      _ -> lager:error("Error receiving login response. Retrying.."),
      login_client(State)
  end.

register_client(State) ->
  io:format("YayChat>> Registration Details >>~n"),
  Input = prompt("Do you like to register (Y/N)? "),
	case string:uppercase(Input) of
		"Y" ->
      RegDetail = #registration{name = prompt("Name"),
				mobile = number_prompt("Mobile"),
				email = prompt("Email"),
				password = prompt("Password")},

      %lager:info("RegDetails [~p]", [RegDetail]),
      TcpReq = #request{type="registration", data=RegDetail},
      %lager:info("Request in record format ~p", [TcpReq]),

      {ok, JSONReq} = to_json(TcpReq),
			%lager:info("Request details : ~s", JSONReq),
			case gen_tcp:send(State#state.socket, JSONReq) of
        ok -> case gen_tcp:recv(State#state.socket, 0) of
                {ok, ResponseStr} ->
                  %lager:debug("Registration response: [~p]", [ResponseStr]),
                  {ok, JsonRegResponse} = from_json(ResponseStr, response),
                  %lager:info("Status => [~p]", [JsonRegResponse#response.success]),
                  case JsonRegResponse#response.success of
                    "true" ->
                      io:format("Registration successful. Please login to continue chat~n"),
                      login_client(#state{
                        socket = State#state.socket,
                        registered = true,
                        loggedIn = false,
                        online_friends = [],
                        user = #client{
                          name = RegDetail#registration.name,
                          mobile = RegDetail#registration.mobile,
                          email = RegDetail#registration.email,
                          password = RegDetail#registration.password
                        }
                      });
                    "false" ->
                      io:format("Registration Failed. Reason: [~p~n]", [JsonRegResponse#response.message]),
                      register_client(State);
                    _ ->
                      lager:error("Something weird. Please register again"),
                      register_client(State)
                  end;
                _ -> io:format("Error receiving registration response. Please register again~n"),
                  register_client(State)
              end;
        {error, Reason} ->
          lager:error("Error sending the registration details.. Reason[~p]. Retrying..", [Reason]),
          register_client(State)
      end;
		"N" -> io:format("Already registered. Please login now~n"),
      login_client(State);
    _ -> register_client(State)
	end.

	
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
