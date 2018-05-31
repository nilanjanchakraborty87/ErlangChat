%%%-------------------------------------------------------------------
%%% @author nilanjanc
%%% @copyright (C) 2018, <NilanjanC>
%%% @doc
%%%
%%% @end
%%% Created : 28. May 2018 3:57 PM
%%%-------------------------------------------------------------------
-module(yaychat_server).
-author("nilanjanc").
-behavior(gen_server).
-define(TCP_OPTIONS, [binary, {packet, 4}, {active, false}, {reuseaddr, true}]).

-record(state, {socket, registered = false, loggedin = false, mobile}).

-json({registration, {string, "name"}, {number, "mobile"}, {string, "emailId"}, {string, "password"}}).
-json({login, {number, "mobile"}, {string, "password"}}).
-json({chat_detail, {number, "from"}, {number, "to"}, {string, "message"}}).
-json({request, {string, "type"}, {record, "data"}}).
-json({response, {string, "isSuccess"}, {string, "message"}, {record, "data"}}).

%% API
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("yaychat.hrl").

start_link(ListenSocket) ->
  lager:info("Initializing yaychat_server to accept new connections"),
  gen_server:start_link(?MODULE, ListenSocket, []).

init(LSock) ->
  lager:info("yaychat_server: loop initializing, pid=~w", [self()]),
  gen_server:cast(self(), accept), %%starts the accept loop
  {ok, #state{socket=LSock}}.

handle_call(_E, _From, State) ->
  {noreply, State}.

handle_cast(accept, S = #state{socket=ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  lager:info("New Connection accepted. Details: [~w]", [inet:peername(AcceptSocket)]),
  refresh_socket(AcceptSocket),
  yaychat_server_sup:start_server(),
  {noreply, S#state{socket=AcceptSocket}};
handle_cast({private_message, From, Message}, State) ->
  lager:info("Received new message[~p] from[~p]", [Message, From]),
  Chat = #chat_detail{from = From, to = State#state.mobile, message = Message},
  Request = #request{type = "chat", data = Chat},
  {ok, ChatJsonReq} = to_json(Request),
  lager:info("Chat json request: [~p]", [ChatJsonReq]),
  gen_tcp:send(State#state.socket, ChatJsonReq),
  {noreply, State}.

handle_info({tcp, Socket, Req}, State) ->
  lager:info("Received request. Raw request: [ ~p]", [Req]),
  {ok, NewReq} = from_json(list_to_binary(Req), request),

  NewState = case NewReq#request.type of
               "registration" ->
                 handle_request(registration, NewReq, Socket, State);
               "login" ->
                 handle_request(login, NewReq, Socket, State);
               "chat" ->
                 handle_request(chat, NewReq, Socket, State);
               _ ->
                 lager:info("Unknown request")
  end,
  refresh_socket(Socket),
  {noreply, NewState};
handle_info({tcp_closed, Socket}, State) ->
  lager:info("Connection close request received from client [~w]", [Socket]),
  {stop, normal, State};
handle_info({tcp_error, Socket}, State) ->
  lager:info("Connection error in [~w]", [Socket]),
  {stop, normal, State};
handle_info(E, State) ->
  lager:info("Unexpected [~w]", E),
  {noreply, State}.

handle_request(registration, Req, Socket, State) ->
  lager:info("Registration Name ~p", [(Req#request.data)#registration.name]),
  RegRouterResponse = gen_server:call(?ROUTER, {new_registration, Req#request.data, Socket, self()}, infinity),
  {RegState, RegResponse} = case RegRouterResponse of
                              true -> lager:info("Registration Successful"),
                                {#state{registered = true, socket = State#state.socket},
                                  #response{success = "true", message = "Registration Successful"}};
                              false ->
                                {State,
                                  #response{success = "false", message = "Mobile no is already in use"}}
                            end,
  lager:info("Registration response details: [~p]", [RegResponse]),
  {ok, JsonRegResponse} = to_json(RegResponse),
  lager:info("Registration json response: [~p]", [JsonRegResponse]),
  gen_tcp:send(Socket, JsonRegResponse),
  RegState;
handle_request(login, Req, Socket, State) ->
  lager:info("Login mobile ~p", [(Req#request.data)#login.mobile]),
  LoginRouterResponse = gen_server:call(?ROUTER, {new_login, Req#request.data, Socket, self()}, infinity),
  {LoginState, LoginResponse} = case LoginRouterResponse of
                                  user_not_found ->
                                    {State,
                                      #response{success = "false", message = "User not found"}};
                                  password_dont_match ->
                                    {State,
                                      #response{success = "false", message = "Password don't match"}};
                                  login_success ->
                                    {#state{registered = true, loggedin = true, socket = State#state.socket, mobile = (Req#request.data)#login.mobile},
                                      #response{success = "true", message = "Login Successful"}}
                                end,
  lager:info("Login response details: [~p]", [LoginResponse]),
  {ok, JsonLoginResponse} = to_json(LoginResponse),
  lager:info("Login json response: [~p]", [JsonLoginResponse]),
  gen_tcp:send(Socket, JsonLoginResponse),
  LoginState;
handle_request(chat, Req, Socket, State) ->
  ChatState = case State#state.loggedin of
                 false -> ChatResponse = #response{success = "false", message = "User can only chat when logged in"},
                   {ok, JsonChatResponse} = to_json(ChatResponse),
                   lager:info("Chat json response: [~p]", [JsonChatResponse]),
                   gen_tcp:send(Socket, JsonChatResponse),
                   State;
                 true -> gen_server:cast(?ROUTER, {private_chat, Req#request.data, Socket}),
                   State
               end,
  ChatState.

terminate(Reason, _) ->
  lager:info("YayServer[Pid:~w] is terminating. [Reason : ~p]", [self(), Reason]).

refresh_socket(Socket) ->
  inet:setopts(Socket, [{active, once}]).

