%%%-------------------------------------------------------------------
%%% @author nilanjanc
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. May 2018 12:37 PM
%%%-------------------------------------------------------------------
-module(yaychat_router).
-author("nilanjanc").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-include("yaychat.hrl").
-record(user, {mobile, password, name, email, loginStatus, pid, socket}).
-record(state, {users}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  lager:info("yaychat_router: initializing"),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  Users = ets:new(users, [set]),
  lager:info("yaychat_router: Users database created successfully"),
  {ok, #state{users = Users}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% %% Check whether the given mobile number is already registered
%% and returns the associated user
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({new_registration, RegDetail, Socket, PID}, _From, State) ->
  Response = ets:insert_new(State#state.users, {RegDetail#registration.mobile,
       RegDetail#registration.password,
       RegDetail#registration.name,
       RegDetail#registration.email,
       false, PID, Socket}),
  {reply, Response, State};
handle_call({new_login, LoginDetail, Socket, PID}, _From, State) ->
  FoundUsers = ets:lookup(State#state.users, LoginDetail#login.mobile),
  Response = case length(FoundUsers) of
    0 -> lager:info("No user found for mobile [~s]", [LoginDetail#login.mobile]),
      user_not_found;
    _ -> User = hd(FoundUsers),
      lager:info("User[~p] found. Login successful", [User]),
      {Mobile, Password, Name, Email, _, _, _} = User,
      case string:equal(LoginDetail#login.password, Password) of
          true ->
            ets:insert(State#state.users, {Mobile,
              Password,
              Name,
              Email,
              true, PID, Socket}),
            Now = calendar:local_time(),
            LoginTime = qdate:to_string("YmdHi", Now),
            #client{name = Name, email = Email, mobile = Mobile, lastLogin = LoginTime};
          false -> lager:info("Password doesn't match"),
            password_dont_match
      end
  end,
  {reply, Response, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({private_chat, ChatDetail, Socket}, State) ->
  From = ChatDetail#chat_detail.from,
  To = ChatDetail#chat_detail.to,
  Recipients = ets:lookup(State#state.users, To),
  case Recipients of
    [] -> lager:info("[~p] is not registered in yaychat", [To]);
    _ -> forward_message(Recipients, From, ChatDetail#chat_detail.message)
  end,
  {noreply, State}.


forward_message([], _, _) -> void;
forward_message([H|T], From, Message) ->
  {Mobile, _, _, _, LoginStatus, Pid, _} = H,
  case LoginStatus of
    true ->
      lager:info("Found recipient process pid [~p]", [Pid]),
      gen_server:cast(Pid, {private_message, From, Message});
    false ->
      lager:info("User[:~p] is offline. So unable to deliver message", [Mobile])
  end,
  forward_message(T, From, Message).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
