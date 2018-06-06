-module(yaychat_receiver).
-export([start/1, init/1]).
-record(state, {socket}).
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

start(Socket) ->
  spawn(?MODULE, init, [Socket]).

init(Socket) ->
  receiveLoop(Socket).

receiveLoop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Response} ->
    try from_json(Response, request) of
      {ok, JsonRecord} ->
          case JsonRecord#request.type of
            "chat" ->
              ChatDetail = JsonRecord#request.data,
              mutex:wait(),
              io:format("YayChat>> Message Received~n"),
              io:format("From: [~p]~n", [ChatDetail#chat_detail.from]),
              io:format("Message: [~p]~n", [ChatDetail#chat_detail.message]),
              io:format("------------------------------------------------~n"),
              mutex:signal()
          end
    catch
        throw:_  -> lager:error("Error parsing the received message");
        error:_ -> lager:error("Error parsing the received message");
        exit:_ -> lager:error("Error parsing the received message")
    end
  end,
  receiveLoop(Socket).



