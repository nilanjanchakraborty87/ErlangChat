-module(helper).

-export([get_body/2]).
-export([get_model/4]).
-export([reply/3]).
-export([pwd2hash/1]).
-export([create_emodel_error_message/1]).



get_body(Body, Req) ->
    case Body of
        [{Input, true}] ->
            {ok, Input, Req};
        [] ->
            {error, empty, reply(400, <<"Missing body">>, Req)};
        _ ->
            {error, empty, reply(400, <<"Bad request">>, Req)}
    end.

get_model(Input, Model, TargetType, Req) ->
    try jiffy:decode(Input, [return_maps]) of
        Data ->
            emodel:from_map(Data, TargetType, Model)
    catch
        _:_ ->
            {error, empty, reply(400, <<"Invalid json">>, Req)}
    end.

create_emodel_error_message(Reason) ->
  MsgMap = lists:foldl(fun(T, M) ->
            {Item, Error} = T,
            maps:put(Item, Error, M)
        end, #{}, Reason),
  binary_to_list(jiffy:encode(MsgMap)).

reply(Code, Body, Req) ->
    cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(Body), Req).

pwd2db(MD5Bin) ->
    list_to_binary(smd5(<<MD5Bin/binary, (application:get_env(yaychat, salt, <<"secret">>))/binary>>)).

pwd2hash(Bin) when is_binary(Bin) ->
    pwd2hash(binary_to_list(Bin));

pwd2hash(L) ->
    pwd2db(list_to_binary(smd5(L))).

smd5(S) ->
    lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= erlang:md5(S)]).
