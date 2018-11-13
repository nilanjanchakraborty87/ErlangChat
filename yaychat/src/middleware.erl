-module(middleware).

-export([already_auth/1, auth/1]).

-import(helper, [reply/3]).

auth(Req) ->
    case cowboy_session:get(<<"user">>, Req) of
        {undefined, Req1} ->
            {false, Req1};
        {User, Req1} ->
            {true, User, Req1}
    end.

already_auth(Req) ->
    case auth(Req) of
        {true, User, Req1} ->
            {true, User, reply(400, <<"Already authenticated">>, Req1)};
        {false, Req1} ->
            {false, Req1}
    end.
