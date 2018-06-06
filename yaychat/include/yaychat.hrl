-record(registration, {name, mobile, email, password}).
-record(login, {mobile, password}).
-record(chat_detail, {from, to, message}).
-record(request, {type, data}).
-record(client, {name, mobile, email, lastLogin}).

-record(response, {success, type, message, data}).

-define(ROUTER, yaychat_router).