-record(registration, {name, mobile, email, password}).
-record(login, {mobile, password}).
-record(chat_detail, {from, to, message}).
-record(request, {type, data}).
-record(client, {name, mobile, email, lastLogin}).
-record(response, {success, type, message, data}).
-record(dbinfo, {username, password, host, port, db}).
-record(dbuser, {user_id, user_fname, user_lname, user_email, user_mobile, user_dob, username, password, is_active}).

-define(ROUTER, yaychat_router).
