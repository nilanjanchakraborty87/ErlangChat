-record(registration, {name, mobile, email, password}).
-record(login, {mobile, password}).
-record(chat_detail, {from, to, message}).
-record(request, {type, data}).
-record(response, {success, message, data}).
-record(client, {name, mobile, email, password, lastLogin, friends}).