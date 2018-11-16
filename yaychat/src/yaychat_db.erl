-module(yaychat_db).
-author("NilanjanC").
-include("yaychat.hrl").

-export([check_user/1, check_node_status/1, list_live_nodes/0, register_node/1, set_node_status_down/0, save_user/1,
         find_user_by_username/1]).


check_user(Username) ->
  lager:info("yaychat_db: check_user called for username ~s", [Username]),
  case mysql_poolboy:query(yaypool, "SELECT COUNT(username) FROM credentials WHERE username = ?", [Username]) of
    {ok, _, [[Count]]} when Count =:= 1 ->
            lager:info("Already ~w user exists with username ~s", [Count, Username]),
            true;
    {error, Reason} -> lager:info("Error querying database, reason: ~p", [Reason]),
            false;
        _ -> false
  end.

check_node_status(Node) ->
  case mysql_poolboy:query(yaypool, "SELECT COUNT(*) FROM live_hosts where is_up = 1 and nodename = ?", [Node]) of
    {ok, _, [[Count]]} when Count =:= 1 ->
      already_up;
    {error, Reason} -> lager:info("Error checking node status. Reason: ~p", [Reason]),
      error;
    _ -> not_available
  end.

list_live_nodes() ->
  lager:info("Query current live nodelist...."),
  case mysql_poolboy:query(yaypool, "SELECT nodename FROM live_hosts where is_up = 1 and nodename != ?", [atom_to_list(node())]) of
    {ok, _, NodeList} when NodeList /= [] ->
      Nodes = lists:flatten(NodeList),
      lager:info("NodeList [~p]", [Nodes]),
      Nodes;
    {error, Reason} -> lager:info("Error loading node list. Reason: ~p", [Reason]),
      error;
    _ -> lager:info("No live node found yet..."),
    []
  end.

register_node(Nodename) ->
  {ok, Hostname} = inet:gethostname(),
   case mysql_poolboy:query(yaypool, "INSERT INTO live_hosts(nodename, is_up, up_since, hostname) VALUES (?, ?, ?, ?)", [Nodename, 1, calendar:local_time(), Hostname]) of
     ok -> ok;
   {error, Reason} -> lager:error("Error inserting the current node details.  Reason: ~p", [Reason]),
    error
  end.

set_node_status_down() ->
  {ok, Hostname} = inet:gethostname(),
   case mysql_poolboy:query(yaypool, "UPDATE live_hosts SET is_up = 0 WHERE is_up = 1 and nodename = ", [atom_to_list(node())]) of
     ok -> ok;
   {error, Reason} -> lager:error("Error updating the current node status.  Reason: ~p", [Reason]),
    error
  end.

save_user(User = #user{}) ->
  SaveUser = fun (Pid) ->
       ok = mysql:query(Pid, "INSERT INTO users(user_fname, user_lname, user_email, user_mobile) VALUES (?, ?, ?, ?)",
                                [User#user.user_fname, User#user.user_lname, User#user.user_email, User#user.user_mobile]),
       LastUserId = mysql:insert_id(Pid),
       ok = mysql:query(Pid, "INSERT INTO credentials(user_id, username, password) VALUES (?, ?, ?)", [LastUserId, User#user.user_mobile, User#user.password]),
      LastUserId
   end,
  Result = mysql_poolboy:transaction(yaypool, SaveUser),
  case Result of
    {atomic, LastInsertedUserId} ->
        io:format("Inserted User id ~p", [LastInsertedUserId]),
       LastInsertedUserId;
    {aborted, Reason} ->
      io:format("User persistence failed due to ~p", [Reason]),
    undefined
  end.

find_user_by_username(Username) ->
  case mysql_poolboy:query(yaypool, """SELECT u.user_id as user_id, u.user_fname as user_fname, u.user_lname as user_lname,
                                        u.user_mobile user_mobile, u.user_email as user_email, c.username as username,
                                        c.password as password from users u join credentials c on u.user_id = c.user_id where
                                        c.username = ? and u.is_active = 1 and c.is_active = 1
                                         """, [Username]) of
    {ok, Columns, Rows} ->
      lager:info("User found for username ~s", [Username]),
      Recs = convert_to_record(Columns, Rows, user, record_info(fields, user)),
      [User|_] = Recs,
      lager:info("User details :[~p]", [User]),
      User;
    {error, Reason} -> lager:info("Error loading user details. Reason: ~p", [Reason]),
      error;
    _ -> record_not_found
  end.

convert_to_record(Columns, Rows, RecordName, Fields) when is_atom(RecordName), is_list(Fields) ->

    S = lists:seq(1, length(Columns)),
    P = lists:zip([ binary_to_atom(C1, utf8) || C1 <- Columns ], S),
    F = fun(FieldName) ->
        case proplists:lookup(FieldName, P) of
            none ->
                    fun(_) -> undefined end;
            {FieldName, Pos} ->
                    fun(Row) -> lists:nth(Pos, Row) end
        end
    end,
    Fs = [ F(FieldName) || FieldName <- Fields ],
    F1 = fun(Row) ->
        RecordData = [ Fx(Row) || Fx <- Fs ],
        list_to_tuple([RecordName|RecordData])
    end,
    [ F1(Row) || Row <- Rows ].
