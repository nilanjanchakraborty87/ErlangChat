-module(yaychat_db).
-author("NilanjanC").
-include("yaychat.hrl").

-export([check_user/1, list_live_nodes/0, register_node/1]).


check_user(Username) ->
  lager:info("yaychat_db: check_user called for username ~s", [Username]),
  case mysql_poolboy:query(yaypool, "SELECT COUNT(username) FROM credentials WHERE username = ?", [Username]) of
    {ok, _, [[Count]]} when Count >= 1 ->
            lager:info("Already ~w user exists with username ~s", [Count, Username]),
            true;
    {error, Reason} -> lager:info("Error querying database, reason: ~p", [Reason]),
            false;
        _ -> false
  end.

list_live_nodes() ->
  lager:info("Query current live nodelist...."),
  case mysql_poolboy:query(yaypool, "SELECT hostname FROM live_hosts where is_up = 1") of
    {ok, _, NodeList} when NodeList /= [] ->
      Nodes = lists:flatten(NodeList),
      lager:info("NodeList [~p]", [Nodes]),
      Nodes;
    {error, Reason} -> lager:info("Error loading node list"),
      error;
    _ -> lager:info("No live node found yet..."),
    []
  end.

register_node(Nodename) ->
   case mysql_poolboy:query(yaypool, "INSERT INTO live_hosts(hostname, is_up, up_since) VALUES (?, ?, ?)", [atom_to_list(Nodename), 1, calendar:local_time()]) of
     ok -> ok;
   {error, Reason} -> lager:error("Error inserting the current node details.  Reason: ~p", [Reason]),
    error
  end.
