-module(yaychat_db).
-author("NilanjanC").
-include("yaychat.hrl").

-export([check_user/1]).


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
