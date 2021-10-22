-module(ephone_book_db).

-import(epgsql, [connect/1]).
-export([connect/0, disconnect/1, query/2]).

-define(DB_NAME, "ephone_book").

connect() ->
  Password = get_password("Db password: "),
  epgsql:connect(#{
    host => "localhost",
    username => "williamthome",
    password => Password,
    database => ?DB_NAME,
    timeout => 5000
  }).

disconnect(Connection) ->
  epgsql:close(Connection).

query(Connection, Query) ->
  epgsql:squery(Connection, Query).

get_password(Label) ->
  io:format(Label),
  io:get_password().
