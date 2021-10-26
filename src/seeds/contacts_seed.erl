-module(contacts_seed).
-export([create_contacts_table/0]).

create_contacts_table() ->
  {ok, Db} = ephone_book_db:connect(),
  Query = "
    CREATE TABLE IF NOT EXISTS contacts(
      id bigserial PRIMARY KEY,
      name VARCHAR(50) NOT NULL
    );
  ",
  {ok, [], []} = ephone_book_db:query(Db, Query),
  ok = ephone_book_db:disconnect(Db).
