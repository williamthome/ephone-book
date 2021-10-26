-module(ephone_book_storage_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one},
  ChildSpecs = [
    #{
      id => contacts_storage,
      start => {contacts_storage, start_link, []}
    }
  ],
  {ok, {SupFlags, ChildSpecs}}.
