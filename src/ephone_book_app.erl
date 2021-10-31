%%%-------------------------------------------------------------------
%% @doc ephone_book public API
%% @end
%%%-------------------------------------------------------------------

-module(ephone_book_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  {ok, _ServerPid} = ephone_book_server:start(get_server_args()),
  ephone_book_sup:start_link().

stop(_State) ->
  ok = ephone_book_server:stop(),
  ok.

%% internal functions

get_server_args() ->
  {ok, Host} = inet:parse_address(os:getenv("HOST", "127.0.0.1")),
  {Port, []} = string:list_to_integer(os:getenv("PORT", "2938")),
  Protocol = os:getenv("PROTOCOL", "http"),
  #{
    host => Host,
    port => Port,
    protocol => Protocol
  }.
