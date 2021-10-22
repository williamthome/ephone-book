%%%-------------------------------------------------------------------
%% @doc ephone_book public API
%% @end
%%%-------------------------------------------------------------------

-module(ephone_book_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, StartArgs) ->
  {ok, _ServerPid} = ephone_book_server:start(StartArgs),
  ephone_book_sup:start_link().

stop(_State) ->
  ok = ephone_book_server:stop().

%% internal functions
