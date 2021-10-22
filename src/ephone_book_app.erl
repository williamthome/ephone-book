%%%-------------------------------------------------------------------
%% @doc ephone_book public API
%% @end
%%%-------------------------------------------------------------------

-module(ephone_book_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ephone_book_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
