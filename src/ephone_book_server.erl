-module(ephone_book_server).

-export([start/1, stop/0, init/2]).

-define(ANY_HOST, '_').
-define(NO_OPTIONS, []).

-define(HOST_MATCH, ?ANY_HOST).
-define(LISTENER, ephone_book).

start(#{host := Host, port := Port, origin := Origin})
  when is_tuple(Host)
  andalso Port >= 1024
  andalso Port =< 65535 ->

  Routes = [
    {"/[...]", ephone_book_server, ?NO_OPTIONS}
  ],
  Dispatch = cowboy_router:compile([{?HOST_MATCH, Routes}]),

  TransOpts = [{ip, Host}, {port, Port}],
  ProtoOpts = #{env => #{dispatch => Dispatch}},

  {ok, CowboyPid} = cowboy:start_clear(?LISTENER, TransOpts, ProtoOpts),

  io:format("Server running at ~s:~p~n", [Origin, Port]),

  {ok, CowboyPid}.

stop() ->
   cowboy:stop_listener(?LISTENER).

init(Req, Opts) ->
  Res = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
    <<"Hello, World!">>,
    Req
  ),
  {ok, Res, Opts}.
