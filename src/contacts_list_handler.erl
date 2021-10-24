-module(contacts_list_handler).

-export([init/2]).

init(Req, Opts) ->
  Res = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/plain; charset=utf-8">>},
    <<"My contacts list">>,
    Req
  ),
  {ok, Res, Opts}.
