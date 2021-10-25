-module(api_contacts_handler).

-export([init/2]).

-define(POST_METHOD, <<"POST">>).

init(#{method := ?POST_METHOD} = Req, Opts) ->
  {ok, Contact} = ephone_book_server:read_and_match_urlencoded_body(
    [{name, nonempty}, {number, nonempty}],
    Req
  ),
  io:format("Contact to insert: ~p~n", [Contact]),
  {ok, Req, Opts}.
