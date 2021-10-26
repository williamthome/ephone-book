-module(api_contacts_handler).

-export([init/2]).

-define(POST_METHOD, <<"POST">>).

init(#{method := ?POST_METHOD} = Req, Opts) ->
  Body = ephone_book_server:read_and_match_urlencoded_body(
    [{name, nonempty}, {phone, nonempty}],
    Req
  ),
  case Body of
    {ok, Payload} ->
      case contacts_storage:new(Payload) of
        {ok, Contact} ->
          io:format("Contact created: ~p~n", [Contact]);
        Error ->
          io:format("Error: ~p~n", [Error])
      end;
    Error ->
      io:format("Error: ~p~n", [Error])
  end,
  {ok, Req, Opts}.
