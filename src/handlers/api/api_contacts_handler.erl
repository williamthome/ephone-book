-module(api_contacts_handler).

-export([init/2]).

-define(POST_METHOD, <<"POST">>).
-define(CONTACT_FIELDS, [name, phone]).

init(#{method := ?POST_METHOD} = Req, Opts) ->
  ContactOrError = case get_payload(Req) of
    {ok, Payload} -> contacts_storage:new(Payload);
    Error -> Error
  end,
  Res = ephone_book_server:reply_with_body_as_binary(Req, ContactOrError),
  {ok, Res, Opts}.

get_payload(Req) ->
  ephone_book_server:read_and_match_urlencoded_body(
    ?CONTACT_FIELDS,
    Req
  ).
