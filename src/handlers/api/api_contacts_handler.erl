-module(api_contacts_handler).

-export([init/2]).

-define(CONTACT_FIELDS, [name, phone]).

init(#{method := <<"POST">>} = Req, Opts) ->
  ContactOrError = case get_payload(Req) of
    {ok, Payload} -> contacts_storage:new(Payload);
    Error -> Error
  end,
  Res = ephone_book_server:reply_with_body_as_binary(Req, ContactOrError),
  {ok, Res, Opts};

init(#{method := <<"DELETE">>} = Req, Opts) ->
  ContactOrError = case get_id_param(Req) of
    {ok, Id} -> contacts_storage:delete_by_id(Id);
    Error -> Error
  end,
  Res = ephone_book_server:reply_with_body_as_binary(Req, ContactOrError),
  {ok, Res, Opts}.

%% TODO

% init(Req0, State) ->
%     Req = cowboy_req:reply(405, #{
%         <<"allow">> => <<"GET">>
%     }, Req0),
%     {ok, Req, State}.

%% END TODO

get_payload(Req) ->
  ephone_book_server:read_and_match_urlencoded_body(
    ?CONTACT_FIELDS,
    Req
  ).

get_id_param(Req) ->
  case cowboy_req:binding(id, Req) of
    undefined -> {error, missing_id_param};
    Id -> {ok, binary_to_integer(Id)}
  end.
