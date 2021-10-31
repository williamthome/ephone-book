-module(ephone_book_server).

-export([start/1, stop/0]).
-export([
  html_response/4,
  read_and_match_urlencoded_body/2,
  read_urlencoded_body/1,
  reply_with_body_as_binary/2
]).

-define(ANY_HOST, '_').
-define(NO_OPTIONS, []).

-define(HOST_MATCH, ?ANY_HOST).
-define(LISTENER, ephone_book).

-define(TEMPLATE_FILENAME, "static/html/template.html").
-define(TEMPLATE, file_utils:read_file_from_priv_dir(?TEMPLATE_FILENAME)).
-define(FUNC_IF_HTML_NOT_FOUND, fun(FileAbsPath) ->
  ["<pre>cannot read:", FileAbsPath, "</pre>"]
end).

-define(EMPTY_BODY_ERROR, {error, no_request_body}).

start(#{host := Host, port := Port, protocol := Protocol})
  when is_tuple(Host)
  andalso Port >= 1024
  andalso Port =< 65535 ->

  Routes = [
    {"/", index_handler, ?NO_OPTIONS},
    {"/api/contacts", api_contacts_handler, ?NO_OPTIONS},
    {"/api/contacts/:id", api_contacts_handler, ?NO_OPTIONS},
    {"/static/css/[...]", cowboy_static, {priv_dir, ephone_book, "static/css"}},
    {"/static/img/[...]", cowboy_static, {priv_dir, ephone_book, "static/img"}},
    {"/[...]", not_found_handler, ?NO_OPTIONS}
  ],
  Dispatch = cowboy_router:compile([{?HOST_MATCH, Routes}]),

  TransOpts = [{ip, Host}, {port, Port}],
  ProtoOpts = #{env => #{dispatch => Dispatch}},

  {ok, CowboyPid} = cowboy:start_clear(?LISTENER, TransOpts, ProtoOpts),

  io:format(
    "Server running at ~s://~s:~p~n",
    [Protocol, host_to_string(Host), Port]
  ),

  {ok, CowboyPid}.

stop() ->
   cowboy:stop_listener(?LISTENER).

html_response(Req, Opts, HtmlPath, Replacements) ->
  {ok, Template} = ?TEMPLATE,
  Content = file_utils:read_file_from_priv_dir(
    HtmlPath,
    ?FUNC_IF_HTML_NOT_FOUND
  ),
  ContentWithReplacements = io_lib:format(Content, Replacements),
  Html = io_lib:format(Template, [ContentWithReplacements]),
  Res = cowboy_req:reply(
    200,
    #{<<"content-type">> => <<"text/html; charset=utf-8">>},
    Html,
    Req
  ),
  {ok, Res, Opts}.

read_and_match_urlencoded_body(Fields, #{has_body := true} = Req)
  when is_list(Fields) ->
    {ok, Data, _Req} = cowboy_req:read_and_match_urlencoded_body(Fields, Req),
    {ok, Data};

read_and_match_urlencoded_body(Fields, _Req)
  when is_list(Fields) ->
    ?EMPTY_BODY_ERROR.

read_urlencoded_body(#{has_body := true} = Req) ->
  {ok, Data, _Req} = cowboy_req:read_urlencoded_body(Req),
  {ok, Data};

read_urlencoded_body(_Req) ->
  ?EMPTY_BODY_ERROR.

reply_with_body_as_binary(Req, Data) ->
  ReqWithBody = cowboy_req:set_resp_body(erlang:term_to_binary(Data), Req),
  cowboy_req:reply(200, ReqWithBody).

host_to_string({127,0,0,1}) ->
  "localhost";

host_to_string(HostAsTuple)
  when is_tuple(HostAsTuple) ->
    ToIntFn = fun(N) -> integer_to_list(N) end,
    AsList = lists:map(ToIntFn, tuple_to_list(HostAsTuple)),
    WithDots = lists:join(".", AsList),
    lists:flatten(WithDots).
