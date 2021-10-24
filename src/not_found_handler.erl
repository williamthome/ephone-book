-module(not_found_handler).

-export([init/2]).

-define(HTML_PATH, "404.html").

init(Req, Opts) ->
  ephone_book_server:html_response(Req, Opts, ?HTML_PATH, []).
