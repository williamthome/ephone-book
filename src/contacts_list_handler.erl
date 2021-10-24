-module(contacts_list_handler).

-export([init/2]).

-define(HTML_PATH, "index.html").

init(Req, Opts) ->
  Contacts = "My contacts",
  ephone_book_server:html_response(Req, Opts, ?HTML_PATH, [Contacts]).
