-module(index_handler).

-export([init/2]).

-include("../../../models/contact.hrl").

-define(HTML_PATH, "static/html/index.html").

init(Req, Opts) ->
  Contacts = contacts_storage:get_all(),
  ContactsAsHtml = lists:map(fun contact_as_html/1, Contacts),
  ephone_book_server:html_response(Req, Opts, ?HTML_PATH, [ContactsAsHtml]).

contact_as_html(#contact{name = Name, phone = Phone}) ->
  io_lib:format(
    "<li class=\"contact\"><strong>~s</strong> - ~s</li>",
    [Name, Phone]
  ).
