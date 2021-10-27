-module(index_handler).

-export([init/2]).

-include("../../../models/contact.hrl").

-define(HTML_PATH, "static/html/index.html").

init(Req, Opts) ->
  Contacts = contacts_storage:get_all(),
  ContactsAsHtml = lists:map(fun contact_as_html/1, Contacts),
  ephone_book_server:html_response(Req, Opts, ?HTML_PATH, [ContactsAsHtml]).

contact_as_html(#contact{id = Id, name = Name, phone = Phone}) ->
  io_lib:format(
    "<li id=\"contact\">
      <span class=\"contact-name\">~s</span>
      <span class=\"contact-phone\">~s</span>
      <button id=\"contact-delete\" data-contact-id=\"~p\">Delete</button>
    </li>",
    [Name, Phone, Id]
  ).
