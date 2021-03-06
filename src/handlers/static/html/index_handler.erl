-module(index_handler).

-export([init/2]).

-include("../../../models/contact.hrl").

-define(HTML_PATH, "static/html/index.html").

init(Req, Opts) ->
  % Contacts = lists:duplicate(10, #contact{
  %   id = undefined,
  %   name = "Foo",
  %   phone = "123"
  % }),
  Contacts = contacts_storage:get_all(),
  ContactsAsHtml = lists:map(fun contact_as_html/1, Contacts),
  ephone_book_server:html_response(Req, Opts, ?HTML_PATH, #{
    head => "<script type=\"text/javascript\" src=\"static/js/utils.js\" defer></script>",
    body => ContactsAsHtml
  }).

contact_as_html(#contact{id = Id, name = Name, phone = Phone}) ->
  AvatarImg = file_utils:priv_dir_concat("/static/img/avatar.png"),
  io_lib:format(
    "<li class=\"contact\">
      <img class=\"avatar\" src=\"~s\" alt=\"avatar\">
      <div class=\"contact__info\">
        <span class=\"contact__info-name\">~s</span>
        <span class=\"contact__info-phone\">~s</span>
      </div>
      <div class=\"contact__actions\">
        <button
          class=\"contact__actions-delete secondary-button\"
          data-contact-id=\"~p\"
          data-action=\"delete\"
        >
          Delete
        </button>
      </div>
    </li>",
    [AvatarImg, Name, Phone, Id]
  ).
