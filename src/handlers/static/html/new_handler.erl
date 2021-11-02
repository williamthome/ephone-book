-module(new_handler).

-export([init/2]).

-define(HTML_PATH, "static/html/new.html").

init(Req, Opts) ->
  ephone_book_server:html_response(Req, Opts, ?HTML_PATH, #{
    head => "<script type=\"text/javascript\" src=\"static/js/utils.js\" defer></script>"
  }).
