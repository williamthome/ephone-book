-module(password_utils).
-export([get_password/1]).

get_password(Label) ->
  io:format(Label),
  io:get_password().
