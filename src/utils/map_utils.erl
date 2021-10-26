-module(map_utils).

-export([cast/2]).

cast(Map, Fields)
  when is_map(Map) ->
    KeyValueList = maps:to_list(Map),
    cast(Map, Fields, KeyValueList, #{}).

cast(Map, Fields, [{Key, Value} | KeyValueList], ValidData) ->
  case lists:member(Key, Fields) of
    true ->
      NewValidData = maps:put(Key, Value, ValidData),
      cast(Map, Fields, KeyValueList, NewValidData);
    false ->
      cast(Map, Fields, KeyValueList, ValidData)
  end;

cast(_Map, _Fields, [], ValidData) ->
  ValidData.
