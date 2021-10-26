-module(map_utils).

-export([test/0, test_map_to_record/0]).
-export([cast/2, cast/3, map_to_record/3]).

test() ->
  Map = #{foo => "foo", bar => "bar", invalid_key => ignore, private => ignore},
  Fields = [foo, bar],
  PrivateFields = [private],
  #{foo := "foo", bar := "bar"} = cast(Map, Fields, PrivateFields),
  ok.

test_map_to_record() ->
  Map = #{foo => "foo", bar => "bar"},
  Record = map_to_record(Map, foobar, [foo, bar]),
  io:format("Record: ~p~n", [Record]),
  {foobar, "foo", "bar"} = Record,
  ok.

cast(Map, Fields) ->
  cast(Map, Fields, []).

cast(Map, Fields, PrivateFields)
  when is_map(Map)
  andalso is_list(PrivateFields) ->
    KeyValueList = maps:to_list(Map),
    cast(Map, Fields, PrivateFields, KeyValueList, #{}).

cast(Map, Fields, PrivateFields, [{Key, Value} | KeyValueList], ValidData) ->
  case valid_field(Key, Fields, PrivateFields) of
    true ->
      NewValidData = maps:put(Key, Value, ValidData),
      cast(Map, Fields, PrivateFields, KeyValueList, NewValidData);
    false ->
      cast(Map, Fields, PrivateFields, KeyValueList, ValidData)
  end;

cast(_Map, _Fields, _PrivateFields, [], ValidData) ->
  ValidData.

valid_field(Key, Fields, PrivateFields) ->
  IsKey = lists:member(Key, Fields),
  IsPrivate = lists:member(Key, PrivateFields),
  IsKey and not IsPrivate.

map_to_record(Map, RecordType, RecordFields)
  when is_map(Map)
  andalso is_atom(RecordType)
  andalso is_list(RecordFields) ->
    RecordValues = [maps:get(Key, Map, undefined) || Key <- RecordFields],
    list_to_tuple([RecordType | RecordValues]).