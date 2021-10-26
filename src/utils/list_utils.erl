-module(list_utils).

-export([test/0, indexed_map/1, indexed_map/2, indexed_map/3]).

test() ->
  [{0, a}, {1, b}, {2, c}] = indexed_map([a, b, c]),
  ok.

indexed_map(List) ->
  indexed_map(List, 0).

indexed_map(List, InitialIndex)
  when is_list(List) ->
    indexed_map(fun(Index, Value) -> {Index, Value} end, List, InitialIndex);

indexed_map(Fn, List) ->
  indexed_map(Fn, List, 0).

indexed_map(Fn, List, InitialIndex)
  when is_function(Fn, 2)
  andalso is_integer(InitialIndex) ->
    indexed_map(Fn, List, InitialIndex, InitialIndex).

indexed_map(Fn, [Last], _InitialIndex, Index) ->
  [Fn(Index, Last)];

indexed_map(Fn, [Head | Tail], _InitialIndex, Index) ->
  [Fn(Index, Head) | indexed_map(Fn, Tail, Index + 1)].
