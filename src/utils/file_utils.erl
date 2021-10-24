-module(file_utils).

-export([read_file_from_priv_dir/1, read_file_from_priv_dir/2]).

-define(PROJECT_NAME, ephone_book).
-define(PRIV_DIR, code:priv_dir(?PROJECT_NAME)).

priv_dir_file(Path) ->
  filename:join([?PRIV_DIR, Path]).

read_file_from_priv_dir(Path) ->
  AbsPath = priv_dir_file(Path),
  File = file:read_file(AbsPath),
  file_binary_or_not_found_func(File, AbsPath).

read_file_from_priv_dir(Path, FuncIfNotFound)
  when is_function(FuncIfNotFound, 1) ->
    AbsPath = priv_dir_file(Path),
    File = file:read_file(AbsPath),
    file_binary_or_not_found_func(File, AbsPath, FuncIfNotFound).

file_binary_or_not_found_func({ok, Binary}, _AbsPath) ->
  {ok, Binary};

file_binary_or_not_found_func(_NotFound, AbsPath) ->
  {error, {not_found, AbsPath}}.

file_binary_or_not_found_func({ok, Binary}, _AbsPath, _FuncIfNotFound) ->
  Binary;

file_binary_or_not_found_func(_NotFound, AbsPath, FuncIfNotFound) ->
  FuncIfNotFound(AbsPath).
