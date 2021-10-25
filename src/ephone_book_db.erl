-module(ephone_book_db).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([connect/0, connect/1, disconnect/0, query/1]).

-define(SERVER, ?MODULE).
-define(DB_NAME, "ephone_book").
-define(UNDEFINED_CONNECTION, ok).

%% Client

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, ?UNDEFINED_CONNECTION, []).

connect() ->
  Password = password_utils:get_password("Db password: "),
  connect(Password).

connect(Password) ->
  gen_server:cast(?SERVER, {connect, Password}).

disconnect() ->
  gen_server:cast(?SERVER, disconnect).

query(Query) ->
  gen_server:call(?SERVER, {query, Query}).

%% Server

init(?UNDEFINED_CONNECTION) ->
  {ok, ?UNDEFINED_CONNECTION}.

handle_call({query, Query}, _From, Connection) ->
  Response = epgsql:squery(Connection, Query),
  {reply, Response, Connection}.

handle_cast({connect, Password}, ?UNDEFINED_CONNECTION) ->
  {ok, Connection} = epgsql:connect(#{
    host => "localhost",
    username => "williamthome",
    password => Password,
    database => ?DB_NAME,
    timeout => 5000
  }),
  io:format("Database connected.~n"),
  {noreply, Connection};

handle_cast({connect, _Password}, Connection) ->
  io:format("Database already connected.~n"),
  {noreply, Connection};

handle_cast(disconnect, ?UNDEFINED_CONNECTION) ->
  io:format("Database isn't connected.~n"),
  {noreply, ?UNDEFINED_CONNECTION};

handle_cast(disconnect, Connection) ->
  io:format("Connection: ~p~n", [Connection]),
  ok = epgsql:close(Connection),
  io:format("Database disconnected.~n"),
  {noreply, ?UNDEFINED_CONNECTION}.

terminate(_Reason, Connection) ->
  ok = epgsql:close(Connection),
  io:format("Database disconnected.~n"),
  ok.
