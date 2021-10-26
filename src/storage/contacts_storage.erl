-module(contacts_storage).
-behaviour(gen_server).
%% TODO
% -include_lib("stdlib/include/ms_transform.hrl").
% -include("../models/contact.hrl").
%% END TODO

-export([start_link/0]).
-export([
  init/1, handle_call/3, handle_cast/2, terminate/2
]).

-define(SERVER, ?MODULE).
-define(TABLE_NAME, contacts).

%% Client

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% TODO

% new(#contact{} = Contact) ->
%   gen_server:call(?MODULE, {new, Contact}).

% get_by_name(Name) ->
%   gen_server:call(?MODULE, {get_by_name, Name}).

% update_by_name(Name, #contact{} = Payload) ->
%   gen_server:call(?MODULE, {update_by_name, Name, Payload}).

% delete_by_name(Name) ->
%   gen_server:call(?MODULE, {delete_by_name, Name}).

%% END TODO

%% Server

init([]) ->
  {ok, ets:new(?TABLE_NAME, [])}.

handle_call(_Msg, _From, Storage) ->
  {reply, Storage, Storage}.

handle_cast(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, Storage) ->
  ets:delete(Storage),
  ok.
