-module(contacts_storage).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").
-include("../models/contact.hrl").

-export([test/0]).
-export([
  start_link/0, contact_field_position/1,
  new/1, get_by_id/1, update_by_id/2, delete_by_id/1
]).
-export([
  init/1, handle_call/3, handle_cast/2, terminate/2
]).

-define(SERVER, ?MODULE).
-define(TABLE_NAME, contacts).
-define(CONTACT_FIELDS, list_utils:indexed_map(record_info(fields, contact), 2)).

%% Test

test() ->
  Contact = #contact{id = 0, name = "Foo", phone = "123"},
  ContactId = Contact#contact.id,
  ContactName = Contact#contact.name,
  {ok, Created} = new(Contact),
  true = Created =:= Contact,
  NewPhone = "456",
  {ok, _Updated} = update_by_id(ContactId, #{phone => NewPhone}),
  {ok, Found} = get_by_id(ContactId),
  UpdatedContact = #contact{id = ContactId, name = ContactName, phone = NewPhone},
  true = Found =:= UpdatedContact,
  {ok, _Deleted} = delete_by_id(ContactId),
  ok.

%% Client

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new(Contact) ->
  gen_server:call(?MODULE, {new, Contact}).

get_by_id(Id) ->
  gen_server:call(?MODULE, {get_by_id, Id}).

update_by_id(Id, Payload) ->
  gen_server:call(?MODULE, {update_by_id, Id, Payload}).

delete_by_id(Id) ->
  gen_server:call(?MODULE, {delete_by_id, Id}).

%% Server

init([]) ->
  {ok, ets:new(?TABLE_NAME,  [{keypos, #contact.id}])}.

handle_call({new, #contact{} = Contact}, _From, Storage) ->
  Reply = case ets:insert_new(Storage, Contact) of
    true -> {ok, Contact};
    false -> {error, contact_already_exists}
  end,
  {reply, Reply, Storage};

handle_call({get_by_id, Id}, _From, Storage) ->
  Reply = case ets:lookup(Storage, Id) of
    [Contact] -> {ok, Contact};
    [] -> {error, contact_does_not_exists}
  end,
  {reply, Reply, Storage};

handle_call({update_by_id, Id, Payload}, _From, Storage) ->
  ElemSpec = parse_contact_elem_spec(Payload),
  Reply = case ets:update_element(Storage, Id, ElemSpec) of
    true -> {ok, Id};
    false -> {error, contact_does_not_exists}
  end,
  {reply, Reply, Storage};

handle_call({delete_by_id, Id}, _From, Storage) ->
  Contact = ets:fun2ms(fun({contact, I, _, _}) -> I =:= Id end),
  Reply = case ets:select_delete(Storage, Contact) of
    N when N > 0 -> {ok, Id};
    0 -> {error, contact_does_not_exists}
  end,
  {reply, Reply, Storage}.

handle_cast(_Msg, Storage) ->
  {noreply, Storage}.

terminate(_Reason, Storage) ->
  ets:delete(Storage),
  ok.

%% Server / Helpers

contact_field_position(Field) ->
  case lists:filter(fun({_Index, Name}) -> Name =:= Field end, ?CONTACT_FIELDS) of
    [{Index, _Name}] -> {ok, Index};
    [] -> {error, not_a_contact_key}
  end.

parse_contact_elem_spec(Map)
  when is_map(Map) ->
    parse_contact_elem_spec(Map, maps:keys(Map), []).

parse_contact_elem_spec(Map, [Key | Keys], ElemSpecList) ->
  case contact_field_position(Key) of
    {ok, Position} ->
      Value = maps:get(Key, Map),
      ElemSpec = {Position, Value},
      parse_contact_elem_spec(Map, Keys, [ElemSpec | ElemSpecList]);
    {error, _Reason} ->
      parse_contact_elem_spec(Map, Keys, ElemSpecList)
  end;

parse_contact_elem_spec(_Map, [], ElemSpecList) ->
  ElemSpecList.

%% TODO

% gen_id(Table) ->
%   case ets:last(Table) of
%     '$end_of_table' -> 0;
%     Key -> Key + 1
%   end.
