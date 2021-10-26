-module(contacts_storage).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").
-include("../models/contact.hrl").

-export([test/0]).
-export([
  start_link/0,
  new/1, get_by_name/1, update_by_name/2, delete_by_name/1
]).
-export([
  init/1, handle_call/3, handle_cast/2, terminate/2
]).

-define(SERVER, ?MODULE).
-define(TABLE_NAME, contacts).
-define(CONTACT_FIELDS, record_info(fields, contact)).

%% Test

test() ->
  Contact = #contact{name = "Foo", phone = "123"},
  ContactName = Contact#contact.name,
  {ok, Created} = new(Contact),
  true = Created =:= Contact,
  NewPhone = "456",
  {ok, _Updated} = update_by_name(ContactName, #{phone => NewPhone}),
  {ok, Found} = get_by_name(ContactName),
  UpdatedContact = #contact{name = ContactName, phone = NewPhone},
  true = Found =:= UpdatedContact,
  {ok, _Deleted} = delete_by_name(ContactName),
  ok.

%% Client

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new(Contact) ->
  gen_server:call(?MODULE, {new, Contact}).

get_by_name(Name) ->
  gen_server:call(?MODULE, {get_by_name, Name}).

update_by_name(Name, Payload) ->
  gen_server:call(?MODULE, {update_by_name, Name, Payload}).

delete_by_name(Name) ->
  gen_server:call(?MODULE, {delete_by_name, Name}).

%% Server

init([]) ->
  {ok, ets:new(?TABLE_NAME,  [{keypos, #contact.name}])}.

handle_call({new, #contact{} = Contact}, _From, Storage) ->
  Reply = case ets:insert_new(Storage, Contact) of
    true -> {ok, Contact};
    false -> {error, contact_already_exists}
  end,
  {reply, Reply, Storage};

handle_call({get_by_name, Name}, _From, Storage) ->
  Reply = case ets:lookup(Storage, Name) of
    [Contact] -> {ok, Contact};
    [] -> {error, contact_does_not_exists}
  end,
  {reply, Reply, Storage};

handle_call({update_by_name, Name, Payload}, _From, Storage) ->
  ElemSpec = parse_contact_elem_spec(Payload),
  Reply = case ets:update_element(Storage, Name, ElemSpec) of
    true -> {ok, Name};
    false -> {error, contact_does_not_exists}
  end,
  {reply, Reply, Storage};

handle_call({delete_by_name, Name}, _From, Storage) ->
  Contact = ets:fun2ms(fun({contact, N, _}) -> N =:= Name end),
  Reply = case ets:select_delete(Storage, Contact) of
    N when N > 0 -> {ok, Name};
    0 -> {error, contact_does_not_exists}
  end,
  {reply, Reply, Storage}.

handle_cast(_Msg, Storage) ->
  {noreply, Storage}.

terminate(_Reason, Storage) ->
  ets:delete(Storage),
  ok.

%% Server / Helpers

is_contact_field(Field)
  when is_atom(Field) ->
    lists:member(Field, ?CONTACT_FIELDS).

contact_field_position(name) -> {ok, #contact.name};
contact_field_position(phone) -> {ok, #contact.phone};
contact_field_position(_) -> {error, not_a_contact_key}.

parse_contact_elem_spec(Map)
  when is_map(Map) ->
    parse_contact_elem_spec(Map, maps:keys(Map), []).

parse_contact_elem_spec(Map, [Key | Keys], ElemSpecList) ->
  case is_contact_field(Key) of
    true ->
      {ok, Position} = contact_field_position(Key),
      Value = maps:get(Key, Map),
      ElemSpec = {Position, Value},
      parse_contact_elem_spec(Map, Keys, [ElemSpec | ElemSpecList]);
    false ->
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
