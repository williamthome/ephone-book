-module(contacts_storage).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").
-include("../models/contact.hrl").

-export([test/0, test_crud/0, test_validation/0]).
-export([
  start_link/0, contact_field_position/1,
  new/1, get_all/0, get_by_id/1, update_by_id/2, delete_by_id/1
]).
-export([
  init/1, handle_call/3, handle_cast/2, terminate/2
]).

-define(SERVER, ?MODULE).
-define(TABLE_NAME, contacts).
-define(CONTACT_FIELDS, record_info(fields, contact)).
-define(REQUIRED_CONTACT_FIELDS, [name, phone]).
-define(PRIVATE_CONTACT_FIELDS, [id]).
-define(INDEXED_CONTACT_FIELDS, list_utils:indexed_map(?CONTACT_FIELDS, 2)).

-define(CONTACT_ALREADY_EXISTS_ERROR, {error, contact_already_exists}).
-define(CONTACT_DOES_NOT_EXISTS_ERROR, {error, contact_does_not_exists}).
-define(NOT_A_CONTACT_KEY_ERROR, {error, not_a_contact_key}).

%% Test

test() ->
  ok = test_crud(),
  ok = test_validation(),
  ok.

test_crud() ->
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

test_validation() ->
  ok = is_valid(#{name => "Foo", phone => "123"}),
  {error, _} = is_valid(#{}),
  ok.

%% Client

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new(Contact) ->
  gen_server:call(?MODULE, {new, Contact}).

get_all() ->
  gen_server:call(?MODULE, get_all).

get_by_id(Id) ->
  gen_server:call(?MODULE, {get_by_id, Id}).

update_by_id(Id, Payload) ->
  gen_server:call(?MODULE, {update_by_id, Id, Payload}).

delete_by_id(Id) ->
  gen_server:call(?MODULE, {delete_by_id, Id}).

%% Server

init([]) ->
  {ok, ets:new(?TABLE_NAME,  [{keypos, #contact.id}])}.

handle_call({new, Payload}, _From, Storage)
  when is_map(Payload) ->
    Reply = case map_to_contact(Payload) of
      {ok, Contact} ->
        case ets:insert_new(Storage, Contact) of
          true -> {ok, Contact};
          false -> ?CONTACT_ALREADY_EXISTS_ERROR
        end;
      Error -> Error
    end,
    {reply, Reply, Storage};

handle_call({get_by_id, Id}, _From, Storage) ->
  Reply = case ets:lookup(Storage, Id) of
    [Contact] -> {ok, Contact};
    [] -> ?CONTACT_DOES_NOT_EXISTS_ERROR
  end,
  {reply, Reply, Storage};

handle_call(get_all, _From, Storage) ->
  Reply = ets:tab2list(Storage),
  {reply, Reply, Storage};

handle_call({update_by_id, Id, Payload}, _From, Storage) ->
  ElemSpec = parse_contact_elem_spec(Payload),
  Reply = case ets:update_element(Storage, Id, ElemSpec) of
    true -> {ok, Id};
    false -> ?CONTACT_DOES_NOT_EXISTS_ERROR
  end,
  {reply, Reply, Storage};

handle_call({delete_by_id, Id}, _From, Storage) ->
  Contact = ets:fun2ms(fun({contact, I, _, _}) -> I =:= Id end),
  Reply = case ets:select_delete(Storage, Contact) of
    N when N > 0 -> {ok, Id};
    0 -> ?CONTACT_DOES_NOT_EXISTS_ERROR
  end,
  {reply, Reply, Storage}.

handle_cast(_Msg, Storage) ->
  {noreply, Storage}.

terminate(_Reason, Storage) ->
  ets:delete(Storage),
  ok.

%% Server / Helpers

contact_field_position(Field) ->
  case lists:filter(fun({_Index, Name}) -> Name =:= Field end, ?INDEXED_CONTACT_FIELDS) of
    [{Index, _Name}] -> {ok, Index};
    [] -> ?NOT_A_CONTACT_KEY_ERROR
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
    ?NOT_A_CONTACT_KEY_ERROR ->
      parse_contact_elem_spec(Map, Keys, ElemSpecList)
  end;

parse_contact_elem_spec(_Map, [], ElemSpecList) ->
  ElemSpecList.

cast(Payload) ->
  map_utils:cast(Payload, ?CONTACT_FIELDS, ?PRIVATE_CONTACT_FIELDS).

is_valid(Payload)
  when is_map(Payload) ->
    AllRequired = validate_required(maps:keys(Payload), ?REQUIRED_CONTACT_FIELDS),
    case AllRequired of
      true -> {ok, Payload};
      false -> {error, some_fields_are_missing}
    end.

validate_required(Fields, RequiredFields) ->
  lists:all(fun(Field) -> lists:member(Field, Fields) end, RequiredFields).

gen_id() ->
  calendar:time_to_seconds(erlang:timestamp()).

map_to_contact(Payload)
  when is_map(Payload) ->
    case is_valid(cast(Payload)) of
      {ok, Validated} ->
        Id = gen_id(),
        PayloadWithId = maps:merge(#{id => Id}, Validated),
        Contact = map_utils:map_to_record(PayloadWithId, contact, ?CONTACT_FIELDS),
        {ok, Contact};
      Error -> Error
    end.
