%%%-------------------------------------------------------------------
%% @doc ephone_book top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ephone_book_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  SupFlags = #{strategy => one_for_all,
               intensity => 0,
               period => 1},
  ChildSpecs = [
    % #{
    %   id => ephone_book_db,
    %   start => {ephone_book_db, start_link, []}
    % },
    #{
      id => ephone_book_storage_sup,
      start => {ephone_book_storage_sup, start_link, []}
    }
  ],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
