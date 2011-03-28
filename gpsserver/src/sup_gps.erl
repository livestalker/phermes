%%%-------------------------------------------------------------------
%%% @author Alexey Grebenshchikov <alexey@livestalker.net>
%%% @copyright (C) 2010, Alexey Grebenshchikov
%%% @version 1.0
%%% @doc
%%% GPS server top supevisor
%%% @end
%%%-------------------------------------------------------------------

-module(sup_gps).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAX_RESTART, 5). %% Max restarts
-define(MAX_TIME, 60).   %% Seconds between restarts

%%%===================================================================
%%% API functions
%%%===================================================================

-type error() :: {already_started, pid()} | shutdown | term().
-spec (start_link() -> {ok, pid()} | ignore | {error, error()}).
%% @doc Start top supervisor.

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-type spec() :: supervisor:child_spec().
-type flags() :: {RestartStrategy :: supervisor:stratagy(), 
				  MaxR :: integer(), 
				  MaxT :: integer()}.
-spec (init(list()) -> {ok, {flags(), [spec()]}} | ignore).
%% @private
%% @doc Init supervisor when it started using supervisor:start_link/[2,3].

init([]) ->
	Flags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
	Spec = [
			{sup_clients,
			 {sup_clients, start_link, []},
			 permanent,
			 infinity,
			 supervisor,
			 [sup_clients]
			},
			{sup_data,
			 {sup_data, start_link, []},
			 permanent,
			 infinity,
			 supervisor,
			 [sup_data]
			}],

	{ok, {Flags, Spec}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
