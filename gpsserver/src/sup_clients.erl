%%%-------------------------------------------------------------------
%%% @author Alexey Grebenshchikov <alexey@livestalker.net>
%%% @copyright (C) 2010, Alexey Grebenshchikov
%%% @version 1.0
%%% @doc
%%% Clients supervisor
%%% @end
%%%-------------------------------------------------------------------

-module(sup_clients).
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2, load_plugins/0]).

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

-type socket() :: port().
-type child() :: pid() | undefined.
-type info() :: term().
-type sc_error() :: already_present | {already_started, child()} | term().
-type sc_result() :: {ok, child()} | {ok, child(), info()} | {error, sc_error()}.
-spec start_child(LSock :: socket(), Plugin :: atom()) -> sc_result().
%% @doc Start new child.

start_child(LSock, Plugin) ->
	Port = erlang:apply(Plugin, port, []),
	supervisor:start_child(sup_clients, [LSock, Port, Plugin]).

-spec load_plugins() -> sc_result().
%% @doc Load all plugins (plg_*.beam)
						  
load_plugins() ->
	Plugin = 'plg_tk102',
	Port = erlang:apply(Plugin, port, []),
	{ok, LSock} = gen_tcp:listen(Port, [{packet, raw}, {active, once}]),
	supervisor:start_child(sup_clients, [LSock, Port, Plugin]).

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
	Flags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
	Spec = [
			{undefined, 
			 {srv_client, start_link, []}, 
			 transient,
			 2000,
			 worker,
			 [srv_client]
			}],

	{ok, {Flags, Spec}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
