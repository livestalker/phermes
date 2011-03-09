%%%-------------------------------------------------------------------
%%% @author LiveStalker alexey@livestalker.net
%%% @copyright (C) 2010, LiveStalker
%%% @doc
%%% TCP server top supervisor
%%% @end
%%% Created :  5 Oct 2010 by LiveStalker
%%%-------------------------------------------------------------------
-module(tcp_server_sup).
-author('alexey@livestalker.net').

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAX_RESTART, 5). %% Max restarts
-define(MAX_TIME, 60).   %% Seconds between restarts

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link(Port::integer()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port]) ->
	%% supervisor flags
	Flags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},

	%% specification of child processes
	Spec = [
			{tcp_listener,                       %% Id
			 {tcp_listener, start_link, [Port]}, %% StartFunc = {M, F, A}
			 permanent,                          %% Permanent - child process is always restarted.
			 2000,                               %% Defines how a child process should be terminated. 2000 - timeout befor terminated.
			 worker,                             %% Type of child (worker | supervisor).
			 [tcp_listener]                      %% Callback module, shuld be a list with one element.
			},
			{tcp_client_sup,
			 {tcp_client_sup, start_link, []},
			 permanent,
			 infinity,
			 supervisor,
			 [tcp_client_sup]
			},
			{tracker_sup,
			 {tracker_sup, start_link, []},
			 permanent,
			 infinity,
			 supervisor,
			 [tracker_sup]
			}],
	
	{ok, {Flags, Spec}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
