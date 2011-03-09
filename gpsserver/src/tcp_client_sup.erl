%%%-------------------------------------------------------------------
%%% @author Alexey Grebenshchikov alexey@livestalker.net
%%% @copyright (C) 2010, Alexey Grebenshchikov
%%% @doc
%%% TCP Listener
%%% @end
%%% Created :  5 Oct 2010 by Alexey Grebenshchikov
%%%-------------------------------------------------------------------

-module(tcp_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

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
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
	supervisor:start_child(tcp_client_sup, []).

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
init([]) ->
	%% supervisr flags
	Flags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
	%% specification of child processes
	Spec = [
			{undefined, 
			 {tcp_fsm, start_link, []}, 
			 temporary,
			 2000,
			 worker,
			 [tcp_fsm]
			}],
	{ok, {Flags, Spec}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
