%%%-------------------------------------------------------------------
%%% @author Alexey Grebenshchikov <alexey@livestalker.net>
%%% @copyright (C) 2010, Alexey Grebenshchikov
%%% @version 1.0
%%% @doc
%%% Supervisor for modules that handle data
%%% @end
%%%-------------------------------------------------------------------

-module(tr_sup).
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

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
	%% supervisor flags
	Flags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},

	%% specification of child processes
	Spec = [
			{tr_parser,                          %% Id
			 {tr_parser, start_link, []},        %% StartFunc = {M, F, A}
			 transient,                          %% Permanent - child process is always restarted.
			 5000,                               %% Defines how a child process should be terminated. 2000 - timeout befor terminated.
			 worker,                             %% Type of child (worker | supervisor).
			 [tr_parser]                         %% Callback module, shuld be a list with one element.
			},
			{mysql,
			 {mysql, start_link, db_opt()},
			 transient,
			 5000,
			 worker,
			 [mysql]
			}],
	
	{ok, {Flags, Spec}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

db_opt() ->
	Db_host = tr_utils:app_env(db_host, "localhost"),
	Db_db = tr_utils:app_env(db_db, ""),
	Db_user = tr_utils:app_env(db_user, ""),
	Db_password = tr_utils:app_env(db_password, ""),
	[pgermes, Db_host, Db_db, Db_user, Db_password].
