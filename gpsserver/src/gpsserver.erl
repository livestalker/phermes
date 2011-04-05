%%%-------------------------------------------------------------------
%%% @author Alexey Grebenshchikov <alexey@livestalker.net>
%%% @copyright (C) 2010, Alexey Grebenshchikov
%%% @version 1.2
%%% @doc
%%% GPS erlang server application
%%% @end
%%%-------------------------------------------------------------------

-module(gpsserver).
-behaviour(application).

%% API
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> term().
%% @private
%% @doc Start application.
				   
start() ->
	application:load(gpsserver),
	application:start(gpsserver).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-type starttype() :: normal | {takeover, Node} | {failover, Node}.
-type startargs() :: term().
-type state() :: term().
-type reason() :: term().
-spec (start(starttype(), startargs()) -> {ok, pid()} | 
										  {ok, pid(), state()} | 
										  {error, reason()}).				  
%% @private
%% @doc 
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end

start(_StartType, _StartArgs) ->
	case sup_gps:start_link() of
		{ok, Pid} ->
			sup_clients:load_plugins(),
			{ok, Pid};
		Error ->
			Error
	end.

-spec stop(state()) -> ok.
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @end

stop(_State) ->
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
