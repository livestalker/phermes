%%%-------------------------------------------------------------------
%%% @author Alexey Grebenshchikov <alexey@livestalker.net>
%%% @copyright (C) 2010, Alexey Grebenshchikov
%%% @version 1.0
%%% @doc
%%% Listening process
%%% @end
%%%-------------------------------------------------------------------

-module(srv_client).

-behaviour(gen_server).

-include("gprmc.hrl").
%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).


-define(SERVER, ?MODULE). 
-define(TIMEOUT, 60000).
-define(SQL_READ_DATA, <<"select imei, device_id, user_id from tracker_device where imei=~s">>).
-define(SQL_WRITE_DATA, <<"update tracker_device SET latitude=?, longitude=?, ts_time=? where device_id=? and user_id=? and imei=?">>).

-record(state, {
		  lsock,          %% Listening socket
		  sock = none,    %% Socket
		  port,           %% Port
		  plugin          %% Plugin
		 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Port::integer()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link(LSock, Port, Plugin) ->
	gen_server:start_link(?MODULE, [LSock, Port, Plugin], []).

%%--------------------------------------------------------------------
%% @doc
%% Accept connection
%%
%% @spec accept() -> ok
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([LSock, Port, Plugin]) ->
	{ok, #state{port = Port, lsock = LSock, plugin = Plugin}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info({tcp, Socket, RawData}, #state{plugin = Plugin, sock = Sock} = State) ->
	Res = erlang:apply(Plugin, parse, [RawData]),
	error_logger:info_msg("Data: ~p~n Parse: ~p~n", [RawData, Res]),
	inet:setopts(Socket, [binary, {packet, raw}, {nodelay, true}, {active, once}, {keepalive, true}]),	
	case Res of
		{ok, pong} ->
			gen_tcp:send(Sock, <<"pong">>);
		{data, GPRMC} ->
			write_data(GPRMC);
		_ ->
			ok
	end,
	{noreply, State, ?TIMEOUT};
handle_info({tcp_closed, _Socket}, State) ->
	error_logger:info_msg("Client close socket.~n"),
	{stop, normal, State};
handle_info(timeout, #state{lsock = LSock, sock = none, plugin = Plugin} = State) ->
	{ok, Sock} = gen_tcp:accept(LSock),
	sup_clients:start_child(LSock, Plugin),
	{noreply, State#state{sock = Sock}, ?TIMEOUT};
handle_info(timeout, #state{sock = Sock} = State) ->
	gen_tcp:close(Sock),
	error_logger:info_msg("Time out (~p ms), socket closed.~n", [?TIMEOUT]),
	{stop, normal, State};
handle_info(Info, _State) ->
	error_logger:info_msg("~p~n", [Info]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
write_data(#gprmc{imei = IMEI, lat = Lat, long = Long} = GPRMC) ->   
	Result = mysql:fetch(pgermes, io_lib:format(?SQL_READ_DATA, [IMEI])),
	{data, {mysql_result, _Fields, Data, _, _, _}} = Result,
	case Data of
		[[_, Device_id, User_id]] ->
			[[_, Device_id, User_id]] = Data,
			Ts_time = "2010-09-03 09:14:26",
			mysql:prepare(sql_write_data, ?SQL_WRITE_DATA),
			mysql:execute(pgermes, sql_write_data, [Lat, Long, Ts_time, Device_id, User_id, IMEI]);
		_ ->
			%% if no records in DB
			%% TODO disconnect device
			ok
	end.
