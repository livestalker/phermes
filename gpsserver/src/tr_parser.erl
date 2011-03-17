%%%-------------------------------------------------------------------
%%% @author Alexey Grebenshchikov <alexey@livestalker.net>
%%% @copyright (C) 2010, Alexey Grebenshchikov
%%% @version 1.0
%%% @doc
%%% Parser of GPS data
%%% @end
%%%-------------------------------------------------------------------

-module(tr_parser).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Public API
-export([parse/1, parse_tk102/4, get_imei/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(SQL_READ_DATA, <<"select imei, device_id, user_id from tracker_device where imei=~s">>).
-define(SQL_WRITE_DATA, <<"update tracker_device SET latitude=?, longitude=?, ts_time=? where device_id=? and user_id=? and imei=?">>).

%% RE definitions

%% imei
-define(TK102_IMEI, "imei:([0-9]*).*").

-record(state, {}).
-record(devices, {imei,
                 device_id,
                 user_id,
                 type = ""}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

parse(Bin) ->
	gen_server:cast(?SERVER, {raw, Bin}).

parse_tk102(IMEI, Device_id, User_id, Bin) ->
	gen_server:cast(?SERVER, {tk102, IMEI, Device_id, User_id, Bin}).
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
init([]) ->
	{ok, #state{}}.

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
handle_cast({raw, Bin}, State) ->
	%% raw data
	%% parse imei
	case get_imei(Bin) of
		nomatch ->
			{noreply, State};
		IMEI ->
			error_logger:info_msg("~p~n", [IMEI]),
			write_data(IMEI, Bin),
			{noreply, State}
	end;
handle_cast({tk102, _, _, _, _Bin}, State) ->
	error_logger:info("tk102~n"),
	{noreply, State};
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
handle_info(_Info, State) ->
	{noreply, State}.

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
%%% Internal functions - Parser
%%%===================================================================

get_imei(Bin) ->
	{ok, MP} = re:compile(?TK102_IMEI, [caseless]),
	error_logger:info_msg("~p~n", [Bin]),
	case re:run(Bin, MP, [{capture, all, binary}]) of
		nomatch ->
			nomatch;
		{match, [_, IMEI|_]} ->
			IMEI;
		_ ->
			nomatch
	end.

%%%===================================================================
%%% Internal functions - DB
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Write data in db
%%
%% @spec 
%% @end
%%--------------------------------------------------------------------

write_data(IMEI, Bin) ->
	%% lookup imei in mnesia db
	F = fun() ->
				mnesia:read(devices, IMEI)
		end,
	case mnesia:transaction(F) of
		{atomic, []} ->
			%% no record in cache
			%% try find in mysql
			Result = mysql:fetch(tracker, io_lib:format(?SQL_READ_DATA, [IMEI])),
			{data, {mysql_result, _Fields, Data, _, _, _}} = Result,						
			case write_cache(Data) of
				%% Values = [IMEI, Device_id, User_id]
				{ok, Values} ->
					%% parse data					
					select_parser(Values ++ ["tk102"], Bin),
					ok;
				{error, Reason} ->
					{error, Reason}
			end;
		{atomic, Records} ->
			%% record in cache
			[Record|_] = Records,
			IMEI = Record#devices.imei,
			Device_id = Record#devices.device_id,
			User_id = Record#devices.user_id,
			%% parse data
			select_parser([IMEI, Device_id, User_id, "tk102"], Bin);
		error ->
			%% error when reading record
			{error, transaction_error}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Write record in cache
%%
%% @spec write_cache(list()) -> {ok, list()} | {error, no_data}
%% @end
%%--------------------------------------------------------------------

write_cache([]) ->
	%% no data for write in cache
	{error, no_data};
write_cache([H|_]) ->
	[IMEI, Device_id, User_id] = H,
	Record = #devices{imei = IMEI, device_id = Device_id, user_id = User_id, type = "tk102"},
	F = fun() ->
				mnesia:write(Record)
		end,
	{atomic, ok} = mnesia:transaction(F),
	{ok, [IMEI, Device_id, User_id]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Write record in MySQL DB
%%
%% @spec write_mysql() -> 
%% @end
%%--------------------------------------------------------------------

write_mysql(Lat, Long, Ts_time, Device_id, User_id, IMEI) ->
	mysql:prepare(sql_write_data, ?SQL_WRITE_DATA),
	mysql:execute(tracker, sql_write_data, [Lat, Long, Ts_time, Device_id, User_id, IMEI]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Select parser for data
%%
%% @spec select_parser 
%% @end
%%--------------------------------------------------------------------

select_parser([IMEI, Device_id, User_id, "tk102"], Bin) ->
	parse_tk102(IMEI, Device_id, User_id, Bin);
select_parser([_, _, _, _], _) ->
	{error, unknown_type}.

