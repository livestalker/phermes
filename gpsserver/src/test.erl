-module(test).

%% API
-export([start/0, stop/0, write_data/0, lookup_mnesia/1, pdata/0, isl/1]).
-define(SQL_READ_DATA, <<"select imei, device_id, user_id from devices where imei=~s">>).
-define(SQL_WRITE_DATA, <<"update devices SET latitude=?, longitude=?, ts_time=? where device_id=? and user_id=? and imei=?">>).
-record(devices, {imei,
                 device_id,
                 user_id,
                 type = ""}).
%%%===================================================================
%%% API
%%%===================================================================

start() ->
	%start application
	application:load(gpsserver),
	application:start(gpsserver).

stop() -> 
	application:stop(gpsserver), 
	R = application:get_key(gpsserver, modules),
	application:unload(gpsserver), 
	case R of 
		{ok, Modules} ->
			unload_modules(Modules); 
		_ -> nop 
	end, ok.  

isl([]) ->
	ok;
isl([H|T]) ->
	io:format("~p~n", [code:is_loaded(H)]),
	isl(T).


unload_modules([]) -> ok;

unload_modules([H | T]) -> 
	code:delete(H), 
	code:purge(H),
	unload_modules(T).

write_data() ->
	%% connect to sql
	IMEI = <<"354777031615151">>,
	erltcps:init_mnesia(),
	mysql:start(tracker, "localhost", "tracker", "tracker", "tracker"),
	lookup_mnesia(IMEI).
	
lookup_mnesia(IMEI) ->
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
				{ok,[IMEI, Device_id, User_id]} ->
					%% parse data
					%% write data in MySQL
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
			%% write data in MySQL
			write_mysql(Device_id, User_id, IMEI);		  
		error ->
			%% error when reading record
			{error, transaction_error}
	end.

write_cache([]) ->
	%% no data for write in cache
	{error, no_data};
write_cache([H|T]) ->
	[IMEI, Device_id, User_id] = H,
	Record = #devices{imei = IMEI, device_id = Device_id, user_id = User_id, type = "tk102"},
	F = fun() ->
				mnesia:write(Record)
		end,
	{atomic, ok} = mnesia:transaction(F),
	{ok, [IMEI, Device_id, User_id]}.

write_mysql(Device_id, User_id, IMEI) ->
	mysql:prepare(sql_write_data, ?SQL_WRITE_DATA),
	mysql:execute(tracker, sql_write_data, [0, 0, 0, Device_id, User_id, IMEI]).

pdata() ->
	%%Bin = <<"##,imei:354777031615151,A;">>,
	%%{ok, MP} = re:compile("^##,imei:([0-9]*),A;\$", [caseless]),
	Bin = <<"imei:354777031615151,tracker,1009240151,+79128295580,F,175145.000,A,5836.1916,N,04936.8198,E,0.57,;">>,
	%%{ok, MP} = re:compile("imei:([0-9]*).*", [caseless]),	
	{ok, MP} = re:compile("^imei:([0-9]*),([a-z]*),([0-9]*),(\\+?[0-9]*),(.?),([.0-9]*),(.?),([.0-9]*),(.?),([.0-9]*),(.?),([.0-9]*),;\$", [caseless]),
	re:run(Bin, MP, [{capture, all, binary}]).
