-module(nmea_parser).

-export([parse/1]).

-define(PING_PKG, <<"##">>).
-define(DATA_PKG, <<"imei">>).

%<<"##,imei:354777031615151,A;">>
%<<"imei:354777031615151,tracker,1009240152,+79128295580,F,175215.000,A,5836.1950,N,04936.8347,E,1.97,;">>
%<<"imei:354777031615151,help me,1009240200,+79128295580,F,180004.000,A,5836.1947,N,04936.8840,E,0.69,;">>
parse(Data) ->
	Words = binary:split(Data, <<",">>, [global]),
	Cmd = binary:split(get_head(Words), <<":">>),
	case define_type(Cmd, Words) of
		{ok, ping} ->
			{ok, ping};
		{ok, data, Parse_data} ->
			{ok, data, Parse_data};
		error ->
			unknown_cmd
	end.

define_type([?PING_PKG|_], _) ->
	%ping package
	{ok, ping};
define_type([?DATA_PKG|_], Data) ->
	%data package
	[IMEI, CMD, _, _TEL, _, _TIME, _|GEO] = Data,
	%build data list
	{ok, data, [get_imei(binary:split(IMEI, <<":">>)), CMD, get_geo(GEO)]};
define_type(_, _) ->
	error.

get_imei([<<"imei">>|[IMEI|_]]) ->
	IMEI.

get_geo(GEO) when length(GEO) == 6 ->
	[Lat, Lat_D, Long, Long_D, _, _] = GEO,
	Latitude = get_latitude(binary_to_list(binary:part(Lat, 0, 2)),
							binary_to_list(binary:part(Lat, 2, 7)),
							Lat_D),
	Longitude = get_longitude(binary_to_list(binary:part(Long, 0, 3)),
							  binary_to_list(binary:part(Long, 3, 7)),
							  Long_D),
	[{latitude, Latitude}, {longitude, Longitude}];
get_geo(_) ->
	error.

get_latitude(Grad, Min, Lat_D) ->
	Result = get_sign(Lat_D) * (to_int(Grad) + to_float(Min) / 60).

get_longitude(Grad, Min, Long_D) ->
	Result = get_sign(Long_D) * (to_int(Grad) + to_float(Min) / 60).

to_int(Str) ->
	{Result, _} = string:to_integer(Str),
	Result.

to_float(Str) ->
	{Result, _} = string:to_float(Str),
	Result.

get_head([H|_]) ->
	H;
get_head([]) ->
	null.

get_sign(<<"W">>) ->
	-1;
get_sign(<<"S">>) ->
	-1;
get_sign(_) ->
	1.



