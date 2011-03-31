%%%-------------------------------------------------------------------
%%% @author Alexey Grebenshchikov <alexey@livestalker.net>
%%% @copyright (C) 2010, Alexey Grebenshchikov
%%% @version 1.0
%%% @doc
%%% Plugin for TK102 like devices
%%% @end
%%%-------------------------------------------------------------------

-module(plg_tk102).

-export([port/0, imei/1, parse/1]).
-include("gprmc.hrl").
-import(gps_utils, [gsign/1, bin_to_float/1, deg_to_rad/1, knots_to_kmh/1]).

%% RE definitions
%% imei
-define(TK102_IMEI, "imei:([0-9]*).*").
%% <<"##,imei:35477703161515,A;">>
-define(TK102_PING, "^##,imei:([0-9]*),A;.*").
%% <<"imei:35477703161515,tracker,1009240151,+79128295580,F,175145.000,A,5836.1916,N,04936.8198,E,0.57,;">>
%% <<"imei:35477703161515,tracker,1009240151,+79128295580,F,000000.000,A,0000.0000,N,04936.8198,E,0.57,;">>
%% <<"imei:35477303161515,tracker,1009240151,+79128295580,F,175145.000,A,5836.1916,N,04936.8198,E,0.57,;">>
-define(TK102_DATA, "^imei:([0-9]*),([a-z]*),([0-9]*),(\\+?[0-9]*),(.?),([.0-9]*),(.?),([.0-9]*),(.?),([.0-9]*),(.?),([.0-9]*),;.*").

-spec port() -> integer().
%% @doc Return port number for plugin.
				  
port() ->
	9000.

-spec imei(Bin :: binary()) -> binary() | nomatch.
%% @doc Return IMEI from captured data
				  
imei(Bin) ->
	{ok, MP} = re:compile(?TK102_IMEI, [caseless]),
	case re:run(Bin, MP, [{capture, all, binary}]) of
		nomatch ->
			nomatch;
		{match, [_, IMEI|_]} ->
			IMEI;
		_ ->
			nomatch
	end.

-spec parse(Bin :: binary()) -> #gprmc{}.
%% @doc Parse captured data and return #gprmc record

parse(Bin) ->
	parse_msg(Bin, re:run(Bin, ?TK102_PING, [{capture, all, binary}]), ping).

%% @private
%% @doc Implementation of the parsing.

parse_msg(_Bin, {match, [_, _IMEI]}, ping) ->
	{ok, pong};
parse_msg(Bin, nomatch, ping) ->
	parse_msg(Bin, re:run(Bin, ?TK102_DATA, [{capture, all, binary}]), data);
parse_msg(_Bin, {match, Data}, data) ->
	[_, IMEI, Cmd, Sn, Tel, _, Sdt, Sfs, Lat, Lat_H, Long, Long_H, Speed] = Data,
	GPRMC = #gprmc{
	  imei  =  IMEI,
	  cmd   =  Cmd,
	  sn    =  Sn,
	  tel   =  Tel,
	  sdt   =  Sdt,
	  sfs   =  Sfs,
	  lat   =  gsign(Lat_H) * deg_to_rad(bin_to_float(Lat)),
	  long  =  gsign(Long_H) * deg_to_rad(bin_to_float(Long)),
	  speed =  knots_to_kmh(bin_to_float(Speed))
	 },
	{data, GPRMC};
parse_msg(_Bin, nomatch, data) ->
	{error, disconnect}.
