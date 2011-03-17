%%%-------------------------------------------------------------------
%%% @author Alexey Grebenshchikov <alexey@livestalker.net>
%%% @copyright (C) 2010, Alexey Grebenshchikov
%%% @version 1.0
%%% @doc
%%% Plugin for TK102 like devices
%%% @end
%%%-------------------------------------------------------------------

-module(plg_tk102).

-export([parse/1]).
-include("gprmc.hrl").
-import(tr_utils, [gsign/1, bin_to_float/1, deg_to_rad/1, knots_to_kmh/1]).

%% RE definitions
%% imei
-define(TK102_IMEI, "imei:([0-9]*).*").
%% <<"##,imei:35477703161515,A;">>
-define(TK102_PING, "^##,imei:([0-9]*),A;.*").
%% <<"imei:35477703161515,tracker,1009240151,+79128295580,F,175145.000,A,5836.1916,N,04936.8198,E,0.57,;">>
-define(TK102_DATA, "^imei:([0-9]*),([a-z]*),([0-9]*),(\\+?[0-9]*),(.?),([.0-9]*),(.?),([.0-9]*),(.?),([.0-9]*),(.?),([.0-9]*),;.*").


parse(Bin) ->
	parse_msg(Bin, re:run(Bin, ?TK102_PING, [{capture, all, binary}]), ping).

parse_msg(Bin, {match, [_, IMEI]}, ping) ->
	{ok, pong};
parse_msg(Bin, nomatch, ping) ->
	parse_msg(Bin, re:run(Bin, ?TK102_DATA, [{capture, all, binary}]), data);
parse_msg(Bin, {match, Data}, data) ->
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
	 };
parse_msg(Bin, nomatch, data) ->
	{error, disconnect}.
