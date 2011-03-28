%%%-------------------------------------------------------------------
%%% @author Alexey Grebenshchikov <alexey@livestalker.net>
%%% @copyright (C) 2010, Alexey Grebenshchikov
%%% @version 1.0
%%% @doc
%%% Different utils for system
%%% @end
%%%-------------------------------------------------------------------
-module(gps_utils).

%% system API
-export([app_env/2]).

%% math API
-export([bin_to_float/1, deg_to_rad/1, knots_to_kmh/1, gsign/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Getting application enviroment
%% @spec get_app_env(Key::atom(), Default::term()) -> Val | Default
%%       Val = term()    
%% @end
%%--------------------------------------------------------------------

app_env(Key, Default) ->
    case application:get_env(Key) of
		{ok, Val} -> Val;
		undefined -> Default
    end.

bin_to_float(Bin) ->
	N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

deg_to_rad(Num) ->
	Grad = erlang:trunc(Num / 100),
	Min = Num - Grad * 100,
	Grad + Min / 60.
	
knots_to_kmh(Num) ->
	1.852 * Num.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get sign of latitude/logitude
%%
%% @spec get_sign(string()) -> -1 | 1
%% @end
%%--------------------------------------------------------------------

gsign(<<"W">>) ->
	-1;
gsign(<<"S">>) ->
	-1;
gsign(_) ->
	1.
