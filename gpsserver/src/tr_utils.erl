%%%-------------------------------------------------------------------
%%% @author Alexey Grebenshchikov <alexey@livestalker.net>
%%% @copyright (C) 2010, Alexey Grebenshchikov
%%% @version 1.0
%%% @doc
%%% Different utils for system
%%% @end
%%%-------------------------------------------------------------------
-module(tr_utils).

%% API
-export([app_env/2]).

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

