%%%-------------------------------------------------------------------
%%% @author Alexey Grebenshchikov alexey@livestalker.net
%%% @copyright (C) 2010, Alexey Grebenshchikov
%%% @doc
%%% TCP FSM
%%% @end
%%% Created :  5 Oct 2010 by Alexey Grebenshchikov
%%%-------------------------------------------------------------------
-module(tcp_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/0, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% fsm states
-export([wait_for_socket/2, wait_for_data/2]).

-define(SERVER, ?MODULE).

%% fsm state record
-record(state, 
		{
		  socket,  %% client socket
		  addr     %% ip client
		}).

-define(TIMEOUT, 60000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit, true),
	{ok, wait_for_socket, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

wait_for_socket({socket_ready, Socket}, StateData) when is_port(Socket) ->
	inet:setopts(Socket, [binary, {packet, raw}, {nodelay, true}, {active, once}]),	
	{ok, {Address, _Port}} = inet:peername(Socket),
    error_logger:info_msg("IP: ~p~n", [Address]),
    {next_state, wait_for_data, StateData#state{socket=Socket, addr=Address}, ?TIMEOUT};

wait_for_socket(Other, StateData) ->
    error_logger:error_msg("State: wait_for_socket. Unexpected message: ~p~n", [Other]),
    {next_state, wait_for_socket, StateData}.

wait_for_data({data, Bin}, #state{socket=Socket} = StateData) ->
    %% echo to client
	ok = gen_tcp:send(Socket, Bin),
	inet:setopts(Socket, [binary, {packet, raw}, {nodelay, true}, {active, once}, {keepalive, true}]),	
	tracker_parser:parse(Bin),
    {next_state, wait_for_data, StateData, ?TIMEOUT};

wait_for_data(timeout, #state{addr=Address} = StateData) ->
    error_logger:info_msg("~p Client connection timeout - closing.~n", [Address]),
    {stop, normal, StateData};

wait_for_data(Other, StateData) ->
    error_logger:error_msg("State: wait_for_data. Unexpected message: ~p~n", [Other]),
    {next_state, wait_for_data, StateData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
%state_name(_Event, _From, StateData) ->
%	Reply = ok,
%

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
	Reply = ok,
	{reply, Reply, StateName, StateData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, _Socket, Bin}, StateName, StateData) ->
	error_logger:info_msg("~p~n", [Bin]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, _Socket}, _StateName, #state{addr=Address} = StateData) ->
    error_logger:info_msg("~p Client disconnected.~n", [Address]),
    {stop, normal, StateData};

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket = Socket} = _State) ->
	gen_tcp:close(Socket),
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
