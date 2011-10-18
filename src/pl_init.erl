%%%-------------------------------------------------------------------
%%% File    : pl_init.erl
%%% Author  : Torben Hoffmann <>
%%% Description : Used to start a PokerLigaen event.
%%%
%%% Created : 10 Feb 2011 by Torben Hoffmann <>
%%%-------------------------------------------------------------------
-module(pl_init).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([add_player/1]).
-export([set_multiplier/1]).
-export([start_event/0]).
-export([close_event/0]).

-export([show_config/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, 
	{ players=[],
	  multiplier=1,
	  event_name}).

-define(SERVER,?MODULE).

%%====================================================================
%% API
%%====================================================================
start_link({Year,Month}=EventName) when is_integer(Year); is_integer(Month) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, EventName, []).

add_player(P) when is_atom(P) ->
    gen_server:call(?SERVER,{add_player,P});
add_player(Ps) when is_list(Ps) ->
    [ gen_server:call(?SERVER,{add_player,P}) || P <- Ps ].


set_multiplier(M) ->
    gen_server:call(?SERVER,{set_multiplier,M}).

start_event() ->
    gen_server:call(?SERVER,start_event).

close_event() ->
    gen_server:call(?SERVER,close_event).

show_config() ->
    gen_server:call(?SERVER, show_config).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(EventName) ->
    pl_results:init(),
    {ok, #state{event_name=EventName}}.

handle_call(start_event, _From, 
	    #state{players=Ps,multiplier=M,event_name={Year,Month}}=S) ->
    Filename = io_lib:format("pl_~p~p.log",[Year,Month]), 
    pl_night:start(Ps,M,Filename),
    {reply, ok, S};
handle_call(show_config, _From, 
	    #state{players=Ps,multiplier=M}=S) ->
    Reply = [{players,Ps}, {multiplier,M}],
    {reply, Reply, S};
handle_call({add_player,P}, _From, #state{players=Ps}=State) ->
    Ps1 = lists:usort([P|Ps]),
    Reply = ok,
    {reply, Reply, State#state{players=Ps1}};
handle_call({set_multiplier,M}, _From, #state{}=State) ->
    {reply, ok, State#state{multiplier=M}};
handle_call(close_event,_From,#state{event_name=EventName}=State) ->
    pl_night:end_game(),
    pl_results:save_result(EventName),
    pl_results:close(),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
