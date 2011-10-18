%%%-------------------------------------------------------------------
%%% File    : pl_night.erl
%%% Author  : Torben Hoffmann <>
%%% Description : Handles a full PokerLigaen nigt.
%%%
%%% Created :  2 Nov 2010 by Torben Hoffmann <>
%%%-------------------------------------------------------------------
-module(pl_night).

-behaviour(gen_server).

-compile(export_all).

%% API
-export([start/3,
	 start/4,
	 start_replay/1,
	 stop/0]).
%% -export([
%% 	 buyin/1,
%% 	 set_bounty/1,
%% 	 start_game/0]).
-export([
	 bust/3,
	 %% rebuy/1,
	 chip_up/2,
	 addon/2,
	 end_game/0,
	 get_result/0,
	 replay_events/1,
	 recording_on/0,
	 recording_off/0
	]).

-export([status/0]).

%% States
%% -export([start_up/3,
%% 	 playing/3,
%% 	 done/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, 
	{multiplier=1,
	 players = [],
	 pl_round,
	 pot,
	 events=[],
	 filename,
	 recording  :: boolean()
	}).

%%--------------------------------------------------------------------
%% start_link(EventName,Multi) ->
%%     gen_fsm:start_link({local,?MODULE}, ?MODULE, [EventName,Multi]).

%% start(P,M) ->
%%     start(P,M).
 
start(Ps,Multi,Filename) ->
%%    gen_server:start({local, ?MODULE}, ?MODULE, [Ps,Multi,Filename,true], []).
    start(Ps, Multi, Filename, true).

start(Ps,Multi,Filename,Recording) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [Ps,Multi,Filename,Recording], []).


restart(Filename) ->
    {ok, Events} = file:consult(Filename),
    start_replay(Events).

start_replay([{init,{Ps,Multi,Filename}}|Es]) ->
    start(Ps,Multi,Filename,false),
    replay_events(Es),
    recording_on().

stop() ->
    gen_server:call(?MODULE,stop).

%%%%%%%

bust(P,Hitman,Rebuy) ->
    gen_server:call(?MODULE,{bust,{P,Hitman,Rebuy}}).

%% rebuy(P) ->
%%     gen_server:call(?MODULE,{rebuy,P}).

addon(P,CurChips) ->
    gen_server:call(?MODULE,{addon,{P,CurChips}}).

chip_up(NewMin,RoundUp) ->
    gen_server:call(?MODULE,{chip_up,{NewMin,RoundUp}}).

end_game() ->
    gen_server:call(?MODULE,end_game).

get_result() ->
    gen_server:call(?MODULE,get_result).

replay_events(Es) ->
    [ gen_server:call(?MODULE,E) || E <- Es ].


status() ->
    gen_server:call(?MODULE,status).

recording_on() ->
    gen_server:call(?MODULE, {recording,true}).

recording_off() ->
    gen_server:call(?MODULE, {recording,false}).


%%%%
init([Players,Multi,Filename,Recording]) ->
    {init_done,State} = execute_event({init,{Players,Multi,Filename}},
				      #state{recording=Recording}),
    {ok, State}.

handle_call(stop,_From,State) ->
    {stop,normal,ok,State};    

handle_call({recording,B}, _From, State) ->
    {reply, ok, State#state{recording=B}};
handle_call(E,_From,State) ->
    {Reply,St2} = execute_event(E,State),
    {reply,Reply,St2}.


add_event(E,#state{events=Events, recording=false}=State) ->
    io:format("correct"),
    State#state{events=Events ++ [E]};
add_event(E,#state{events=Events, recording=true, filename=Filename}=State) ->
    save_event(E, Filename),
    State#state{events=Events ++ [E]}.


save_event(E, Filename) ->
    {ok, F} = file:open(Filename, [write, append]),
    io:format(F, "~p.~n", [E]),
    file:close(F).

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

format_event({bust,{B,H}}) ->
    ok.

%% (event,#state{}) -> {reply,#state{}}
-spec execute_event(term(),#state{}) -> {term(),#state{}}.
execute_event({init,{Players,Multi,Filename}},#state{}=State0) ->
    R = pl_round:new(Players,Multi),
    Pot = pot_ds:new(length(Players),1000,5),
    State1= State0#state{pl_round=R,pot=Pot,
			 filename=Filename
			},
    State = add_event({init,{Players,Multi,Filename}},State1),
    {init_done, State};
execute_event({bust,{P,Hitman,Rebuy}}=E, #state{pl_round=R,pot=Pot}=State) ->
    R1 = pl_round:bust(P,Hitman,R),
    case Rebuy of
	true ->
	    {Chips,Pot2} = pot_ds:rebuy(Pot),
	    R2 = pl_round:rebuy(P,R1),
	    {{new_chips,Chips}, 
	     add_event(E,State#state{pl_round=R2,pot=Pot2})};
	false ->
	    Pot1 = pot_ds:bust(Pot),
	    {no_chips, 
	     add_event(E,State#state{pl_round=R1,
				      pot=Pot1})}
    end;
execute_event({addon,{P,CurChips}}=E, 
	    #state{pl_round=R,pot=Pot}=State) ->
    {{Extra,T,AP},Pot2} = pot_ds:addon(CurChips,Pot),
    R2 = pl_round:addon(P,AP,R),
    Reply = {extra_chips,Extra,{new_total,T}},
    {Reply, 
     add_event(E,State#state{pl_round=R2,
			     pot=Pot2})};
execute_event({chip_up,{NewMin,RoundUp}}=E,
	   #state{pl_round=R,pot=Pot}=State) ->
    try pot_ds:set_min_chip(NewMin,RoundUp,Pot) of
	Pot2 ->
	    {ok,
	     add_event(E,State#state{pot=Pot2})}
    catch
	error:incorrect_roundup ->
	    {incorrect_roundup,State}
    end;
execute_event(end_game=E,#state{pl_round=R}=State) ->
    try pl_round:end_round(R) of
	R2 ->
	    {ok,add_event(E,State#state{pl_round=R2})}
    catch
	error:E ->
	    {E,State}
    end;
execute_event(get_result,#state{pl_round=R,events=Es}=State) ->
    Reply = pl_round:calculate_points(R),
    {Reply ++ [{events,Es}],State};
execute_event(status,#state{pl_round=R,
				pot=Pot,
				events=Es}=State) ->
    {{R,Pot,Es},State}.
	       

test(1) ->
    start([a,b,c,d,e,f],1,"test.log");
test(2) ->
    bust(a,c,true),
    bust(c,b,true),
    bust(d,a,false),
    addon(e,400),
    addon(b,1500),
    addon(f,2200),
    bust(f,b,false),
    bust(c,a,false),
    bust(e,a,false),
    bust(a,b,false),
    end_game(),
    get_result().
