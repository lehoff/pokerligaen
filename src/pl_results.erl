-module(pl_results).

-compile(export_all).

-export([
	 init/0,
	 save_result/1,
	 get_result/1,
	 all_results/0,
	 all_events/0,
	 close/0
	 ]).

-export([ 
	  fix_event/1 
	]).

-define(NAME,pl_res2011).

init() ->
    {ok,?NAME} = dets:open_file(?NAME,[]).

save_result(Event) ->
    Res = pl_night:get_result(),
    dets:insert(?NAME,{Event,Res}).

get_result(Event) ->
    dets:lookup(?NAME,Event).

all_results() ->
    dets:traverse(?NAME,fun(X) -> {continue,X} end).
		
all_events() ->		
    lists:sort( [ Name || {Name,_} <- all_results()] ).

close() ->
    dets:close(?NAME).


%%% 
fix_event(Event) ->
    [{Event,Res}] = get_result(Event),
    Events = proplists:get_value(events,Res),
    Bs = event_bounties(Event),
    Multi = event_multi(Event),
    Players = event_players(Res),
    pl_night:start_replay([{init,{Players,Bs,Multi}}|Events]),
    save_result(Event).
    
		   


event_bounties({2010,10}) ->
   [{peter,1}]; 
event_bounties({2010,11}) ->
    [{peter,1},{soren,2}].

event_multi(_) -> 1.
    
event_players(Results) ->
    [ P || {P,_,_}  <- proplists:get_value(clean_points,Results) ].
	    
