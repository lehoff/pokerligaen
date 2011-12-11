-module(pl_scores).

-compile(export_all).

-export([
	 calculate/0,
	 no_rebuys/1,
	 no_buyins/1,
	 multiplier/1
	]).

calculate() ->
    Events = pl_results:all_events(),
%%   TotalPoints = total_points(Events),
    AdjustedTotal = total_minus_3(Events),
    TotalPoints = [ {Pl,T} || {Pl,{T,_,_}} <- AdjustedTotal ],
%%    GrandTotal = sum_results(fun id/1, TotalPoints ++ Bounties),
    lists:reverse(
      lists:keysort(2,
		    [
		     {P,proplists:get_value(P,TotalPoints,0),
		      lowest_score(P,AdjustedTotal)} ||
			{P,_} <- TotalPoints ])).
		     %% {P,proplists:get_value(P,Bounties,0),proplists:get_value(P,TotalPoints,0),G,
		     %%  lowest_score(P,AdjustedTotal)} ||
		     %% 	{P,G} <- GrandTotal ])).



lowest_score(P,Scores) ->
    {_,Low,_} = proplists:get_value(P,Scores,{na,0,0}),
    Low.


%% event_players(Results) ->
%%     [ P || {P,_,_}  <- proplists:get_value(clean_points,Results) ].

total_points(Events) ->
    TPs = extract_subresults(points,Events),
    sum_results(fun id/1,TPs).

total_minus_3(Events) ->			
    TPs = extract_subresults(points,Events),
    EventScores = collect_event_points(TPs),
    AdjustedScores = adjust_scores(3,9,EventScores),
    [ {Pl,{lists:sum(Ss),lists:last(Ss),Ss}} 
      || {Pl,Ss} <- AdjustedScores ].
    
adjust_scores(Dump,NoEvents,EventScores) ->
    [ {Pl,adjust_score(Dump,NoEvents,Scores)} || {Pl,Scores} <- EventScores ].

adjust_score(Dump,NoEvents,Scores) ->
    Pad = lists:duplicate(NoEvents,0),
    lists:sublist(lists:reverse(lists:sort(Scores++Pad)),
		  NoEvents-Dump).

collect_event_points(TPs) ->
    lists:foldl( fun({Pl, _Pos, Points},Acc) ->
			 orddict:append_list(Pl,[Points],Acc)
		 end,
		 orddict:new(),
		 lists:flatten(TPs)).
    

extract_subresults(SubRes,Events) ->
    [ extract_subresult(SubRes,E) || E <- Events ].

extract_subresult(SubRes,Event) ->
    proplists:get_value(SubRes,pl_results:get_result(Event)).
    
sum_results(F,Xs) ->
    Total = lists:foldl( fun(R,Acc) ->
				 {Pl,Points} = F(R),
				 orddict:update_counter(Pl,Points,Acc)
			 end,
			 orddict:new(),
			 lists:flatten(Xs)),
    sort_total(orddict:to_list(Total)).

id(X) -> X.
    
sort_total(L) ->
    lists:reverse(lists:keysort(2,L)).

bounties(Events) ->
    Bs = extract_subresults(bounties,Events),
    sum_results(fun({P,_,B}) -> {P,B} end,Bs).
    
no_rebuys(Evt) ->		      
    Es = pl_results:get_result(Evt),
    length( [ E || {bust,{_,_,true}} = E <- proplists:get_value(events,Es) ] ). 

no_buyins(Evt) ->
    {Ps,_Bs,_M} = proplists:get_value(init,extract_subresult(events,Evt)),
    length(Ps).

multiplier(Evt) ->
    {_Ps,_Bs,M} = proplists:get_value(init,extract_subresult(events,Evt)),
    M.
