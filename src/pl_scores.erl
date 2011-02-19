-module(pl_scores).


-export([
	 calculate/0,
	 no_rebuys/1,
	 no_buyins/1,
	 multiplier/1
	]).

calculate() ->
    Events = pl_results:all_events(),
    TotalPoints = total_points(Events),
    Bounties = bounties(Events),
    GrandTotal = sum_results(fun id/1, TotalPoints ++ Bounties),
    [ {P,proplists:get_value(P,Bounties,0),proplists:get_value(P,TotalPoints,0),G} ||
      {P,G} <- GrandTotal ].


%% event_players(Results) ->
%%     [ P || {P,_,_}  <- proplists:get_value(clean_points,Results) ].

total_points(Events) ->
    TPs = extract_subresults(total_points,Events),
    sum_results(fun id/1,TPs).
			

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
