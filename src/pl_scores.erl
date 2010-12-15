-module(pl_scores).


-export([
	 calculate/0
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
    [ proplists:get_value(SubRes,pl_results:get_result(E)) || E <- Events ].
    
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
    
			      
