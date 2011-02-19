-module(pl_tally).

-compile(export_all).

fine_price() -> 5.0.
    
buyin_price() -> 60.0.

rebuy_price() -> 30.0.
    
payout_fraction() -> 0.7.    


event_entries({2010,10}) ->
    [{fine,10},{softdrink,6,5,3},{beer,6,7,3.5},{coffee,6,3,1}];


event_entries(_) ->
    [].

calc_total() ->
    pl_results:init(),
    Events = pl_results:all_events(),
    EventTotals = [ calc_event(E) || E <- Events ],
    Totals = 
	lists:foldl( fun({_,{In,Out,Profit}},{In0,Out0,Profit0}) ->
			     {In+In0, Out+Out0, Profit+Profit0}
		     end,
		     {0,0,0},
		     EventTotals),
    {total,Totals}.

calc_event(Evt) ->
    Es = event_entries(Evt),
    Es1 = transform_fines(Es) ++ extract_event_entries(Evt),
    PEs = [ {N * P, N * E } || {_,N,P,E} <- Es1 ],
    {In,Out} = lists:unzip(PEs),
    {P,E} = {lists:sum(In),lists:sum(Out)},
    {Evt,{P,E,P-E}}.

transform_fines(Es) ->
    lists:map( fun({fine,N}) -> {fine,N,fine_price(),0.0};
		     (E) -> E
		  end,
	       Es).

extract_event_entries(Evt) ->
    NoRebuys = pl_scores:no_rebuys(Evt),
    NoBuyins = pl_scores:no_buyins(Evt),
    M = pl_scores:multiplier(Evt),
    Income = (NoRebuys * rebuy_price() + NoBuyins * buyin_price()) * M,
    Payout = (trunc(payout_fraction() * Income) div 30 ) * 30,
    [{buyin,NoBuyins,buyin_price()*M,0},
     {rebuy,NoRebuys,rebuy_price()*M,0},
     {payout,1,0,Payout}].
    




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test(1) ->
    calc_event({2010,10}).
			  
