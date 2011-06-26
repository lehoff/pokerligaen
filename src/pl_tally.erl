-module(pl_tally).

-compile(export_all).

%% @todo: a function to project total pot for the year and winnings.
%% @todo: a funciton to calculate total winnings per player.

fine_price() -> 5.0.
    
buyin_price() -> 60.0.

rebuy_price() -> 30.0.
    
payout_fraction() -> 0.7.   

multiplier_year() -> 
    lists:sum ( [1,1,1.5, 1,1,1,1, 1.5,2] ).

year_base_payouts(Pot) when Pot < 1200 ->
    [{1,400},{2,200},{3,0}];
year_base_payouts(Pot) when Pot >= 1200, Pot < 1700 ->
    [{1,600},{2,400},{3,200}];
year_base_payouts(Pot) when Pot >= 1700, Pot < 2300 ->
    [{1,700},{2,500},{3,300},{4,200}];
year_base_payouts(Pot) when Pot >= 2300, Pot < 2800 ->
    [{1,800},{2,600},{3,400},{4,300},{5,200}];
year_base_payouts(Pot) when Pot >= 2800 ->
    [{1,900},{2,700},{3,500},{4,400},{5,300},{6,200}].

year_fraction(1) -> 0.5;
year_fraction(2) -> 0.25;
year_fraction(3) -> 0.25;
year_fraction(_) -> 0.
    

-type beverage() :: 'softdrink' | 'beer' | 'coffee'.
-type amount() :: float().
-type tally_entry() :: {'fine', non_neg_integer()} | {beverage(), non_neg_integer(), amount(), amount()}.

-type event_name() :: {non_neg_integer(),1..12}.

-spec event_entries(event_name()) -> [ tally_entry() ].			   
event_entries({2010,10}) ->
    [{fine,10},{softdrink,6,5,3},{beer,6,7,3.5},{coffee,6,3,1}];
event_entries({2010,11}) ->
    [{fine,10},{softdrink,5,5,3},{beer,8,7,3.5},{coffee,0,3,1}];
event_entries({2010,12}) ->
    [{fine,19}];
event_entries({2011,1}) ->
    [{fine,7},{softdrink,5,10,2},{beer,8,10,3.5},{coffee,2,3,1}];
event_entries({2011,2}) ->
    [{fine,6},{softdrink,7,5,1.5},{beer,7,3.5}];
event_entries({2011,3}) ->
    [{fine,5},{softdrink,2,8,4},{beer,3,8,4}];
event_entries({2011,4}) ->
    [{fine,6},{softdrink,6,8,4.5},{beer,4,8,4},{coffee,3,3,1}];
event_entries({2011,5}) ->
    [{fine,7},{softdrink,8,8,3.5},{beer,4,8,3.5}];
event_entries({2011,6}) ->
    [{fine,5},{softdrink,9,8,4},{beer,1,8,4}];
event_entries(_) ->
    [].

year_prices() ->
    %% engraving + medals
    226 + 174.

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
    EvtMulti = [ pl_scores:multiplier(E) || E <- Events ],
    [{total,Totals},{event_totals,EventTotals},{multipliers,EvtMulti}].

predict_year_total() ->
    Total = calc_total(),
    Multi = lists:sum( proplists:get_value(multipliers,Total) ),
    Myear = multiplier_year(),
    {_,_,Tally} = proplists:get_value(total,Total),
    Ytotal = trunc((Tally / Multi) * Myear - year_prices()),
    [{year_total,Ytotal},{payouts,year_payouts(Ytotal)}].

year_payouts(Pot) ->
    Base = year_base_payouts(Pot),
    BaseTotal = lists:sum( [ P || {_,P} <- Base ] ),
    Remainder = trunc ( (Pot - BaseTotal) div 40 ) * 40,
    Extra = [ {No,Remainder*year_fraction(No)} || No <- [1,2,3] ],
    [ {No, B + proplists:get_value(No,Extra,0)} || {No,B} <- Base ].

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
			  
