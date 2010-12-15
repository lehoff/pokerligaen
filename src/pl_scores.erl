-module(pl_scores).


-export([
	 calculate/0
	]).

calculate() ->
    Events = pl_results:all_results(),
    TotalPoints = total_points(Events),
    Bounties = bounties(Events),
    GrandTotal = 
