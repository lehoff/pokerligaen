%%%-------------------------------------------------------------------
%%% File    : pl_round.erl
%%% Author  : Torben Hoffmann <>
%%% Description : Calculates the points for a PokerLigaen round.
%%%
%%% Created :  1 Nov 2010 by Torben Hoffmann <>
%%%-------------------------------------------------------------------
-module(pl_round).

-compile(export_all).

-define(INITIAL_FRACTION,0.7).
-define(MAX_FRACTION,0.9).

-export([ 
	  new/3,
	  bust/3,
	  rebuy/2,
	  addon/3,
	  end_round/1,
	  calculate_points/1,
	  events/1
	]).

%-type event() :: {bust,Player}
-type event() :: term().
-type points() :: non_neg_integer().

-record(pl_round,
	{alive = [],
	 busted = [],
	 bounties = [],
	 bounties_collected = [],
	 no_players = 0,
	 fractions = [],
	 current_fraction=0,
	 fraction_step=0,
	 multiplier = 1,
	 events  = []
	}).

-spec events(#pl_round{}) -> [event()].
events(#pl_round{events=Es}) ->
    Es.
		     

-spec new([Player::atom()],
	  Bounties::[{atom(),pos_integer()}],
	  Multiplier::float()) -> #pl_round{}.
new(Ps,Bs,M) ->
    Bs1 = [ {P,B+1} || {P,B} <- Bs ],
    N = length(Ps),
    FS = (?MAX_FRACTION - ?INITIAL_FRACTION) / N,
    #pl_round{alive=Ps,
	      no_players=N,
	      bounties = Bs1,
	      fraction_step=FS,
	      current_fraction=?INITIAL_FRACTION - FS,
	      multiplier=M}.

-spec end_round(#pl_round{}) -> #pl_round{}.
%% The last player collects the bounty on his own head.
end_round(#pl_round{alive=[P]}=R) ->
    bust(P,P,R);
end_round(_) ->
    erlang:error(more_than_one_player_left).

-type result() ::
	{clean_rank,CleanRank::[{atom(),pos_integer(),points()}]} |
        {final_rank,FinalRank::[{atom(),pos_integer(),points()}]} |
	{total_points,TotalPoints::[{atom(),points()}]} |
	{bounties,Bounties::[{atom(),pos_integer(),points()}]} |
	{fractions,Fractions::[{atom(),float()}]}.


-spec calculate_points(#pl_round{}) -> [result()].
calculate_points(#pl_round{alive=[],
			   bounties_collected=BountyCount,
			   fractions=Fractions,
			   no_players=N,
			   multiplier=Multi}=R) ->
    Busts = enumerate_busts(R),
    Players = players_from_bustenum(Busts),
    CR = clean_ranking(Busts),
    FR = final_ranking(Busts),
    CleanPointsBase = Multi * points_base(N,0,0),
    FinalPointsBase = Multi * points_base(N,no_rebuys(R),sum_addon_percentage(R)),
    CleanPoints = assign_points(CR,CleanPointsBase),
    FinalPointsA = assign_points(FR,FinalPointsBase),
    FinalPoints = apply_fractions(FinalPointsA,Fractions),
    TotalPoints = 
	[ {P,get_player_points(P,CleanPoints) +
	     get_player_points(P,FinalPoints)} || P <- Players ],
    BV = Multi * bounty_value(N),
    Bounties = 
	[ {P,BC,BC*BV} || 
	    {P,BC} <- [ {P1, proplists:get_value(P1,BountyCount,0)} 
			|| P1 <- Players ] ],
    [{clean_points,CleanPoints},
     {final_points,FinalPoints},
     {total_points,TotalPoints},
     {bounties,Bounties},
     {fractions,Fractions}].
			       
			      

-spec bust(P::atom(),Hitman::atom(),#pl_round{}) -> #pl_round{}.		  
bust(P,Hitman,#pl_round{alive=Ps,
			busted=Bs}=R) ->
    R2 = get_bounty(P,Hitman,R),
    add_event({bust,{P,Hitman}},R2#pl_round{alive=lists:delete(P,Ps),
					    busted=[P|Bs]}).


-spec rebuy(P::atom(),#pl_round{}) -> #pl_round{}.
rebuy(P,#pl_round{busted=Bs,
		  alive=Alive}=R) ->
    %% @todo: add check that P is busted before revival.
    add_event({rebuy,P},
	      update_fraction(P,100,R#pl_round{alive=[P|Alive],
					     busted=lists:delete(P,Bs)})).

-spec addon(P::atom(),AP::0..100,#pl_round{}) -> #pl_round{}.
addon(P,AP,#pl_round{}=R) ->
    %% @todo: add check that P is alive before adding chips.
    add_event({addon,{P,AP}},
	      update_fraction(P,AP,R)).

-spec update_fraction(P::atom(),FPercent::0..100,#pl_round{}) -> #pl_round{}.		     
update_fraction(P,FPercent,#pl_round{fractions=Fs,
				     current_fraction=CF,
				     fraction_step=S}=R) ->
    MyF = CF + 2*S - (S * FPercent/100),
    R#pl_round{fractions=[{P,MyF}|Fs],
	       current_fraction=MyF}.
	       

-spec add_event(E::term(),#pl_round{}) -> #pl_round{}.
add_event(E,#pl_round{events=Es}=R) ->
    R#pl_round{events=Es ++ [E]}.
		     
-spec process_event(Event::term(),#pl_round{}) ->  #pl_round{}.			   
process_event({rebuy,P},#pl_round{}=R) ->
    ok;
process_event({bust,{P,Hitman}},#pl_round{}=R) ->
    ok;
process_event({addon,{P,AP}},#pl_round{}=R) ->
    ok.

clean_ranking(BustEnum) ->
    GetBust = fun (P,BE) ->
		      hd(proplists:lookup_all(P,BustEnum))
	      end,
    calculate_ranking(BustEnum,GetBust).

players_from_bustenum(BustEnum) ->
    proplists:get_keys(BustEnum).

calculate_ranking(BustEnum,GetBust) ->
    Ps = players_from_bustenum(BustEnum),
    Busts = [ GetBust(P,BustEnum) || P <- Ps ],
    Sorted = lists:sort(fun bust_le/2,Busts),
    Rankings = lists:seq(1,length(Sorted)),
    Ps2 = [ P || {P,_} <- Sorted ],
    lists:zip(Ps2,Rankings).


final_ranking(BustEnum) ->
    GetBust = fun (P,BE) ->
		      lists:last(proplists:lookup_all(P,BustEnum))
	      end,
    calculate_ranking(BustEnum,GetBust).

%% Helper functions
-spec get_bounty(P::atom(),Hitman::atom(),#pl_round{}) -> {Bounty::pos_integer,#pl_round{}}.
get_bounty(P,Hitman,#pl_round{bounties=Bs,
			     bounties_collected=BCs}=R) ->
    B = proplists:get_value(P,Bs,1),
    R#pl_round{bounties=proplists:delete(P,Bs),
	       bounties_collected=orddict:update_counter(Hitman,B,BCs)}.
		 

enumerate_busts(#pl_round{events=Es}) -> 
    Busts = [ P ||  {T,{P,_}} = E <- Es,
	    T == bust orelse T == addon],
    Numbers = lists:seq(1,length(Busts)),
    lists:zip(Busts,Numbers).

bust_le({_,N},{_,M}) when M =< N -> true;
bust_le(_,_) -> false.
    
rank_fraction(1) -> 0.5;
rank_fraction(2) -> 0.3;
rank_fraction(3) -> 0.2;
rank_fraction(_) -> 0.

no_rebuys(#pl_round{events=Es}) ->    
    length( [ E || {rebuy,_} = E <- Es ] ).

sum_addon_percentage(#pl_round{events=Es}) ->
    APs = [ AP || {addon,{_,AP}} <- Es ],
    lists:sum(APs) / 100.

		       
assign_points(Ranking,Base) ->
    [ {P,R,round(rank_fraction(R) * Base)} || {P,R} <- Ranking ].

apply_fractions(FPoints,Fractions) ->
    [ {P,R, round(get_player_fraction(P,Fractions) * Points) } 
       || {P,R,Points} <- FPoints ].

get_player_fraction(P,Fractions) ->
    proplists:get_value(P,Fractions,1).

get_player_points(P,Points) ->
    case lists:keyfind(P,1,Points) of
	{_,_,PP} ->
	    PP
    end.

bounty_value(N) ->
    %% Was about 4% of #1 points before, but now we have two "rounds"
    round(0.10*rank_fraction(1)*points_base(N,0,0)).

points_base(N,R,SA) ->
    math:pow(N+0.7*(R+SA),1.75) * 30.

test(0) ->
    new([a,b,c,d,e,f],[{a,2},{b,1}],1);
test(1) ->
    R1 = test(0),
    R2 = bust(a,c,R1),
    R2a = rebuy(a,R2),
    R3 = bust(c,b,R2a),
    R4 = addon(d,40,R3),
    [R1,R2,R3,R4];
test(2) ->
    lists:last(test(1));
test(3) ->
    R1 = new([a,b,c,d,e,f],[{a,2},{b,1}],1),
    R2 = bust(a,c,R1),
    R2a = rebuy(a,R2),
    R3 = bust(c,b,R2a),
    R4 = rebuy(c,R3),
    R5 = bust(d,a,R4),
    R5a = addon(e,40,R5),
    R6 = bust(f,b,R5a),
    R7 = bust(c,a,R6),
    R8 = bust(e,a,R7),
    R9 = bust(a,b,R8),
    end_round(R9);
test(4) ->
    R = test(3),
    calculate_points(R).
