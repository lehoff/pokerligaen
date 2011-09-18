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
-define(MAX_FRACTION,1).

-export([ 
	  new/2,
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
	  Multiplier::float()) -> #pl_round{}.
new(Ps,M) ->
    N = length(Ps),
    FS = round(1000*(?MAX_FRACTION - ?INITIAL_FRACTION) / (N-1))/1000,
    #pl_round{alive=Ps,
	      no_players=N,
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
	{points,Points::[{atom(),points()}]} |
	{fractions,Fractions::[{atom(),float()}]}.


-spec calculate_points(#pl_round{}) -> [result()].
calculate_points(#pl_round{alive=[],
			   fractions=Fractions,
			   no_players=N,
			   multiplier=Multi}=R) ->
    Busts = enumerate_busts(R),
    FR = final_ranking(Busts),
    PointsBase = round(Multi * points_base(N,no_rebuys(R),sum_addon_percentage(R))),
    PointsA = assign_points(FR,PointsBase),
    Points = apply_fractions(PointsA,Fractions),

    [{points,Points},
     {fractions,Fractions}].
			       
			      

-spec bust(P::atom(),Hitman::atom(),#pl_round{}) -> #pl_round{}.		  
bust(P,Hitman,#pl_round{alive=Ps,
			busted=Bs}=R) ->
    add_event({bust,{P,Hitman}},R#pl_round{alive=lists:delete(P,Ps),
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
    MyF = roundf(CF + 2*S - (S * FPercent/100)),
    R#pl_round{fractions=[{P,MyF}|Fs],
	       current_fraction=MyF}.
	       

-spec add_event(E::term(),#pl_round{}) -> #pl_round{}.
add_event(E,#pl_round{events=Es}=R) ->
    R#pl_round{events=Es ++ [E]}.
		     
-spec process_event(Event::term(),#pl_round{}) ->  #pl_round{}.			   
process_event({rebuy,P},#pl_round{}=R) ->
    rebuy(P,R);
process_event({bust,{P,Hitman}},#pl_round{}=R) ->
    bust(P,Hitman,R);
process_event({addon,{P,AP}},#pl_round{}=R) ->
    addon(P,AP,R).


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
		      lists:last(proplists:lookup_all(P,BE))
	      end,
    calculate_ranking(BustEnum,GetBust).

		 

enumerate_busts(#pl_round{events=Es}) -> 
    Busts = [ P ||  {T,{P,_}} <- Es,
	    T == bust orelse T == addon],
    Numbers = lists:seq(1,length(Busts)),
    lists:zip(Busts,Numbers).

bust_le({_,N},{_,M}) when M =< N -> true;
bust_le(_,_) -> false.
    
total_le(A,B) -> bust_le(A,B).
    

rank_fraction(1) -> 0.47;
rank_fraction(2) -> 0.27;
rank_fraction(3) -> 0.16;
rank_fraction(4) -> 0.10;
rank_fraction(_) -> 0.

roundf(F) ->
    round(F*100)/100.

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


points_base(N,R,SA) ->
    math:pow(N+0.7*(R+SA),1.75) * 30.

test(0) ->
    new([a,b,c,d,e,f],1);
test(1) ->
    R1 = test(0),
    R2 = bust(a,c,R1),
    R2a = rebuy(a,R2),
    R3 = bust(c,b,R2a),
    R4 = addon(d,40,R3),
    R5 = addon(b,20,R4),
    R6 = addon(e,-50,R5),
    R7 = addon(f,-60,R6),
    [R1,R2,R3,R4,R5,R6,R7];
test(2) ->
    lists:last(test(1));
test(3) ->
    R1 = new([a,b,c,d,e,f],1),
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
