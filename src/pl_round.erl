%%%-------------------------------------------------------------------
%%% File    : pl_round.erl
%%% Author  : Torben Hoffmann <>
%%% Description : Calculates the points for a PokerLigaen round.
%%%
%%% Created :  1 Nov 2010 by Torben Hoffmann <>
%%%-------------------------------------------------------------------
-module(pl_round).

-compile(export_all).

-define(INITIAL_FRACTION,0.5).
-define(MAX_FRACTION,0.9).

-export([ 
	  new/3,
	  bust/3,
	  rebuy/2,
	  addon/3
	]).

%%-type event() :: {bust,Player}

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

-spec new([Player::atom()],
	  Bounties::[{atom(),pos_integer()}],
	  Multiplier::float()) -> #pl_round{}.
new(Ps,Bs,M) ->
    Bs1 = [ {P,B+1} || {P,B} <- Bs ],
    FS = (?MAX_FRACTION - ?INITIAL_FRACTION) / length(Ps),
    #pl_round{alive=Ps,
	      bounties = Bs1,
	      fraction_step=FS,
	      current_fraction=?INITIAL_FRACTION - FS,
	      multiplier=M}.

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
    add_event({addon,{P,AP}},R).

-spec update_fraction(P::atom(),FPercent::0..100,#pl_round{}) -> #pl_round{}.		     
update_fraction(P,FPercent,#pl_round{fractions=Fs,
				     current_fraction=CF,
				     fraction_step=S}=R) ->
    MyF = CF + S * FPercent/100,
    R#pl_round{fractions=[{P,MyF}|Fs],
	       current_fraction=MyF}.
	       

-spec add_event(E::term(),#pl_round{}) -> #pl_round{}.
add_event(E,#pl_round{events=Es}=R) ->
    R#pl_round{events=Es ++ [E]}.
		       

%% Helper functions
-spec get_bounty(P::atom(),Hitman::atom(),#pl_round{}) -> {Bounty::pos_integer,#pl_round{}}.
get_bounty(P,Hitman,#pl_round{bounties=Bs,
			     bounties_collected=BCs}=R) ->
    B = proplists:get_value(P,Bs,1),
    R#pl_round{bounties=proplists:delete(P,Bs),
	       bounties_collected=orddict:update_counter(Hitman,B,BCs)}.
		 

test(1) ->
    R1 = new([a,b,c,d,e,f],[{a,2},{b,1}],1),
    R2 = bust(a,c,R1),
    R2a = rebuy(a,R2),
    R3 = bust(c,b,R2a),
    [R1,R2,R3].
