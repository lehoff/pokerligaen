-module(pot_ds).

-compile(export_all).

-export([ 
	  new/3,
	  bust/1,
	  rebuy/1,
	  addon/2,
	  set_min_chip/3
	]).

-record(pot,
	{ players = 0 ,
	  chips = 0,
	  min_chip = 5
	}).

-spec new(Players::pos_integer(),
	  InitialChips::pos_integer,
	  MinChip::pos_integer()) -> #pot{}.		 
new(Players,InitialChips,MinChip) ->
    #pot{players = Players,
	 chips   = Players * InitialChips,
	 min_chip = MinChip}.

-spec bust(#pot{}) -> #pot{}.
bust(#pot{players=P}=Pot) ->
    Pot#pot{players=P-1}.

-spec rebuy(#pot{}) -> {Chips::pos_integer(),#pot{}}.
rebuy(#pot{players=P,
	   chips = C,
	   min_chip= M}=Pot) ->
    R1 = C / (P - 1), 
    R = round_chips(R1,M),
    {R, Pot#pot{chips = C + R}}.
		  
-spec addon(CurrentChips::pos_integer(),#pot{}) ->
    {{ExtraChips::pos_integer(),AddonPercentage::0..100},#pot{}}.
addon(CurChips,#pot{players=P,
		    chips= C,
		    min_chip=M}=Pot) ->
    {RebuyChips,_} = rebuy(Pot),
    A1 = (C - P*CurChips) / (P-1),
    A = round_chips(A1,M),
    AP = round(100*A/RebuyChips),
    {{A,AP},Pot#pot{chips=C+A}}.
 
-spec set_min_chip(NewMin::pos_integer(),
		   RoundUp::pos_integer(),
		   #pot{}) -> #pot{}.
set_min_chip(NewMin,RoundUp,#pot{chips=C}=Pot) when ((C+RoundUp) rem NewMin) == 0 ->
    Pot#pot{chips=C+RoundUp,
	    min_chip=NewMin};
set_min_chip(_,_,_) ->
    erlang:error(incorrect_roundup).


%% Helper functions
round_chips(C,M) ->
    M * (round(C/M)).


test(1) ->
    Pot = new(6,1000,5),
    {R1,P2} = rebuy(Pot),
    {R2,P3} = rebuy(P2),
    P4 = set_min_chip(25,10,P3),
    {R3,P5} = rebuy(P4),
    P6 = bust(P5),
    {{A1,AP1},P7} = addon(300,P6),
    [{R1,P2},{R2,P3},P4,{R3,P5},P6,{{A1,AP1},P7}].
