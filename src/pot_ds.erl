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
		  
%% This will not be used directly since it gives the addon'ers the
%% same amount of chips and too many to the ones with few before the
%% add-on
-spec addons([CurChips::pos_integer()],#pot{}) -> {[{ExtraChips::pos_integer(),AddonPcnt::0..100}],
						   #pot{}}.
addons(CurChips,#pot{players=P,
		     chips=C,
		     min_chip=M}=Pot) ->
    {RebuyChips,_} = rebuy(Pot),
    SumCurrent = lists:sum(CurChips),
    NoAddons = length(CurChips),
    SumAddons = (NoAddons*C - P * SumCurrent) / (P - NoAddons),
    Target = round_chips((SumCurrent + SumAddons) / NoAddons, M),
    Extras = [ {Target - CC, Target, round(100*(Target-CC)/RebuyChips)} || CC <- CurChips],
    SumAddons2 = lists:sum([ Target - CC || CC <- CurChips ]),
    {Extras,Pot#pot{chips=C+SumAddons2}}.

-spec addon(CurrentChips::pos_integer(),#pot{}) ->
    {{ExtraChips::pos_integer(),AddonPercentage::0..100},#pot{}}.

%% If the player has more chips than the average chip count he gets no
%% chips, but his AddonPercentage becomes negative and that means a higher
%% fraction for the the final points.
addon(CurChips,#pot{players=P,
		    chips= C,
		    min_chip=M}=Pot) when CurChips > C/P ->
    Extra = { 0, CurChips, round(100*(1-(CurChips*P)/C))},
    {Extra,Pot};

%% When a player has less than the average chip count his
%% AddonPercentage becomes how much of the new average chip count he
%% has to get to level up with the rest.
addon(CurChips,#pot{players=P,
		    chips= C,
		    min_chip=M}=Pot) ->
    {RebuyChips,_} = rebuy(Pot),
    Addon = (C - P * CurChips) / (P - 1),
    Target = round_chips(CurChips + Addon, M),
    Extra = {ExtraChips,_,_} = {Target - CurChips, Target, round(100*(Target-CurChips)/Target)},
    {Extra,Pot#pot{chips=C+ExtraChips}}.
 
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


test(0) ->
    new(6,1000,5);
test(1) ->
    Pot = test(0),
    {R1,P2} = rebuy(Pot),
    {R2,P3} = rebuy(P2),
    P4 = set_min_chip(25,10,P3),
    {R3,P5} = rebuy(P4),
    P6 = bust(P5),
    {{A1,T1,AP1},P7} = addon(300,P6),
    {{A2,T2,AP2},P8} = addon(1500,P7),
    [{R1,P2},{R2,P3},P4,{R3,P5},P6,{{A1,T1,AP1},P7},{{A2,T2,AP2},P8}];
test(2) ->
    Pot = test(0),
    {R1,P2} = rebuy(Pot),
    {R2,P3} = rebuy(P2),
    P4 = set_min_chip(25,10,P3),
    {R3,P5} = rebuy(P4),
    P6 = bust(P5),
    {Extra,P7} = addon(1500,P6),
    [{R1,P2},{R2,P3},P4,{R3,P5},P6,{Extra,P7}];
test(3) ->
    Pot = test(0),
    addon(2000,Pot);
test(4) ->
    Pot = test(0),
    {R1,P2} = rebuy(Pot),
    {R2,P3} = rebuy(P2),
    P4 = set_min_chip(25,10,P3),
    {R3,P5} = rebuy(P4),
    P6 = bust(P5),
    {[{A1,T1,AP1},{A2,T2,AP2}],P7} = addons([300,1500],P6),
    [{R1,P2},{R2,P3},P4,{R3,P5},P6,{[{A1,T1,AP1},{A2,T2,AP2}],P7}];
test(5) ->
Pot = test(0),
    {R1,P2} = rebuy(Pot),
    {R2,P3} = rebuy(P2),
    P4 = set_min_chip(25,10,P3),
    {R3,P5} = rebuy(P4),
    P6 = bust(P5).

