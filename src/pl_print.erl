-module(pl_print).


-export([
	 print_total/0,
	 print_event/1,
	 print_events_scores/0
	 ]).

-define(NAMES,
	[{torben,"Torben"},
	 {soren, "Soren"},
	 {peter, "Peter"},
	 {jacob, "Jacob"},
	 {benny, "Benny"},
	 {rune, "Rune"},
	 {lars, "Lars"},
	 {nick, "Nick"},
	 {casper, "Casper"},
	 {thomas, "Thomas"},
	 {hilbert, "Hilbert"},
	 {mo,  "Mo"},
	 {shuo, "Shuo"},
	 {raghav, "Raghav"},
	 {jannik, "Jannik"},
	 {alex, "Alex"}
	]).

print_total() ->
    T = pl_scores:calculate(),
    print_table(["Navn","Bounties","Events","Total","Min"],T).

print_events_scores() ->
    Events = pl_results:all_events(),
    TPs = pl_scores:collect_total_points(pl_scores:extract_subresults(total_points,Events)),
    io:format("Navn      Scores~n",[]),
    [ io:format("~-10s~p~n",[pretty_name(P),Scores]) || {P,Scores} <- TPs ].

print_event(Event) ->
    Res = pl_results:get_result(Event),
    EventPoints =
	lists:reverse(
	  lists:keysort(4,[ {P,
			     player_event_points(P,clean_points,Res),
			     player_event_points(P,final_points,Res),
			     player_event_points(P,total_points,Res)} 
			    || P <- event_players(Res) 
			  ])),				       
    print_table(["Navn","Clean","Final","Total"],EventPoints),
    io:format("~n",[]),
    Bounties = event_bounties(Res),
    print_table(["Navn","Bounties","Points"],Bounties).

event_bounties(Res) ->
    proplists:get_value(bounties,Res,[]).

event_players(Results) ->
    [ P || {P,_,_}  <- proplists:get_value(clean_points,Results) ].

player_event_points(Player,CF,Res) when CF == clean_points; CF == final_points ->
    {_,_,Points} = lists:keyfind(Player,1,proplists:get_value(CF,Res,[])),
    Points;
player_event_points(Player,Type,Res) ->
    proplists:get_value(Player,proplists:get_value(Type,Res,[]),0).

print_table(Header,Content) -> 
    NameLengths = [ length(N) || {_,N} <- ?NAMES ],
    NameWidth = lists:max(NameLengths) + 3,
    HeaderWidths = [NameWidth | tl([ length(H) ||  H <- Header ])],
    NewContent = contents_to_strings(Content), %% @todo: insert pretty names
    ContentLines = 
	[ lists:zip(Header,HeaderWidths) |
	 [ lists:zip(Line,HeaderWidths) || Line <- NewContent ]
	 ],
    FormattedLines = 
	[ format_line(L) || L <- ContentLines] ,
    [ io:format("~s~n",[L]) || L <- FormattedLines ].

format_line([{HS,HW}|T]) ->
    string:join([ string:left(HS,HW) |
		  [ string:right(S,W) || {S,W} <- T ]
		],
		"  ").

contents_to_strings(Contents) ->
    [ content_to_string(tuple_to_list(C)) || C <- Contents ].

content_to_string([H|T]) ->
    [ pretty_name(H) |
      [ content_to_list(E) || E <- T ]
    ].
    
content_to_list(N) when is_integer(N) ->
    integer_to_list(N);
content_to_list(Xs) when is_list(Xs) ->
    Ls =[ integer_to_list(X) || X <- Xs ],
    string:join(Ls,",").

pretty_name(Atom) ->
    proplists:get_value(Atom,?NAMES,atom_to_list(Atom)).
			
