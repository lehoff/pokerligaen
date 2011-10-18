-module(pl_print).

-compile(export_all).

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
    EventPoints = player_scores(Res), 
    print_table(["Navn","Multiplier","Basis","Points"],EventPoints),
    io:format("~n",[]).

event_players(Results) ->
    [ P || {P,_,_}  <- proplists:get_value(points,Results) ].

player_scores(Res) ->
    Players = event_players(Res),
    Points = proplists:get_value(points, Res),
    Fractions = proplists:get_value(fractions, Res),
    [ player_score(P,Points,Fractions) || P <- Players ].

player_score(Player,Points,Fractions) ->
    F = proplists:get_value(Player, Fractions),
    {_,_,P} = lists:keyfind(Player, 1, Points),
    B = round(P/F),
    {Player, F, B, P}.


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
content_to_list(F) when is_float(F) ->
    io_lib:format("~w", [F]);
content_to_list(Xs) when is_list(Xs) ->
    Ls =[ integer_to_list(X) || X <- Xs ],
    string:join(Ls,",").

pretty_name(Atom) ->
    proplists:get_value(Atom,?NAMES,atom_to_list(Atom)).
			
