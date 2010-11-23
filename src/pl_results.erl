-module(pl_results).

-export([
	 init/0,
	 save_result/1,
	 get_result/1
	 ]).

-define(NAME,pl_res2011).

init() ->
    {ok,?NAME} = dets:open_file(?NAME,[]).

save_result(Event) ->
    Res = pl_night:get_result(),
    dets:insert(?NAME,{Event,Res}).

get_result(Event) ->
    dets:lookup(?NAME,Event).

close() ->
    dets:close(?NAME).
