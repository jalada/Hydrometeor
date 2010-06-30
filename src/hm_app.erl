-module(hm_app).
-behaviour(application).

-export([start/2, stop/1]).

-define(HOUSEKEEPINTERVAL, 300). % 5 minutes in seconds, probably longer in production.

start(_Type, _StartArgs) ->
	% Start loop that tells server to housekeep every...whatever
	spawn(fun() -> housekeeping_timer() end),
	hm_sup:start_link().

stop(_State) ->
	exit(whereis(hm_sup), shutdown).

housekeeping_timer() ->
	timer:sleep(?HOUSEKEEPINTERVAL * 1000),
	R = hm_server:run_housekeeping(),
	% If you want to do anything with this pruned channel information, do it here.
	io:format("Pruned ~p channel(s)~n", [length(R)]),
	housekeeping_timer().