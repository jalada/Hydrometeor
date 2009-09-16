-module(hm_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
	hm_sup:start_link().

stop(_State) ->
	exit(whereis(hm_sup), shutdown).
