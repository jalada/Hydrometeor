-module(hm_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({global, ?SERVER}, ?MODULE, []).

init([]) ->
	Server = {hm_server, {hm_server, start_link, []},
		  permanent, 2000, worker, [hm_server]},
	{ok, {{one_for_one,5,10}, [Server]}}.
