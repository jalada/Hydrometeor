-module(hm_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({global, ?SERVER}, ?MODULE, []).

init([]) ->
	Ip = case os:getenv("MOCHIWEB_IP") of false -> "127.0.0.1"; Any -> Any end,
	WebConfig = [{ip, Ip},
                     {port, 9002},
                     {docroot, local_path(["priv", "www"])}],
	Web = {hm_web, {hm_web, start, [WebConfig]},
	       permanent, 5000, worker, dynamic},
	Server = {hm_server, {hm_server, start_link, []},
		  permanent, 2000, worker, [hm_server]},
	{ok, {{one_for_one,5,10}, [Server, Web]}}.

get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

local_path(Components, Module) ->
    filename:join([get_base_dir(Module) | Components]).

local_path(Components) ->
    local_path(Components, ?MODULE).

