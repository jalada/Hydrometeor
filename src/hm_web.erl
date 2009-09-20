-module(hm_web).

-export([start/1, stop/0, loop/2, subscribe/4, backlog/4]).

-define(TIMEOUT, 110000).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/hm1backend/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "subscribe" ->
			QueryString = Req:parse_qs(),
			case lists:keysearch("callback", 1, QueryString) of
				false ->
					Type = normal;
				{value, {_, Z}} ->
					Type = {callback, Z}
			end,
			case full_keyfind("channel", 1, QueryString) of
				[] ->
					Channels = null;
				List ->
					Channels = [ C || {_, C} <- List ]
			end,
			case lists:keysearch("since", 1, QueryString) of
				false ->
					Since = null;
				{value, {_, Y}} ->
					case string:to_integer(Y) of
						{error, _} ->
							Since = null;
						{X, _} ->
							Since = X
					end
			end,
			if
				Channels /= null ->
					?MODULE:subscribe(Req, Channels, Since, Type);
				true ->
					Req:respond({400, [], []})
			end;
		"backlog" ->
			QueryString = Req:parse_qs(),
			case lists:keysearch("callback", 1, QueryString) of
				false ->
					Type = normal;
				{value, {_, Z}} ->
					Type = {callback, Z}
			end,
			case full_keyfind("channel", 1, QueryString) of
				[] ->
					Channels = null;
				List ->
					Channels = [ C || {_, C} <- List ]
			end,
			case lists:keysearch("count", 1, QueryString) of
				false ->
					Count = null;
				{value, {_, Y}} ->
					case string:to_integer(Y) of
						{error, _} ->
							Count = null;
						{X, _} ->
							Count = X
					end
			end,
			if
				Channels /= null, Count /= null ->
					?MODULE:backlog(Req, Channels, Count, Type);
				true ->
					Req:respond({400, [], []})
			end;
	
		_ ->
			Req:serve_file(Path, DocRoot)
            end;
	'OPTIONS' ->
		case Path of
			"subscribe" ->
				Req:respond({200, [], []});
			"backlog" ->
				Req:respond({200, [], []});
			_ ->
				Req:not_found()
		end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

% Procedure of starting a a stream:
% This is long-polling. So first we need to make sure we haven't missed anything.
% This is done by using the provided Since parameter, if it exists:
%  Get all the channel logs for the requested channels, and if there are any messages
%  in the logs with an id of < Since, send the first one.
% If there wasn't anything, then time to subscribe to them all. The first one that sends a response
% gets sent.
subscribe(Req, Channels, Since, Type) ->
	case Since of
		null ->
			% Much easier
			[ hm_server:login(C, self()) || C <- Channels ],
			Response = Req:ok({"text/html; charset=utf-8",
				   [{"Server","Hydrometeor"}], chunked}),
			feed(Response, Type);
		_ ->
			% Get all the logs
			L = lists:flatten([ hm_server:get_channel_log(C, max) || C <- Channels ]),
			% Find all messages greater_than Since
			M = lists:keysort(1, [ {Id, Msg} || {Id, Msg} <- L,
							    Id > Since]),
			case M of
				[] ->
					% There isn't one, do_stream without Since
					subscribe(Req, Channels, null, Type);
				_ ->
					% Send ourselves the messages, then call feed
					self() ! {router_msg, M},
		                        Response = Req:ok({"text/html; charset=utf-8",
       				                   [{"Server","Hydrometeor"}], chunked}),
					feed(Response, Type)
			end
	end.

backlog(Req, Channels, Count, Type) ->
	Response = Req:ok({"text/html; charset=utf-8",
                   [{"Server","Hydrometeor"}], chunked}),
	% Get a log of Count messages for each channel
	L = lists:keysort(1, lists:flatten([ hm_server:get_channel_log(C, Count) || C <- Channels ])),
	case L of
		[] ->
			ok;
		_ ->
			[ Response:write_chunk(format_chunk(Id, Msg, Type)) || {Id, Msg} <- L ]
	end,
	Response:write_chunk([]).
			

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

feed(Response, Type) ->
        receive
		%% If the router sent a list of messages in the first place, 
		%% this could be consolidated into one. But for now, it's
		%% separate in case I decide to have other messages sent to
		%%this (which would then be tricky to spot
        	{router_msg, {Id, Msg}} ->
			Response:write_chunk(format_chunk(Id, Msg, Type));
		{router_msg, L} when is_list(L) ->
			[ Response:write_chunk(format_chunk(Id, Msg, Type)) || {Id, Msg} <- L ];
		Else ->
			% Just in case.
			io:format("Stream process received unknown message: ~p~n", [Else])
	% Currently hm_server doesn't implement a timeout, as Erlang itself guarantees messages will be sent.
	% Could perhaps be added though.
	after
		?TIMEOUT ->
			Response:write_chunk(["-1,\"\""])
	end,
       	Response:write_chunk([]).

format_chunk(Id, Msg, Type) ->
	% I don't think there's enough backslashes here. Stupid re.
	R = ["\"",re:replace(Msg, "[^\\\\]\\\"", "\\\\\\\"", [{return, list}, global]),"\""],
	case Type of
		normal ->
			[integer_to_list(Id), ",", R, "\n"];
		{callback, Callback} ->
			[integer_to_list(Id), ",", Callback, "(", R, ")\n"]
	end.

	

full_keyfind(Key, N, List) ->
        case lists:keytake(Key, N, List) of
                false ->
                        [];
                {value, Tuple, List2} ->
                        [Tuple | full_keyfind(Key, N, List2)]
        end.
