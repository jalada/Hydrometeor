-module(hm_web).

-export([start/1, stop/0, loop/2, subscribe/6, backlog/5]).

-define(TIMEOUT, 110000).
% This is a macro to placate syntax highlighters..
-define(Q, $\").

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
	"/hm1backend" ++ Path = Req:get(path),
	case Req:get(method) of
		Method when Method =:= 'GET'; Method =:= 'HEAD' ->
			case Path of
				"/subscribe" ->
					QueryString = Req:parse_qs(),
					Type = find_callback(QueryString),
					JSONP = proplists:get_value("jsonp", QueryString),
					Channels = find_channels(QueryString),
					Since = find_number(QueryString, "since"),
					Login_Function = find_method(QueryString),
					if
						Channels /= null ->
							?MODULE:subscribe(Req, Channels, Since, Type, Login_Function, JSONP);
						true ->
							Req:respond({400, [], []})
					end;				
				"/stream" ->
					QueryString = Req:parse_qs(),
					Type = {stream, find_callback(QueryString)},
					Channels = find_channels(QueryString),
					Since = find_number(QueryString, "since"),
					Login_Function = find_method(QueryString),					
					if
						Channels /= null ->
							?MODULE:subscribe(Req, Channels, Since, Type, Login_Function, undefined);
						true ->
							Req:respond({400, [], []})
					end;
				"/backlog" ->
					QueryString = Req:parse_qs(),
					Type = find_callback(QueryString),
					JSONP = proplists:get_value("jsonp", QueryString),					
					Channels = find_channels(QueryString),
					Count = find_number(QueryString, "count"),
					if
						Channels /= null, Count /= null ->
							?MODULE:backlog(Req, Channels, Count, Type, JSONP);
						true ->
							Req:respond({400, [], []})
					end;
				_ ->
					Req:respond({400, [], []})
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
				"/admin/send" ->
					Post = Req:parse_post(),
					case lists:keysearch("channel", 1, Post) of
						false ->
							Channels = null;
						{value, {_, Z}} ->
							L = string:tokens(Z, ","),
							Channels = [ mochiweb_util:unquote(C) || C <- L ]
					end,
					case lists:keysearch("message", 1, Post) of
						false ->
							Message = null;
						{value, {_, Y}} ->
							%Message = list_to_binary(mochiweb_util:unquote(Y))
							Message = list_to_binary(Y)
					end,
					if
						Channels /= null, Message /= null ->
							% This is really simple, so we'll do it right here
							[ hm_server:send(Channel, Message) || Channel <- Channels ],
							Req:respond({200, [], []});
						true ->
							Req:respond({400, [], []})
					end;
				"/admin/deletechannel" ->
					Post = Req:parse_post(),
					case lists:keysearch("channel", 1, Post) of
						false ->
							Channels = null;
						{value, {_, Z}} ->
							L = string:tokens(Z, ","),
							Channels = [ mochiweb_util:unquote(C) || C <- L ]
					end,
					if
						Channels /= null ->
							[ hm_server:delete_channel(Channel) || Channel <- Channels ],
							Req:respond({200, [], []});
						true ->
							Req:respond({400, [], []})
					end;
				_ ->
					Req:not_found()
			end;
		_ ->
			Req:respond({501, [], []})
	end.

find_callback(QueryString) ->
	case lists:keysearch("callback", 1, QueryString) of
		false ->
			normal;
		{value, {_, Z}} ->
			{callback, Z}
	end.

find_channels(QueryString) ->
	case full_keyfind("channel", 1, QueryString) of
		[] ->
			null;
		List ->
			[ mochiweb_util:unquote(C) || {_, C} <- List ]
	end.

find_number(QueryString, Key) ->
	case lists:keysearch(Key, 1, QueryString) of
		false ->
			null;
		{value, {_, Y}} ->
			case string:to_integer(Y) of
				{error, _} ->
					null;
				{X, _} ->
					X
			end
	end.

find_method(QueryString) ->
	case lists:keysearch("method", 1, QueryString) of
		false ->
			% Default to normal login
			fun hm_server:login/2;
		{value, {_, "fanout"}} ->
			fun hm_server:login/2;
		{value, {_, "roundrobin"}} ->
			fun hm_server:rrlogin/2;
		_ ->
			% Too much code to send an error, default
			fun hm_server:login/2
	end.
	
% Procedure of starting a a stream:
% This is long-polling. So first we need to make sure we haven't missed anything.
% This is done by using the provided Since parameter, if it exists:
%  Get all the channel logs for the requested channels, and if there are any messages
%  in the logs with an id of < Since, send the first one.
% If there wasn't anything, then time to subscribe to them all. The first one that sends a response
% gets sent.
subscribe(Req, Channels, Since, Type, Login_Function, JSONP) ->
	Response = Req:ok({"text/html; charset=utf-8",
		   [{"Server","Hydrometeor"}], chunked}),	
	case Since of
		null ->
			% Much easier
			[ Login_Function(C, self()) || C <- Channels ],
			feed(Response, Type, JSONP);
		_ ->
			% Get all the logs
			L = lists:flatten([ hm_server:get_channel_log(C, max) || C <- Channels ]),
			% Find all messages greater_than Since
			M = lists:keysort(1, [ {Id, Msg} || {Id, Msg} <- L,
							    Id > Since]),
			case M of
				[] ->
					% There isn't one, stream without since
					subscribe(Req, Channels, null, Type, Login_Function, JSONP);
				_ ->
					% Send ourselves the messages, then call feed
					self() ! {router_msg, M},
					feed(Response, Type, JSONP)
			end
	end.

backlog(Req, Channels, Count, Type, JSONP) ->
	Response = Req:ok({"text/html; charset=utf-8",
                   [{"Server","Hydrometeor"}], chunked}),
	case JSONP of
		undefined ->
			ok;
		_ ->
			Response:write_chunk([JSONP, "(\""])
	end,
	% Get a log of Count messages for each channel
	L = lists:keysort(1, lists:flatten([ hm_server:get_channel_log(C, Count) || C <- Channels ])),
	case L of
		[] ->
			ok;
		_ ->
			R = newlines([ format_chunk(Id, Msg, Type) || {Id, Msg} <- L ]),
			case JSONP of
				undefined ->
					Response:write_chunk(R);
				_ ->
					Response:write_chunk([js_escape(R), "\")"])
			end
	end,
	Response:write_chunk([]).

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

feed(Response, Type, JSONP) ->
	receive
    	{router_msg, {Id, Msg}} ->
			case JSONP of
				undefined ->
					ok;
				_ ->
					Response:write_chunk([JSONP, "(\""])
			end,
			Response:write_chunk(js_escape(format_chunk(Id, Msg, Type)));
		{router_msg, L} when is_list(L) ->
			case JSONP of
				undefined ->
					ok;
				_ ->
					Response:write_chunk([JSONP, "(\""])
			end,
			R = newlines([ format_chunk(Id, Msg, Type) || {Id, Msg} <- L ]),
			case JSONP of
				undefined ->
					Response:write_chunk(R);
				_ ->
					Response:write_chunk(js_escape(R))
			end;
		Else ->
			% Just in case.
			io:format("Stream process received unknown message: ~p~n", [Else])
	after
		?TIMEOUT ->
			%% Not fully implemented in JS
			Response:write_chunk(["-1,\"\""])
	end,
	case Type of
		{stream, _} ->
			feed(Response, Type, JSONP);
		_ ->
			case JSONP of
				undefined ->
					ok;
				_ ->
					Response:write_chunk(["\")"])
			end,
			Response:write_chunk([])
	end.

format_chunk(Id, Msg, Type) when is_binary(Msg) ->
	% Abusing json encoder to make the string Javascript safe
	R = mochijson2:encode(Msg),	 
	case Type of
		normal ->
			[integer_to_list(Id), ",", R];
		{callback, Callback} ->
			[integer_to_list(Id), ",", Callback, "(", R, ")"];
		{stream, Actual_Type} ->
			[format_chunk(Id, Msg, Actual_Type), "\n"]
	end.

% Adapted from mochijson for when we just want to escape. (for JSONP)
% This will deep recurse through lists and binaries.
js_escape(Str) when is_binary(Str) ->
	js_escape(binary_to_list(Str), [], ext);
js_escape(Str) when is_list(Str) ->
	js_escape(Str, [], ext).
js_escape([], Acc, int) ->
	Acc;
js_escape([], Acc, _) ->
    lists:reverse(Acc);
js_escape([B | Cs], Acc, Type) when is_binary(B) ->
	js_escape(Cs, js_escape(binary_to_list(B), Acc, int), Type);
js_escape([L | Cs], Acc, Type) when is_list(L) ->
	js_escape(Cs, js_escape(L, Acc, int), Type);
js_escape([C | Cs], Acc, Type) when is_integer(C) ->
    Acc1 = case C of
               ?Q ->
                   [?Q, $\\ | Acc];
               $\\ ->
                   [$\\, $\\ | Acc];
               $\b ->
                   [$b, $\\ | Acc];
               $\f ->
                   [$f, $\\ | Acc];
               $\n ->
                   [$n, $\\ | Acc];
               $\r ->
                   [$r, $\\ | Acc];
               $\t ->
                   [$t, $\\ | Acc];
               C ->
			       %% Blindly trust those that came before us
                   [C | Acc]
           end,
    js_escape(Cs, Acc1, Type).

% There must be a better way than writing my own function.
newlines([]) ->
	[];
newlines([H|[]]) ->
	[H];
newlines([H | T]) ->
	[H, "\n" | newlines(T)].

full_keyfind(Key, N, List) ->
        case lists:keytake(Key, N, List) of
                false ->
                        [];
                {value, Tuple, List2} ->
                        [Tuple | full_keyfind(Key, N, List2)]
        end.

any_to_list(X) when is_list(X) ->
	X;
any_to_list(X) when is_bitstring(X) ->
	bitstring_to_list(X).
