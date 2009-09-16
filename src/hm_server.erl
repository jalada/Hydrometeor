-module(hm_server).
-behaviour(gen_server).

%% First attempt at hydrometeor server should:
%% Act as a message router
%% Store messages as they are sent
%% Return recent messages
%% Empty channels

%% Lots of this code is taken from Richard Jones's tutorial about
%% making a million-user Comet application with Mochiweb.

-export([start_link/0]).
-export([login/2, logout/1, send/2, get_channel_log/2, list_channels/0]).
-export([delete_channel/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-define(SERVER, global:whereis_name(?MODULE)).
-define(MAXLOGSIZE, 50).

% state will hold bidirectional mapping between channel <-> pid
% and also contain the last messages sent in a channel in a list.
-record(state, {pid2channel, channel2pid, channels, id}).


% Public API
start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

% Log a Pid into a Channel
login(Channel, Pid) when is_pid(Pid) ->
	gen_server:call(?SERVER, {login, Channel, Pid}).

% Logout of any channels
logout(Pid) when is_pid(Pid) ->
	gen_server:call(?SERVER, {logout, Pid}).

% Send Msg to anyone logged in to Channel.
% I want to set the standards here, Msg should be a binary.
send(Channel, Msg) when is_binary(Msg) ->
	gen_server:call(?SERVER, {send, Channel, Msg});
send(Channel, Msg) when is_list(Msg) ->
	gen_server:call(?SERVER, {send, Channel, list_to_binary(Msg)}).

% Get a message log for a Channel of size Size.
get_channel_log(Channel, Size) when is_integer(Size) ->
	gen_server:call(?SERVER, {get_channel_log, Channel, Size});
get_channel_log(Channel, max) ->
	gen_server:call(?SERVER, {get_channel_log, Channel, ?MAXLOGSIZE}).

list_channels() ->
	gen_server:call(?SERVER, {list_channels}).

% Delete a channel (can also be used to clear a channel's message history)
% Doesn't really need to be a cast, I'm just practising.
delete_channel(Channel) ->
	gen_server:cast(?SERVER, {delete_channel, Channel}).

%%%

init([]) ->
	% set this so we can catch death of logged in pids:
	process_flag(trap_exit, true),
	% use ets for tables. Could be changed to dets for persistent channels.
	% Consider using ordered_set for channels in case we want to traverse
	% them or something.
	{ok, #state{
			pid2channel = ets:new(?MODULE, [bag]),
			channel2pid = ets:new(?MODULE, [bag]),
			channels = ets:new(?MODULE, [set]),
			id = 1
		   }
	}.

handle_call({login, Channel, Pid}, _From, State) when is_pid(Pid) ->
	io:format("~p Logged in to ~p~n", [Pid, Channel]),
	ets:insert(State#state.pid2channel, {Pid, Channel}),
	ets:insert(State#state.channel2pid, {Channel, Pid}),
	% tell us if they exit, so we can log them out
	link(Pid),
	{reply, ok, State};

handle_call({logout, Pid}, _From, State) when is_pid(Pid) ->
	unlink(Pid),
	PidRows = ets:lookup(State#state.pid2channel, Pid),
	case PidRows of
		[] ->
			ok;
		_ ->
			% invert tuples for deletion
			ChannelRows = [ {C,P} || {P,C} <- PidRows ],
			io:format("~p~n", [ChannelRows]),
			% delete all pid->channel entries
			ets:delete(State#state.pid2channel, Pid),
			% and all id->pid
			[ ets:delete_object(State#state.channel2pid, Obj) || Obj <- ChannelRows ]
	end,
	{reply, ok, State};

handle_call({send, Channel, Msg}, _From, State) when is_binary(Msg) ->
	% Make Msg tuple
	IdMsg = {State#state.id, Msg},
	% get Pids logged in to this channel
	Pids = [ P || { _Channel, P} <- ets:lookup(State#state.channel2pid, Channel) ],
	% send Msg to them all
	M = {router_msg, IdMsg},
	[ Pid ! M || Pid <- Pids],
	% add msg to the channel log
	case ets:lookup(State#state.channels, Channel) of
		[] ->
			ets:insert(State#state.channels, {Channel, [IdMsg]});
		[{_Channel, Log}] ->
			ets:insert(State#state.channels, {Channel, lists:sublist([IdMsg | Log], ?MAXLOGSIZE)})
	end,
	{reply, ok, State#state{id=State#state.id + 1}};

handle_call({get_channel_log, Channel, Size}, _From, State) when is_integer(Size) ->
	case ets:lookup(State#state.channels, Channel) of
		[] ->
			{reply, not_found, State};
		[{_Channel, Log}] ->
			{reply, lists:sublist(Log, Size), State}
	end;

handle_call({list_channels}, _From, State) ->
	L = [ {Channel, length(Log), length(ets:lookup(State#state.channel2pid, Channel))} || {Channel, Log} <- ets:tab2list(State#state.channels) ],
	{reply, L, State}.

handle_cast({delete_channel, Channel}, State) ->
	ets:delete(State#state.channels, Channel),
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
