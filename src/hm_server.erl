-module(hm_server).
-behaviour(gen_server).

%% Lots of this code is taken from Richard Jones's tutorial about
%% making a million-user Comet application with Mochiweb.

-export([start_link/0]).
-export([login/2, rrlogin/2, logout/1, send/2, get_channel_log/2, list_channels/0,
	 current_id/0]).
-export([delete_channel/1, dump_messages/0, load_messages/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-define(SERVER, ?MODULE).
-define(MAXLOGSIZE, 50).
-define(CHANNELSTATEFILENAME, "hm_channelstate.log").

% state will hold bidirectional mapping between channel <-> pid
% and also contain the last messages sent in a channel in a list.
-record(state, {pid2channel, channel2pid, channels, id}).
-record(channel, {log, last_accessed}).

% Public API
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Log a Pid into a Channel
login(Channel, Pid) when is_pid(Pid) ->
	gen_server:call(?SERVER, {login, Channel, Pid}).

% Log a Pid into a RR Channel
rrlogin(Channel, Pid) when is_pid(Pid) ->
	gen_server:call(?SERVER, {rrlogin, Channel, Pid}).

% Logout of any channels
logout(Pid) when is_pid(Pid) ->
	gen_server:call(?SERVER, {logout, Pid}).

% Send Msg to anyone logged in to Channel.
% I want to set the standards here, Msg should be a binary.
% TODO: Change everything else so we don't need to unquote here.
send(Channel, Msg) when is_binary(Msg) ->
	gen_server:cast(?SERVER, {send, mochiweb_util:unquote(Channel), Msg});
send(Channel, Msg) when is_list(Msg) ->
	gen_server:cast(?SERVER, {send, mochiweb_util:unquote(Channel), list_to_binary(Msg)}).

% Get a message log for a Channel of size Size.
get_channel_log(Channel, Size) when is_integer(Size) ->
	gen_server:call(?SERVER, {get_channel_log, Channel, Size});
get_channel_log(Channel, max) ->
	gen_server:call(?SERVER, {get_channel_log, Channel, ?MAXLOGSIZE}).

list_channels() ->
	gen_server:call(?SERVER, {list_channels}).

current_id() ->
	gen_server:call(?SERVER, {current_id}).

% Delete a channel (can also be used to clear a channel's message history)
% Doesn't really need to be a cast, I'm just practising.
% TODO: remove need for unquote (clientfall)
delete_channel(Channel) ->
	gen_server:cast(?SERVER, {delete_channel, mochiweb_util:unquote(Channel)}).

% Dump message queues out to a pre-determined file. Used for maintaining state
% across restarts.
dump_messages() ->
	gen_server:call(?SERVER, {dump_messages}).

% Ditto the above, but for loading.
load_messages() ->
	gen_server:call(?SERVER, {load_messages}).

%%%

init([]) ->
	% set this so we can catch death of logged in pids:
	process_flag(trap_exit, true),

	% Load channel queues from state file.
	case ets:file2tab(filename:absname("") ++ "/" ++ ?CHANNELSTATEFILENAME) of
		{error, Reason} ->
			io:format("Error loading channels from statefile: ~p~nMaking new channel table~n", [Reason]),
			C = ets:new(?MODULE, [set]);
		{ok, Tab} ->
			C = Tab
	end,

	% Consider using ordered_set for channels in case we want to traverse
	% them or something.
	{ok, #state{
			pid2channel = ets:new(?MODULE, [bag]),
			channel2pid = ets:new(?MODULE, [bag]),
			channels = C,
			id = 1
		   }
	}.

handle_call({login, Channel, Pid}, _From, State) when is_pid(Pid) ->
	ets:insert(State#state.pid2channel, {Pid, Channel}),
	ets:insert(State#state.channel2pid, {Channel, Pid}),
	% tell us if they exit, so we can log them out
	link(Pid),
	{reply, ok, State};

handle_call({rrlogin, Channel, Pid}, _From, State) when is_pid(Pid) ->
	ets:insert(State#state.pid2channel, {Pid, Channel}),
	case ets:lookup(State#state.channel2pid, Channel) of
		[] ->
			% Not important
			Old_Data = [],
			Data = [Pid];
		% Pure RR channel
		[{Channel, List}] when is_list(List) ->
			Old_Data = List,
			Data = lists:append(List, [Pid]);
		% Hybrid, or just normal subscribers
		List when is_list(List) ->
			case find_rr_list([ X || {_Channel, X} <- List ]) of
				false ->
					Old_Data = [],
					Data = [Pid];
				RR_List ->
					Old_Data = RR_List,
					Data = lists:append(RR_List, [Pid])
			end
	end,
	ets:delete_object(State#state.channel2pid, {Channel, Old_Data}),
	ets:insert(State#state.channel2pid, {Channel, Data}),
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
			% delete all pid->channel entries
			ets:delete(State#state.pid2channel, Pid),
			% For efficiency, try and delete all the channel2pids we can first anyway
			[ ets:delete_object(State#state.channel2pid, Obj) || Obj <- ChannelRows ],
			% Now find results containing this Pid (e.g. round robin)
			% Quite longwinded to weed them out.
			
			Channels = [ C || {C,_} <- ChannelRows ],
			Possible_Pid_Lists = lists:flatten([ ets:select(State#state.channel2pid, [{{Channel, '$2'}, [{is_list, '$2'}], ['$_']}]) || Channel <- Channels]),
			[ find_and_delete_pids(State#state.channel2pid, Obj, Pid) || Obj <- Possible_Pid_Lists ]
	end,
	{reply, ok, State};

handle_call({get_channel_log, Channel, Size}, _From, State) when is_integer(Size) ->
	case ets:lookup(State#state.channels, Channel) of
		[] ->
			{reply, [], State};
		[{_Channel, Channel_State}] ->
			{reply, lists:sublist(Channel_State#channel.log, Size), State}
	end;

handle_call({list_channels}, _From, State) ->
	L = [ {Channel, length(Channel_State#channel.log), length(ets:lookup(State#state.channel2pid, Channel))} || {Channel, Channel_State} <- ets:tab2list(State#state.channels) ],
	{reply, L, State};

handle_call({current_id}, _From, State) ->
	{reply, State#state.id, State};

handle_call({dump_messages}, _From, State) ->
	R = ets:tab2file(State#state.channels, filename:absname("") ++ "/" ++ ?CHANNELSTATEFILENAME),
	{reply, R, State};

handle_call({load_messages}, _From, State) ->
	case ets:file2tab(?CHANNELSTATEFILENAME) of
		{error, Reason} ->
			{reply, {error, Reason}, State};
		{ok, Tab} ->
			{reply, ok, State#state{channels = Tab}}
	end.

handle_cast({delete_channel, Q_Channel}, State) ->
	% TODO: Don't want to use unquote
	Channel = mochiweb_util:unquote(Q_Channel),
	ets:delete(State#state.channels, Channel),
	{noreply, State};

handle_cast({send, Channel, Msg}, State) when is_list(Msg) ->
	handle_cast({send, Channel, list_to_binary(Msg)}, State);
handle_cast({send, Q_Channel, Msg}, State) when is_binary(Msg) ->
	% TODO: Don't want to use unquote
	Channel = mochiweb_util:unquote(Q_Channel),
	% Make Msg tuple
	IdMsg = {State#state.id, Msg},
	% get Pids logged in to this channel
	{Pids, RR_List} = sort_channel_subscribers([ P || {_Channel, P} <- ets:lookup(State#state.channel2pid, Channel) ], {[], null}),
	% send Msg to them all
	M = {router_msg, IdMsg},
	[ Pid ! M || Pid <- Pids ],
	case RR_List of
		null ->
			ok;
		[H | T] ->
			% Send to front of RR and put it at the back
			H ! M,
			ets:delete_object(State#state.channel2pid, {Channel, RR_List}),
			ets:insert(State#state.channel2pid, {Channel, lists:append(T, [H])})
	end,
	% add msg to the channel log
	case ets:lookup(State#state.channels, Channel) of
		[] ->
			ets:insert(State#state.channels, {Channel, #channel{log=[IdMsg], last_accessed=erlang:now()}});
		[{_Channel, Channel_State}] ->
			ets:insert(State#state.channels, {Channel, Channel_State#channel{log = lists:sublist([IdMsg | Channel_State#channel.log], ?MAXLOGSIZE), last_accessed=erlang:now()}})
	end,
	{noreply, State#state{id=State#state.id + 1}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    case Info of
        {'EXIT', Pid, _Why} ->
            % force logout:
            handle_call({logout, Pid}, blah, State);
        Wtf ->
            io:format("Caught unhandled message: ~w\n", [Wtf])
    end,
    {noreply, State}.

terminate(_Reason, State) ->
	ets:tab2file(State#state.channels, filename:absname("") ++ "/" ++ ?CHANNELSTATEFILENAME),
    	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
find_rr_list([]) ->
	false;
find_rr_list([H | _]) when is_list(H) ->
	H;
find_rr_list([_ | T]) ->
	find_rr_list(T).
	
find_and_delete_pids(Table, {Channel, Pid_List}, Pid) when is_list(Pid_List) ->
	% Round Robin channel
	ets:delete_object(Table, {Channel, Pid_List}),
	% We may have duplicates, so don't just use lists:delete
	X = lists:filter(fun(Elem) -> Elem /= Pid end, Pid_List),
	case X of
		[] ->
			% No more RR subscribers, don't recreate the (empty) list
			ok;
		_ ->
			ets:insert(Table, {Channel, X})
	end.
	
sort_channel_subscribers([], Acc) ->
	Acc;
sort_channel_subscribers([H | T], {Pids, RR_List}) when is_pid(H) ->
	sort_channel_subscribers(T, {[H | Pids], RR_List});
sort_channel_subscribers([H | T], {Pids, null}) when is_list(H) ->
	sort_channel_subscribers(T, {Pids, H});
sort_channel_subscribers([H | T], {Pids, _}) when is_list(H) ->
	error_logger:error_msg("A channel had two round-robin lists...That's not meant to happen. Fix your code~n"),
	sort_channel_subscribers(T, {Pids, H}).
