-module( earl_channel_directory ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-behaviour( gen_server ).
%% gen_server callbacks
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2] ).
%% public api
-export( [start_link/0, register/2, find/1] ).

-define( ETS_TABLE_NAME, channel_directory ).

%%
%%	@doc Start a named channel directory process
%%
start_link() ->
	gen_server:start_link( { local, ?MODULE }, ?MODULE, [], [] ).

%%
%%	@doc Register a channel process with the directory and associate it with the given Channel ID
%%
register( ChannelID, Pid ) ->
	gen_server:call( ?MODULE, { register, ChannelID, Pid } ).

%%
%%	@doc Find the channel with the given Channel ID. If no such channel exists, a new one will
%%	be created.
%%
find( ChannelID ) ->
	case ets:lookup( ?ETS_TABLE_NAME, ChannelID ) of
		[] ->
			{ ok, Pid } = earl_channel_sup:start_child( ChannelID ),
			Pid;
		[{ ChannelID, _Reference, Pid }] ->
			Pid
	end.

%%
%%	gen_server callbacks
%%
-record( state, { table :: ets:tab() } ).

init( _ ) ->
	{ ok, #state{ table = ets:new( ?ETS_TABLE_NAME, [set, protected, named_table, { read_concurrency, true }] ) } }.

handle_call( { register, ChannelID, Pid }, _From, State = #state{ table = Table } ) ->
	ets:insert( Table, { ChannelID, erlang:monitor( process, Pid ), Pid } ),
	{ reply, Pid, State };

handle_call( _Call, _From, State ) ->
	{ reply, ok, State }.

handle_cast( _Cast, State ) ->
	{ noreply, State }.

handle_info( { 'DOWN', Ref, process, Pid, _ }, State = #state{ table = Table } ) ->
	ChannelIDs = ets:match( Table, { '$1', Ref, _ } ),
	[ets:delete( Table, ChannelID ) || ChannelID <- ChannelIDs],
	{ noreply, State };

handle_info( _Info, State ) ->
	{ noreply, State }.

code_change( _OldVersion, State, _Extra ) ->
	{ ok, State }.

terminate( _Reason, _State ) ->
	ok.