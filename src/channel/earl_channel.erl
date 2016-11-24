-module( earl_channel ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-behaviour( gen_server ).
%% gen_server callbacks
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2] ).
%% public api
-export( [start_link/2, handle_message/2] ).

%%
%%	@doc Start a new channel process and associate it with the given Channel ID
%%
start_link( Channel, ChannelID ) ->
	gen_server:start_link( ?MODULE, { Channel, ChannelID }, [] ).

%%
%%	@doc Have this channel handle a message
%%
handle_message( Pid, Message ) ->
	gen_server:cast( Pid, { handle_message, Message } ).

%%
%%	gen_server callbacks
%%
-record( state, { channel :: pid(), channel_id :: any() } ).

init( { Channel, ChannelID } ) ->
	earl_channel_directory:register( ChannelID, self() ),
	{ ok, #state{ channel = Channel, channel_id = ChannelID } }.

handle_call( _Call, _From, State ) ->
	{ reply, ok, State }.

handle_cast( { handle_message, Message }, State = #state{ channel = Channel } ) ->
	