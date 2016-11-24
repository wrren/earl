-module( earl_slack_router ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-behaviour( gen_server ).
%% gen_server callbacks
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2] ).
%% public api
-export( [start_link/1] ).

%% Interval between reconnect attempts
-define( RECONNECT_INTERVAL, 10000 ).

-type token() :: string().

%%
%%	@doc Start the named slack router process
%%
-spec start_link( token() ) -> { ok, pid() } | { error, term() }.
start_link( Token ) ->
	gen_server:start_link( { local, ?MODULE }, ?MODULE, Token, [] ).

%%
%%	gen_server callbacks
%%
-record( state, { token :: token(), connection = undefined :: undefined | { reference(), pid() } } ).

init( Token ) ->
	gen_server:cast( self(), connect ),
	{ ok, #state{ token = Token } }.

handle_call( _Call, _From, State ) -> 
	{ reply, ok, State }.

handle_cast( connect, State = #state{ token = Token, connection = undefined } ) ->
	case sr:connect( want:binary( Token ) ) of
		{ ok, Pid } ->
			{ noreply, State#state{ connection = { erlang:monitor( process, Pid ), Pid } } };
		_ ->
			erlang:start_timer( ?RECONNECT_INTERVAL, self(), connect ),
			{ noreply, State }
	end;

handle_cast( _Cast, State ) ->
	{ noreply, State }.

handle_info( { timeout, _TimerRef, connect }, State = #state{ connection = undefined } ) ->
	gen_server:cast( self(), connect ),
	{ noreply, State };

handle_info( { 'DOWN', Ref, process, Pid, Reason }, State = #state{ connection = { Ref, Pid } } )  when Reason =/= normal ->
	gen_server:cast( self(), connect ),
	{ noreply, State = #state{ connection = undefined } };

handle_info( _Info, State ) ->
	{ noreply, State }.

code_change( _OldVersion, State, _Extra ) ->
	{ ok, State }.

terminate( _Reason, _State ) ->
	ok.