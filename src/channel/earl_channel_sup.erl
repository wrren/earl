-module( earl_channel_sup ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-behaviour( supervisor ).
%% supervisor callbacks
-export( [init/1] ).
%% public api
-export( [start_link/0, start_channel/1] ).

start_link() ->
    supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

init( [] ) ->
    { ok, { { simple_one_for_one, 1, 1 }, [
		#{	id		=> channel,
			start 	=> { earl_channel, start_link, [] },
			restart	=> permanent,
			type 	=> worker
		}
	] } }.

start_channel( ChannelID ) ->
	supervisor:start_child( ?MODULE, [ChannelID] ).