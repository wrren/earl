-module( earl_sup ).
-behaviour( supervisor ).
%% public API
-export( [start_link/0] ).
%% supervisor callbacks
-export( [init/1] ).

start_link() ->
    supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

init( [] ) ->
    { ok, { { one_for_all, 1, 1 }, [] } }.
