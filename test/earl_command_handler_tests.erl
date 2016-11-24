-module( earl_command_handler_tests ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "eunit/include/eunit.hrl" ).

destructure_test() ->
	?assertMatch( { ok, "list", [] }, earl_command_handler:destructure( <<"list">> ).