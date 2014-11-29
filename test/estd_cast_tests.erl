-module(estd_cast_tests).

-include_lib("eunit/include/eunit.hrl").

to_ip_test() ->
    ?assertEqual(estd_cast:to_ip({1, 1, 1, 1}), {1, 1, 1, 1}),
    ?assertEqual(estd_cast:to_ip("1.1.1.1"), {1, 1, 1, 1}),
    ?assertEqual(estd_cast:to_ip(<<"1.1.1.1">>), {1, 1, 1, 1}),
    ?assertEqual(estd_cast:to_ip("::1"), {0, 0, 0, 0, 0, 0, 0, 1}).
