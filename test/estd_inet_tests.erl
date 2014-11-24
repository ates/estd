-module(estd_inet_tests).

-include_lib("eunit/include/eunit.hrl").

is_macaddr_test() ->
    ?assertEqual(estd_inet:is_macaddr("AB:CD:EF:00:11:22"), true),
    ?assertEqual(estd_inet:is_macaddr("AB-CD-EF-00-11-22"), true),
    ?assertEqual(estd_inet:is_macaddr("ab:cd:ef:00:11:22"), true),
    ?assertEqual(estd_inet:is_macaddr("ab-cd-ef-00-11-22"), true),
    ?assertEqual(estd_inet:is_macaddr("ab-cz-ef-00-110-22"), false),
    ?assertEqual(estd_inet:is_macaddr("08002b:010203"), true),
    ?assertEqual(estd_inet:is_macaddr("08002b-010203"), true),
    ?assertEqual(estd_inet:is_macaddr("0800.2b01.0203"), true).

aton_test() ->
    ?assertEqual(estd_inet:aton({192, 168, 1, 1}), 3232235777).

ntoa_test() ->
    ?assertEqual(3232235777, estd_inet:aton({192, 168, 1, 1})).

proto_test() ->
    ?assertEqual(estd_inet:proto({1, 1, 1, 1}), inet),
    ?assertEqual(estd_inet:proto({0, 0, 0, 0, 0, 0, 0, 1}), inet6).

broadcast_test() ->
    ?assertEqual(estd_inet:broadcast({1, 1, 1, 1}, 16), {1, 1, 255, 255}).

range_test() ->
    ?assertEqual(estd_inet:range({1, 1, 1, 1}, 16), {{1, 1, 0, 1}, {1, 1, 255, 254}}),
    ?assertEqual(estd_inet:range({1, 1, 1, 1}, 31), {{1, 1, 1, 0}, {1, 1, 1, 1}}),
    ?assertEqual(estd_inet:range({1, 1, 1, 1}, 32), {{1, 1, 1, 1}, {1, 1, 1, 1}}).
