-module(estd_cast_tests).

-include_lib("eunit/include/eunit.hrl").

to_ip_test() ->
    ?assertEqual(estd_cast:to_ip({1, 1, 1, 1}), {1, 1, 1, 1}),
    ?assertEqual(estd_cast:to_ip("1.1.1.1"), {1, 1, 1, 1}),
    ?assertEqual(estd_cast:to_ip(<<"1.1.1.1">>), {1, 1, 1, 1}),
    ?assertEqual(estd_cast:to_ip("::1"), {0, 0, 0, 0, 0, 0, 0, 1}).

to_bin_test() ->
    ?assertEqual(estd_cast:to_bin(<<"test">>), <<"test">>),
    ?assertEqual(estd_cast:to_bin("test"), <<"test">>),
    ?assertEqual(estd_cast:to_bin(test), <<"test">>),
    ?assertEqual(estd_cast:to_bin([$t, $e, $s, $t]), <<"test">>).

to_str_test() ->
    ?assertEqual(estd_cast:to_str("test"), "test"),
    ?assertEqual(estd_cast:to_str(<<"test">>), "test"),
    ?assertEqual(estd_cast:to_str(9999), "9999"),
    ?assertEqual(estd_cast:to_str([]), ""),
    ?assertEqual(estd_cast:to_str(test), "test").

to_int_test() ->
    ?assertEqual(estd_cast:to_int("9999"), 9999),
    ?assertEqual(estd_cast:to_int(<<"9999">>), 9999),
    ?assertEqual(estd_cast:to_int(9999), 9999).

to_atom_test() ->
    ?assertEqual(estd_cast:to_atom(<<"test">>), test),
    ?assertEqual(estd_cast:to_atom("test"), test),
    ?assertEqual(estd_cast:to_atom(test), test).
