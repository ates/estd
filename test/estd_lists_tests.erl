-module(estd_lists_tests).

-include_lib("eunit/include/eunit.hrl").

merge_test() ->
    List1 = [{a, 10}, {b, 20}, {c, 0}],
    List2 = [{a, 15}, {b, 25}, {d, 30}],
    ?assertEqual(estd_lists:merge(List1, List2), [{d,30},{b,25},{a,15},{c,0}]). 

to_hex_test() ->
    ?assertEqual(estd_lists:to_hex(111), "6f").

to_hex_string_test() ->
    ?assertEqual(estd_lists:to_hex_string(<<1,2,15>>), "01020f"),
    ?assertEqual(estd_lists:to_hex_string([1,2,15]), "01020f").

get_value_test() ->
    ?assertEqual(estd_lists:get_value(key, [{key, 1}]), 1),
    ?assertEqual(estd_lists:get_value(key, [{foo, 1}]), undefined),
    ?assertEqual(estd_lists:get_value(key, [{foo, 1}], error), error).
