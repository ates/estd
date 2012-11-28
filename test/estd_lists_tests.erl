-module(estd_lists_tests).

-include_lib("eunit/include/eunit.hrl").

merge_test() ->
    List1 = [{a, 10}, {b, 20}, {c, 0}],
    List2 = [{a, 15}, {b, 25}, {d, 30}],
    ?assertEqual(estd_lists:merge(List1, List2), [{d,30},{b,25},{a,15},{c,0}]). 
