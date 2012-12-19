-module(estd_crc16_tests).

-include_lib("eunit/include/eunit.hrl").

calc_test() ->
    ?assertEqual(estd_crc16:calc([1,2,3]), 24929),
    ?assertEqual(estd_crc16:calc(<<1,2,3>>), 24929).

is_valid_test() ->
    ?assertEqual(estd_crc16:is_valid(<<1,2,3>>, 24929), true).
