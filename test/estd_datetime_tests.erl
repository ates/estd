-module(estd_datetime_tests).

-include_lib("eunit/include/eunit.hrl").

timestamp_test() ->
    Timestamp = {1354,121559,348583},
    ?assertEqual(estd_datetime:timestamp(sec, Timestamp), 1354121559),
    ?assertEqual(estd_datetime:timestamp(msec, Timestamp), 1354121559348). 
