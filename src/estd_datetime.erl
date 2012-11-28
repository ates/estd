-module(estd_datetime).

-export([midnight/0, uptime/0, timestamp/1, timestamp/2]).

%% @doc Tell how many seconds left till midnight.
-spec midnight() -> non_neg_integer().
midnight() ->
    {_, LocalTime} = erlang:localtime(),
    86400 - calendar:time_to_seconds(LocalTime).

%% @doc Tell how long the system has been running (uptime).
-spec uptime() -> {non_neg_integer(), calendar:time()}.
uptime() ->
    {T, _} = erlang:statistics(wall_clock),
    calendar:seconds_to_daystime(erlang:trunc(T / 1000)).

%% @doc Returns the elapsed time since
%% 00:00 GMT, January 1, 1970 in seconds or milliseconds.
-spec timestamp(sec | msec) -> non_neg_integer().
timestamp(Measure) ->
    timestamp(Measure, os:timestamp()).

%% @doc Converts the timestamp to seconds or milliseconds.
-spec timestamp(sec | msec, erlang:timestamp()) -> non_neg_integer().
timestamp(sec, {MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs;
timestamp(msec, {MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000 + (MicroSecs div 1000).
