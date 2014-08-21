-module(estd_datetime).

-export([midnight/0, uptime/0, timestamp/1, timestamp/2]).
-export([datetime_to_binary/0, datetime_to_binary/1]).

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

%% @doc Converts the datetime to binary
-spec datetime_to_binary() -> binary().
datetime_to_binary() ->
    datetime_to_binary(calendar:local_time()).

-spec datetime_to_binary(calendar:datetime()) -> binary().
datetime_to_binary({{YYYY, MM, D}, {H, M, S}}) ->
    <<(int_fmt(YYYY))/binary, "-", (int_fmt(MM))/binary,
    "-", (int_fmt(D))/binary, " ", (int_fmt(H))/binary,
    ":", (int_fmt(M))/binary, ":", (int_fmt(S))/binary>>.

%% Internal functions
-spec int_fmt(non_neg_integer()) -> binary().
int_fmt(N) when N < 10 ->
    <<$0, (integer_to_binary(N))/binary>>;
int_fmt(N) ->
    integer_to_binary(N).
