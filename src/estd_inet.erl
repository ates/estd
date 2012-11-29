-module(estd_inet).

-export([is_macaddr/1]).

-define(MAC_REGEXP, "^([0-9a-f]{2}([:-]|$)){6}|^([0-9a-f]{6}([:-]|$)){2}|^([0-9a-f]{4}(\.|$)){3}$").

%% @doc Checks is HW address is valid.
%% Accepts the HW address in the following formats:
%% 08002b:010203, 08002b-010203, 0800.2b01.0203
%% 08-00-2b-01-02-03, 08:00:2b:01:02:03
-spec is_macaddr(string()) -> boolean().
is_macaddr(Address) ->
    case re:run(Address, ?MAC_REGEXP, [{capture, none}, caseless]) of
        match -> true;
        _ -> false
    end.
