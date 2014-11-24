-module(estd_inet).

-export([is_macaddr/1]).
-export([aton/1]).
-export([ntoa/1]).
-export([proto/1]).

-define(MAC_FMT1, "^([0-9a-f]{2}([:-]|$)){6}").
-define(MAC_FMT2, "^([0-9a-f]{6}([:-]|$)){2}").
-define(MAC_FMT3, "^([0-9a-f]{4}(\.|$)){3}$").
-define(MAC_REGEXP, string:join([?MAC_FMT1, ?MAC_FMT2, ?MAC_FMT3], "|")).

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

%% @doc Return the numeric value of an IP address.
-spec aton(inet:ip_address()) -> non_neg_integer().
aton({A, B, C, D}) ->
    (A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D.

%% @doc Return the IP address from a numeric value.
%% Limitation: support only IPv4
ntoa(IP) when IP =< 4294967295 ->
    {(IP div 16777216) rem 256, (IP div 65536) rem 256, (IP div 256) rem 256, IP rem 256}.

%% @doc Returns the type of IP address.
-spec proto(inet:ip_address()) -> inet:address_family().
proto(Address) when tuple_size(Address) == 4 -> inet;
proto(Address) when tuple_size(Address) == 8 -> inet6.
