-module(estd_inet).

-export([is_macaddr/1]).
-export([aton/1]).
-export([ntoa/1]).
-export([proto/1]).
-export([broadcast/2]).
-export([range/2]).
-export([range2list/2]).
-export([in_range/2]).

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

%% @doc Return the broadcast IP of the network.
-spec broadcast(IP :: inet:ip_address(), Mask :: 0..32 | inet:ip_address()) -> inet:ip_address().
broadcast(Address, NetMask) ->
    {IP, Mask} = parse_address(Address, NetMask),
    ntoa((IP band Mask) bor (bnot Mask band 16#ffffffff)).

%% @doc Return the range of addresses in the network.
-spec range(IP :: inet:ip_address(), Mask :: 0..32 | inet:ip_address()) -> {inet:ip_address(), inet:ip_address()}.
range(Address, NetMask) ->
    {IP, Mask} = parse_address(Address, NetMask),
    Broadcast = aton(broadcast(Address, NetMask)),
    case Mask of
        4294967294 -> % 31
            {ntoa(IP band Mask), ntoa(Broadcast)};
        4294967295 -> % 32
            {ntoa(IP), ntoa(IP)};
        _ ->
            {ntoa((IP band Mask) + 1), ntoa(Broadcast - 1)}
    end.

%% @doc Return the list of an IP addresses in the network
-spec range2list(IP :: inet:ip_address(), Mask :: 0..32 | inet:ip_address()) -> [inet:ip_address()].
range2list(Address, NetMask) ->
    {From, To} = range(Address, NetMask),
    [ntoa(I) || I <- lists:seq(aton(From), aton(To))].

-spec in_range(IP :: inet:ip_address(), {Network :: inet:ip_address(), Mask :: 0..32 | inet:ip_address()}) -> boolean().
in_range(IP, {Network, Mask}) ->
    {Network0, Mask0} = parse_address(Network, Mask),
    (aton(IP) band Mask0) == (Network0 band Mask0).


%% Internal functions
-spec parse_address(IP :: inet:ip_address(), Mask :: 0..32 | inet:ip_address()) -> {0..4294967295, 0..4294967295}.
parse_address(IP, Mask) ->
    NetMask = case Mask of
        N when is_tuple(N) andalso tuple_size(N) == 4 ->
            aton(Mask);
        N when N >= 0 andalso N =< 32 ->
            (16#ffffffff bsr (32 - Mask)) bsl (32 - Mask)
    end,
    {aton(IP), NetMask}.
