-module(estd_cast).

-export([to_ip/1]).

-spec to_ip(IP :: tuple() | binary() | string()) -> inet:ip_address().
to_ip({A, B, C, D} = IP) when
    is_integer(A), A >= 0, A =< 255,
    is_integer(B), B >= 0, B =< 255,
    is_integer(C), C >= 0, C =< 255,
    is_integer(D), D >= 0, D =< 255 ->
    IP;
to_ip(IP) when is_binary(IP) ->
    to_ip(binary_to_list(IP));
to_ip(IP) when is_list(IP) ->
    {ok, Address} = inet_parse:address(IP),
    Address.
