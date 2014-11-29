-module(estd_cast).

-export([to_ip/1]).
-export([to_bin/1]).
-export([to_str/1]).
-export([to_int/1]).
-export([to_atom/1]).

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

-spec to_bin(Data :: binary() | list() | atom()) -> binary().
to_bin(Data) when is_binary(Data) ->
    Data;
to_bin(Data) when is_list(Data) ->
    list_to_binary(Data);
to_bin(Data) when is_atom(Data) ->
    atom_to_binary(Data, latin1).

to_str(Data) when is_binary(Data) ->
    binary_to_list(Data);
to_str([]) ->
    "";
to_str(Data) when is_list(Data) ->
    Data;
to_str(Data) when is_integer(Data) ->
    integer_to_list(Data);
to_str(Data) when is_atom(Data) ->
    atom_to_list(Data).

to_int(Data) when is_integer(Data) ->
    Data;
to_int(Data) when is_binary(Data) ->
    binary_to_integer(Data);
to_int(Data) when is_list(Data) ->
    list_to_integer(Data).

to_atom(Data) when is_atom(Data) ->
    Data;
to_atom(Data) when is_binary(Data) ->
    binary_to_atom(Data, latin1);
to_atom(Data) when is_integer(hd(Data)) ->
    list_to_atom(Data).
