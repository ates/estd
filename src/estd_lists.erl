-module(estd_lists).

-export([merge/2]).
-export([to_hex/1]).
-export([to_hex_string/1]).

%% @doc Returns the list formed by merging List1 and List2 with replacing
%% the values of even elements from List2 to List1, also, adds the new
%% elements from List2 to List1.
-spec merge([proplists:property()], [proplists:property()]) ->
    [proplists:property()].
merge(List1, []) -> List1;
merge(List1, [{Key, _} = El | Tail]) ->
    NewList = case lists:keymember(Key, 1, List1) of
        true ->
            [El | lists:keydelete(Key, 1, List1)];
        false ->
            [El | List1]
    end,
    merge(NewList, Tail).

%% @doc Converts number to hex
to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

%% @doc Converts list or binary to hex string
-spec to_hex_string(binary()) -> string().
to_hex_string(Binary) when is_binary(Binary) ->
    to_hex_string(binary_to_list(Binary));

to_hex_string([]) -> [];
to_hex_string([H | T]) ->
    to_hex(H) ++ to_hex_string(T).

%% Internal functions
hex(N) when N < 10 -> $0 + N;
hex(N) when N >= 10, N < 16 -> $a + (N - 10).
