-module(estd_lists).

-export([merge/2]).

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
