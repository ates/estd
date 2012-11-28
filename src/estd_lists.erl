-module(estd_lists).

-export([merge/2]).

merge(List1, []) -> List1;
merge(List1, [{Key, _} = El | Tail]) ->
    NewList = case lists:keymember(Key, 1, List1) of
        true ->
            [El | lists:keydelete(Key, 1, List1)];
        false ->
            [El | List1]
    end,
    merge(NewList, Tail).
