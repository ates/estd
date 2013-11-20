-module(estd_iconv).

-export([utf8_to_win/1, win_to_utf8/1]).

utf8_to_win(Str) ->
    [to_win(C) || C <- Str].

win_to_utf8(Str) ->
    [to_utf8(C) || C <- Str].

%% Internal functions
to_win(Char) ->
    if
        Char == 1025 -> Char - 857; %% Ё
        Char == 1105 -> Char - 921; %% ё
        Char >= 1040 -> Char - 848; %% А..Яа..я
        true -> Char
    end.

to_utf8(Char) ->
    if
        Char == 168 -> Char + 857; %% Ё
        Char == 184 -> Char + 921; %% ё
        Char >= 192 -> Char + 848; %% А..Яа..я
        true -> Char
    end.
