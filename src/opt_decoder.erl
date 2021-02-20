-module(opt_decoder).

-include("sql_alter.hrl").

-export([decode/1]).

decode(Input) ->
    decode(skip_prefix_blank(Input), undefined, []).

decode(<<>>, undefined, Result) -> {ok, Result};
decode(<<>>, FirstOpt, Result) -> {ok, [FirstOpt | Result]};
decode(<<"--", _/binary>> = Binary, undefined, _Result) ->
    {false, {no_first_opt, Binary}};
decode(<<"--", Rest/binary>>, FirstOpt, Result) ->
    case get_key_value(Rest) of
        {ok, KeyToken, ValueToken, Rest2} ->
            Key = erlang:binary_to_atom(KeyToken, utf8),
            Sub2 = maps:put(Key, ValueToken, FirstOpt#rec_opt.sub),
            FirstOpt2 = FirstOpt#rec_opt{sub = Sub2},
            decode(skip_prefix_blank(Rest2), FirstOpt2, Result);
        {false, Reason} ->
            {false, Reason}
    end;
decode(<<"-", Rest/binary>>, undefined, Result) ->
    case get_key_value(Rest) of
        {ok, KeyToken, ValueToken, Rest2} ->
            Key = erlang:binary_to_atom(KeyToken, utf8),
            Opt = #rec_opt{key = Key, val = ValueToken, sub = #{}},
            decode(skip_prefix_blank(Rest2), Opt, Result);
        {false, Reason} ->
            {false, Reason}
    end;
decode(<<"-", Rest/binary>>, FirstOpt, Result) ->
    case get_key_value(Rest) of
        {ok, KeyToken, ValueToken, Rest2} ->
            Key = erlang:binary_to_atom(KeyToken, utf8),
            Opt = #rec_opt{key = Key, val = ValueToken, sub = #{}},
            decode(skip_prefix_blank(Rest2), Opt, [FirstOpt | Result]);
        {false, Reason} ->
            {false, Reason}
    end.


get_key_value(<<>>) -> {false, {empty, <<>>}};
get_key_value(Binary) ->
    case get_token(skip_prefix_blank(Binary)) of
        {ok, KeyToken, Rest} ->
            case skip_prefix_blank(Rest) of
                <<"=", Rest1/binary>> ->
                    case get_token(skip_prefix_blank(Rest1)) of
                        {ok, ValueToken, Rest2} ->
                            {ok, KeyToken, ValueToken, Rest2};
                        false ->
                            {false, {no_opt_value, Rest1}}
                    end;
                _ ->
                    {false, {no_equal_symbol, Rest}}
            end;
        false ->
            {false, {no_opt_key, Binary}}
    end.

get_token(<<>>) -> false;
get_token(Binary) ->
    get_token_loop(Binary, 0, []).
get_token_loop(<<>>, 0, Result) ->
    {ok, iolist_to_binary(lists:reverse(Result)), <<>>};
get_token_loop(<<>>, _, _Result) ->
    false;
get_token_loop(<<$":8, Rest/binary>>, 1, Result) ->
    {ok, iolist_to_binary(lists:reverse(Result)), Rest};
get_token_loop(<<$":8, Rest/binary>>, 0, Result) ->
    get_token_loop(Rest, 1, Result);
get_token_loop(<<$=:8, _/binary>> = Binary, 0, Result) ->
    {ok, iolist_to_binary(lists:reverse(Result)), Binary};
get_token_loop(<<$ :8, Rest/binary>>, 0, Result) ->
    {ok, iolist_to_binary(lists:reverse(Result)), Rest};
get_token_loop(<<C:8, Rest/binary>>, Layer, Result) ->
    get_token_loop(Rest, Layer, [C | Result]).

skip_prefix_blank(<<>>) -> <<>>;
skip_prefix_blank(<<$ :8, Rest/binary>>) ->
    skip_prefix_blank(Rest);
skip_prefix_blank(Binary) ->
    Binary.