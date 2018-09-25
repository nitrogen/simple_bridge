-module(test_replacement).
-compile(export_all).

val() ->
    <<"this-_is_my_test_stringi-I_hope_it-works">>.

listmap() ->
    B = binary_to_list(val()),
    V = lists:map(fun($_) -> $-; (C) -> C end, B),
    list_to_binary(V).


binroll() ->
    B = val(),
    binroll(B, 0).

binroll(B, Position) when Position >= byte_size(B) ->
    B;
binroll(B, Position) ->
    case binary:at(B, Position) of
        $_ ->
            {H, <<_,Rest/binary>>} = erlang:split_binary(B, Position),
            New = <<H/binary,$-,Rest/binary>>,
            binroll(New, Position+1);
        _ ->
            binroll(B, Position+1)
    end.
    
time_listmap(N) ->
    {Time, _} = timer:tc(fun() ->
        [listmap() || _ <- lists:seq(1,N)]
    end),
    Time.

time_binroll(N) ->
    {Time, _} = timer:tc(fun() ->
        [binroll() || _ <- lists:seq(1,N)]
    end),
    Time.
