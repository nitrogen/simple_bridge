
-module(simple_bridge_util).
-export([atomize_header/1]).


%% converts a Header to a lower-case, underscored version
%% ie. "X-Forwarded-For" -> x_forwarded_for
atomize_header(Header) when is_binary(Header) ->
	atomize_header(binary_to_list(Header));
atomize_header(Header) when is_atom(Header) ->
    atomize_header(atom_to_list(Header));
atomize_header(Header) when is_list(Header) ->
    LowerUnderscore = fun(H) -> 
        if
            H >= 65 andalso H =< 90 ->
                H + 32; % Convert "A" to "a" by adding 32 to its ASCII val
            H == 45 ->
                95; %% convert "-" to "_"
            true -> H
        end
    end,
    list_to_atom(lists:map(LowerUnderscore,Header)).
