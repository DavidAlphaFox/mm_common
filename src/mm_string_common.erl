-module(mm_string_common).
-export([to_hex/1]).


to_hex(Bin) when erlang:is_binary(Bin)->
	L0 = binary_to_list(Bin),
	L1 = lists:flatten(list_to_hex(L0)),
	erlang:list_to_binary(L1);
to_hex(List) ->
	lists:flatten(list_to_hex(List)).

list_to_hex(L) ->
	lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
	[hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
	$0 + N;
hex(N) when N >= 10, N < 16 ->
	$a + (N-10).

