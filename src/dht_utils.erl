-module(dht_utils).
-export([distance/2]).

distance(Id1, Id2) when is_integer(Id1), is_integer(Id2) ->
  XorResult = Id1 bxor Id2,
  io:format("XorResult:: ~p~n", [XorResult]),
  BinaryResult = integer_to_binary(XorResult, 2),
  ListResult = binary_to_list(BinaryResult),
  length(ListResult).
