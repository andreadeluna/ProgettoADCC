%% Modulo dht_datastore
-module(dht_utils).

% Esportazione funzioni
-export([
  distance/2
]).

%% Calcolo della distanza XOR tra due nodi
distance(Id1, Id2) when is_integer(Id1), is_integer(Id2) ->
  XorResult = Id1 bxor Id2,
  BinaryResult = integer_to_binary(XorResult, 2),
  ListResult = binary_to_list(BinaryResult),
  length(ListResult).
