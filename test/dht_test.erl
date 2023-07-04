%% Modulo dht_test
-module(dht_test).

% Esportazione funzioni
-export([
  create_nodes/1,
  connect_nodes/1,
  find_node/2,
  lookup_node/2
]).

%% Calcolo del tempo di creazione automatizzata di N nodi
create_nodes(N) ->
  StartTime = os:timestamp(),
  Risposta = dht_node:create_nodes(N),
  io:format("~p~n", [Risposta]),
  EndTime = os:timestamp(),
  ExecutionTime = timer:now_diff(EndTime, StartTime) / 1000,
  io:format("~nNodi creati in: ~p millisecondi~n", [ExecutionTime]),
  ExecutionTime.

%% Calcolo del tempo di connessione automatizzata di tutti i nodi creati
connect_nodes(Nodes) ->
  StartTime = os:timestamp(),
  Risposta = dht_node:connect_nodes(Nodes),
  io:format("~p~n", [Risposta]),
  EndTime = os:timestamp(),
  ExecutionTime = timer:now_diff(EndTime, StartTime) / 1000,
  io:format("~nConnessione efettuata in: ~p millisecondi~n", [ExecutionTime]),
  ExecutionTime.

%% Calcolo del tempo di esecuzione della funzione di ricerca dei nodi più vicini all'ID cercato
find_node(Node, TargetId) ->
  StartTime = os:timestamp(),
  Risposta = dht_node:find_node(Node, TargetId),
  io:format("~p~n", [Risposta]),
  EndTime = os:timestamp(),
  ExecutionTime = timer:now_diff(EndTime, StartTime) / 1000,
  io:format("~nTempo esecuzione: ~p millisecondi~n", [ExecutionTime]),
  ExecutionTime.

%% Calcolo del tempo di ricerca di un nodo all'interno della rete
lookup_node(Node, TargetId) ->
  StartTime = os:timestamp(),
  Risposta = dht_node:lookup_node(Node, TargetId),
  io:format("~p~n", [Risposta]),
  EndTime = os:timestamp(),
  ExecutionTime = timer:now_diff(EndTime, StartTime) / 1000,
  io:format("~nTempo esecuzione: ~p millisecondi~n", [ExecutionTime]),
  ExecutionTime.