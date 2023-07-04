%% Modulo dht_datastore
-module(dht_datastore).

% Inclusione della definizione del record
-include("dht_datastore.hrl").

% Esportazione funzioni
-export([
  init/0,
  store/3,
  lookup/2
]).

%% Inizializzazione del datastore del nodo
init() ->
  #dht_datastore{
    data = dict:new()
  }.

%% Salvataggio di un valore nel datastore del nodo e aggiornamento datastore
store(Datastore, Key, Value) ->
  UpdatedData = dict:store(Key, Value, Datastore#dht_datastore.data),
  #dht_datastore{
    data = UpdatedData
  }.

%% Ricerca di un valore nel datastore di un nodo e gestione degli errori
lookup(Datastore, Key) ->
  case dict:find(Key, Datastore#dht_datastore.data) of
    {ok, Value} ->
      {ok, Value};
    error ->
      error
  end.