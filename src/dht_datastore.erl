-module(dht_datastore).
-include("dht_datastore.hrl").

-export([init/0, store/3, lookup/2]).

init() ->
  #dht_datastore{data = dict:new()}.

store(Datastore, Key, Value) ->
  UpdatedData = dict:store(Key, Value, Datastore#dht_datastore.data),
  #dht_datastore{data = UpdatedData}.

lookup(Datastore, Key) ->
  case dict:find(Key, Datastore#dht_datastore.data) of
    {ok, Value} ->
      {ok, Value};
    error ->
      error
  end.
