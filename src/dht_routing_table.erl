%% Modulo dht_routing_table
-module(dht_routing_table).

% Inclusione della definizione dei record
-include("dht_routing_table.hrl").

% Esportazione funzioni
-export([
  init/1,
  remove_node/2,
  get_node_info/1,
  get_node_info_by_id/2,
  get_all_nodes/1,
  find_node/2,
  find_closest_nodes/2,
  add_node/2
]).

% Definizione dimensione bucket della routing table
-define(BUCKET_SIZE, 20).

%% Inizializzazione della routing table del nodo
init(NodeId) ->
  Buckets = lists:foldl(fun(_, Acc) -> [dict:new() | Acc] end, [], lists:seq(1, 20 div ?BUCKET_SIZE)),
  #dht_routing_table{
    node_id = NodeId,
    buckets = Buckets
  }.

%% Aggiunta del nodo alla routing table
add_node(#dht_routing_table{node_id = NodeId, buckets = Buckets} = Table, NodeInfo) ->
  BucketIndex = get_bucket_index(Table, NodeId),
  Bucket = lists:nth(BucketIndex, Buckets),
  UpdatedBucket = replace_node(Bucket, NodeInfo),
  UpdatedBuckets = replace_element(BucketIndex, UpdatedBucket, Buckets),
  Table#dht_routing_table{buckets = UpdatedBuckets}.

%% Rimozione del nodo dalla routing table
remove_node(NodeId, RoutingTable) ->
  NewBucket = remove_node_from_bucket(NodeId, RoutingTable#dht_routing_table.buckets),
  RoutingTable#dht_routing_table{buckets = NewBucket}.

%% Rimozione ciclica del nodo dal bucket
remove_node_from_bucket(_, []) ->
  [];
%% Ricerca del nodo da eliminare
remove_node_from_bucket(NodeId, Bucket) ->
  UpdatedBucket = lists:filter(
    fun(Dict) ->
      case dict:find(NodeId, Dict) of
        error -> true;
        {ok, Node} when Node#node_info.id == NodeId ->
          false;
        _ -> true
      end
    end, Bucket),
  UpdatedBucket.

%% Sostituzione del nodo nella routing table
replace_node(Bucket, NodeInfo) ->
  case dict:find(NodeInfo#node_info.id, Bucket) of
    {ok, _} ->
      io:format("~nBucket ok~n"),
      Bucket;
    error ->
      case dict:size(Bucket) >= ?BUCKET_SIZE of
        true ->
          io:format("~nBucket pieno~n"),
          % Se il bucket è pieno, il nodo viene sostituito con lo stesso ID
          SmallestId = find_smallest_id(Bucket),
          io:format("~nSmallestID: ~p~n", [SmallestId]),
          UpdatedBucket = dict:erase(SmallestId, Bucket),
          dict:store(NodeInfo#node_info.id, NodeInfo, UpdatedBucket);
        false ->
          io:format("~nBucket vuoto~n"),
          % Se il bucket non è pieno, il nodo viene aggiunto
          dict:store(NodeInfo#node_info.id, NodeInfo, Bucket)
      end
  end.

%% Ricerca dell'id del nodo all'interno del bucket
find_smallest_id(Bucket) ->
  Keys = lists:map(fun({Key, _}) -> Key end, dict:to_list(Bucket)),
  lists:min(Keys).

%% Ricerca delle informazioni di un nodo
get_node_info(Table) ->
  {Table#dht_routing_table.node_id,
    element(1,get_node_info_by_id(Table,Table#dht_routing_table.node_id))}.

%% Ricerca delle informazioni di un nodo tramite ID
get_node_info_by_id(Table, NodeId) ->
  BucketIndex = get_bucket_index(Table, NodeId),
  Bucket = lists:nth(BucketIndex, Table#dht_routing_table.buckets),
  case dict:find(NodeId, Bucket) of
    {ok, NodeInfo} ->
      %% Se il nodo viene trovato viene restituito l'IP e la Porta
      {NodeInfo#node_info.pid};
    error ->
      %% Se il nodo non viene trovato viene segnalato un errore
      {not_found}
  end.

%% Ricerca di un nodo all'interno della routing table di un altro nodo
find_node(Table, TargetId) ->
  BucketIndex = get_bucket_index(Table, TargetId),
  Bucket = lists:nth(BucketIndex, Table#dht_routing_table.buckets),
  Nodes = dict:to_list(Bucket),
  lists:sort(fun({_, Node1}, {_, Node2}) -> dht_utils:distance(TargetId, Node1#node_info.id) < dht_utils:distance(TargetId, Node2#node_info.id) end, Nodes).

%% Ricerca dei nodi più vicini all'ID di un nodo ricercato
find_closest_nodes(Table, TargetId) ->
  AllNodes = get_all_nodes(Table),
  SortedNodes = lists:sort(fun({_, Node1}, {_, Node2}) ->
    %% Ricerca tramite XOR e ordinamento dei nodi trovati
    dht_utils:distance(TargetId, Node1#node_info.id) < dht_utils:distance(TargetId, Node2#node_info.id) end, AllNodes),
  lists:sublist(SortedNodes, 1, 3).

%% Ricerca dell'ID del bucket
get_bucket_index(Table, Id) ->
  Distance = dht_utils:distance(Table#dht_routing_table.node_id, Id),
  BucketSize = 20,
  Index = 20 - trunc(math:log2(Distance + 1)),
  Index div BucketSize + 1.

%% Restituzione di tutti i nodi all'interno di una routing table
get_all_nodes(Table) ->
  lists:flatten([dict:to_list(Bucket) || Bucket <- Table#dht_routing_table.buckets]).

%% Sostituzione ricorsiva dei nodi all'interno del bucket
replace_element(Index, NewElement, List) ->
  replace_element(Index, NewElement, List, []).

replace_element(_, _, [], Acc) ->
  lists:reverse(Acc);
replace_element(1, NewElement, [_|T], Acc) ->
  lists:reverse([NewElement|T] ++ Acc);
replace_element(Index, NewElement, [H|T], Acc) ->
  replace_element(Index - 1, NewElement, T, [H|Acc]).