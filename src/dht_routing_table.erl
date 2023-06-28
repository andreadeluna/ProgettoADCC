-module(dht_routing_table).
-include("dht_routing_table.hrl").

-export([init/1]).

-define(BUCKET_SIZE, 20).

init(NodeId) ->
  Buckets = lists:foldl(fun(_, Acc) -> [dict:new() | Acc] end, [], lists:seq(1, 160 div ?BUCKET_SIZE)),
  #dht_routing_table{node_id = NodeId, buckets = Buckets}.

add_node(#dht_routing_table{node_id = NodeId, buckets = Buckets} = Table, NodeInfo) ->
  BucketIndex = get_bucket_index(Table, NodeId),
  Bucket = lists:nth(BucketIndex, Buckets),
  UpdatedBucket = replace_node(Bucket, NodeInfo),
  UpdatedBuckets = replace_element(BucketIndex, UpdatedBucket, Buckets),
  Table#dht_routing_table{buckets = UpdatedBuckets}.

remove_node(NodeId, RoutingTable) ->
  NewBucket = remove_node_from_bucket(NodeId, RoutingTable#dht_routing_table.buckets),
  RoutingTable#dht_routing_table{buckets = NewBucket}.

remove_node_from_bucket(_, []) ->
  [];
remove_node_from_bucket(NodeId, [Bucket | Rest]) ->
  NewBucket = lists:filter(fun(Node) -> Node#node_info.id /= NodeId end, Bucket),
  [NewBucket | remove_node_from_bucket(NodeId, Rest)].

replace_node(Bucket, NodeInfo) ->
  case dict:find(NodeInfo#node_info.id, Bucket) of
    {ok, _} ->
      Bucket;
    error ->
      case dict:size(Bucket) >= ?BUCKET_SIZE of
        true ->
          % Il bucket è pieno, sostituisci il nodo con lo stesso ID
          SmallestId = find_smallest_id(Bucket),
          UpdatedBucket = dict:erase(SmallestId, Bucket),
          dict:store(NodeInfo#node_info.id, NodeInfo, UpdatedBucket);
        false ->
          % Il bucket non è pieno, aggiungi il nodo
          dict:store(NodeInfo#node_info.id, NodeInfo, Bucket)
      end
  end.

find_smallest_id(Bucket) ->
  Keys = lists:map(fun({Key, _}) -> Key end, dict:to_list(Bucket)),
  lists:min(Keys).

get_node_info(Table) ->
  {Table#dht_routing_table.node_id, self(), get_udp_port()}.

get_node_info_by_id(Table, NodeId) ->
  BucketIndex = get_bucket_index(Table, NodeId),
  Bucket = lists:nth(BucketIndex, Table#dht_routing_table.buckets),
  case dict:find(NodeId, Bucket) of
    {ok, NodeInfo} ->
      {NodeInfo#node_info.ip, NodeInfo#node_info.port};
    error ->
      {get_own_ip(), get_udp_port()}
  end.