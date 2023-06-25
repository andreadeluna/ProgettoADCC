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