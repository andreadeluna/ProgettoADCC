-module(dht_routing_table).
-include("dht_routing_table.hrl").

-export([
  init/1,
  remove_node/2,
  get_node_info/1,
  get_node_info_by_id/2,
  get_all_nodes/1,
  find_node/2,
  find_closest_nodes/2,
  add_node/2,
  get_own_ip/0,
  get_udp_port/0
]).

-define(BUCKET_SIZE, 20).

init(NodeId) ->
  Buckets = lists:foldl(fun(_, Acc) -> [dict:new() | Acc] end, [], lists:seq(1, 160 div ?BUCKET_SIZE)),
  #dht_routing_table{
    node_id = NodeId,
    buckets = Buckets
  }.

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
  {Table#dht_routing_table.node_id,
    element(1,get_node_info_by_id(Table,Table#dht_routing_table.node_id)),
    element(2,get_node_info_by_id(Table,Table#dht_routing_table.node_id))}.

get_node_info_by_id(Table, NodeId) ->
  BucketIndex = get_bucket_index(Table, NodeId),
  Bucket = lists:nth(BucketIndex, Table#dht_routing_table.buckets),
  case dict:find(NodeId, Bucket) of
    {ok, NodeInfo} ->
      {NodeInfo#node_info.ip, NodeInfo#node_info.port};
    error ->
      {not_found}
  end.

find_node(Table, TargetId) ->
  BucketIndex = get_bucket_index(Table, TargetId),
  Bucket = lists:nth(BucketIndex, Table#dht_routing_table.buckets),
  Nodes = dict:to_list(Bucket),
  lists:sort(fun({_, Node1}, {_, Node2}) -> dht_utils:distance(TargetId, Node1#node_info.id) < dht_utils:distance(TargetId, Node2#node_info.id) end, Nodes).

find_closest_nodes(Table, TargetId) ->
  AllNodes = get_all_nodes(Table),
  SortedNodes = lists:sort(fun({_, Node1}, {_, Node2}) ->
    dht_utils:distance(TargetId, Node1#node_info.id) < dht_utils:distance(TargetId, Node2#node_info.id) end, AllNodes),
  lists:sublist(SortedNodes, 1, 3).

get_bucket_index(Table, Id) ->
  Distance = dht_utils:distance(Table#dht_routing_table.node_id, Id),
  BucketSize = 20,
  Index = 160 - trunc(math:log2(Distance + 1)),
  Index div BucketSize + 1.

get_all_nodes(Table) ->
  lists:flatten([dict:to_list(Bucket) || Bucket <- Table#dht_routing_table.buckets]).

replace_element(Index, NewElement, List) ->
  replace_element(Index, NewElement, List, []).

replace_element(_, _, [], Acc) ->
  lists:reverse(Acc);
replace_element(1, NewElement, [_|T], Acc) ->
  lists:reverse([NewElement|T] ++ Acc);
replace_element(Index, NewElement, [H|T], Acc) ->
  replace_element(Index - 1, NewElement, T, [H|Acc]).

get_udp_port() ->
  {ok, Socket} = gen_udp:open(0, []),
  {ok, Port} = inet:sockname(Socket),
  gen_udp:close(Socket),
  TailPort = element(2, Port),
  TailPort.

get_own_ip() ->
  {ok, IfList} = inet:getif(),
  {IP, _, _} = hd(IfList),
  IP.