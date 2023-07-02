-module(dht_routing_table).
-include("dht_routing_table.hrl").

-export([init/1, remove_node/2, get_node_info/1, get_node_info_by_id/2, get_all_nodes/1, find_node/2, find_closest_nodes/2, add_node/2, get_own_ip/0, get_udp_port/0]).

-define(BUCKET_SIZE, 20).

init(NodeId) ->
  Buckets = lists:foldl(fun(_, Acc) -> [dict:new() | Acc] end, [], lists:seq(1, 160 div ?BUCKET_SIZE)),
  #dht_routing_table{node_id = NodeId, buckets = Buckets}.

add_node(#dht_routing_table{node_id = NodeId, buckets = Buckets} = Table, NodeInfo) ->
  io:format("Table Info Id: ~p~n", [Table#dht_routing_table.node_id]),
  io:format("Node Info Id: ~p~n", [NodeInfo#node_info.id]),
  BucketIndex = get_bucket_index(Table, NodeId),
  Bucket = lists:nth(BucketIndex, Buckets),
  UpdatedBucket = replace_node(Bucket, NodeInfo),
  UpdatedBuckets = replace_element(BucketIndex, UpdatedBucket, Buckets),
  io:format("UpdatedBuckets: ~p~n", [Table#dht_routing_table.buckets]),
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
  io:format("Table Info: ~p~n", [Table]),
  {Table#dht_routing_table.node_id, self(), get_udp_port()}.

get_node_info_by_id(Table, NodeId) ->
  io:format("Node Info ID: ~p~n", [NodeId]),
  BucketIndex = get_bucket_index(Table, NodeId),
  Bucket = lists:nth(BucketIndex, Table#dht_routing_table.buckets),
  io:format("Bucket Info: ~p~n", [Bucket]),
  io:format("Table Info: ~p~n", [Table]),
  case dict:find(NodeId, Bucket) of
    {ok, NodeInfo} ->
      io:format("NodeInfoOK: ~p~n", [NodeInfo]),
      {NodeInfo#node_info.ip, NodeInfo#node_info.port};
    error ->
      io:format("Error"),
      {not_found}
  end.

find_node(Table, TargetId) ->
  io:format("Table ID: ~p~n", [Table#dht_routing_table.node_id]),
  io:format("TargetID ID: ~p~n", [TargetId]),
  BucketIndex = get_bucket_index(Table, TargetId),
  Bucket = lists:nth(BucketIndex, Table#dht_routing_table.buckets),
  Nodes = dict:to_list(Bucket),
  lists:sort(fun({_, Node1}, {_, Node2}) -> dht_utils:distance(TargetId, Node1#node_info.id) < dht_utils:distance(TargetId, Node2#node_info.id) end, Nodes).

find_closest_nodes(Table, TargetId) ->
  AllNodes = get_all_nodes(Table),
  io:format("All nodes: ~p~n", [AllNodes]),
  io:format("Table All nodes: ~p~n", [Table]),
  io:format("Target All nodes: ~p~n", [TargetId]),
  SortedNodes = lists:sort(fun({_, Node1}, {_, Node2}) ->
    io:format("Node1: ~p~n", [Node1]),
    io:format("Node2: ~p~n", [Node2]),
    dht_utils:distance(TargetId, Node1#node_info.id) < dht_utils:distance(TargetId, Node2#node_info.id) end, AllNodes),
  lists:sublist(SortedNodes, 1, 3).

get_bucket_index(Table, Id) ->
  io:format("ID1: ~p~n", [Table#dht_routing_table.node_id]),
  io:format("ID2: ~p~n", [Id]),
  Distance = dht_utils:distance(Table#dht_routing_table.node_id, Id),
  BucketSize = 20,
  Index = 160 - trunc(math:log2(Distance + 1)),
  Index div BucketSize + 1.

get_all_nodes(Table) ->
  io:format("Table Buckets: ~p~n", [Table]),
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
  io:format("Porta macchina: ~p~n", [TailPort]),
  TailPort.

get_own_ip() ->
  {ok, IfList} = inet:getif(),
  {IP, _, _} = hd(IfList),
  io:format("IP macchina: ~p~n", [IP]),
  IP.

%% get_udp_port() ->
% {12345}.

%% get_own_ip() ->
%  {127,0,0,1}.
