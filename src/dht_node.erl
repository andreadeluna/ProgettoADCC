-module(dht_node).
-include("dht_node.hrl").

-define(PING_TIMEOUT, 15000). % Timeout per il messaggio PING (in millisecondi)
-define(NODE_TIMEOUT, 10000). % Timeout per il nodo inattivo (in millisecondi)

start(NodeId) ->
  RoutingTable = dht_routing_table:init(NodeId),
  Datastore = dht_datastore:init(),
  Socket = gen_udp:open(0, [{active, true}]),
  Node = #node{node_id = NodeId, routing_table = RoutingTable, datastore = Datastore, socket = Socket, active_nodes = []},
  NodeInfo = #node_info{id = NodeId, ip = dht_routing_table:get_own_ip(), port = dht_routing_table:get_udp_port()},
  UpdatedRoutingTable = dht_routing_table:add_node(RoutingTable, NodeInfo),
  UpdatedNode = Node#node{routing_table = UpdatedRoutingTable},
  spawn(fun() -> listen(UpdatedNode) end),
  UpdatedNode.

join_network(NewNode, ExistingNode) ->
  ExistingNodeInfo = #node_info{id = ExistingNode#node.node_id, ip = dht_routing_table:get_own_ip(), port = dht_routing_table:get_udp_port()},
  NewNodeInfo = #node_info{id = NewNode#node.node_id, ip = dht_routing_table:get_own_ip(), port = dht_routing_table:get_udp_port()},
  ExistingNodeUpdated = ExistingNode#node{
    node_id = ExistingNode#node.node_id,
    routing_table = dht_routing_table:add_node(ExistingNode#node.routing_table, NewNodeInfo),
    datastore = ExistingNode#node.datastore,
    socket = ExistingNode#node.socket,
    active_nodes = ExistingNode#node.active_nodes
  },
  NewNodeUpdated = NewNode#node{
    node_id = NewNode#node.node_id,
    routing_table = dht_routing_table:add_node(NewNode#node.routing_table, ExistingNodeInfo),
    datastore = NewNode#node.datastore,
    socket = NewNode#node.socket,
    active_nodes = NewNode#node.active_nodes
  },
  {ExistingNodeUpdated, NewNodeUpdated}.

lookup_node(Node, TargetId) ->
  case dht_routing_table:find_node(Node, TargetId) of
    [] ->
      {error, not_found};
    [NodeInfo|_] ->
      lookup_node_recursive(Node, NodeInfo)
  end.

lookup_node_recursive(Node, NodeInfo) ->
  case ping(Node, NodeInfo) of
    ok ->
      handle_active_node(Node, NodeInfo),
      {ok, NodeInfo};
    {error, timeout} ->
      handle_inactive_node(Node, NodeInfo),
      case dht_routing_table:find_closest_nodes(Node, NodeInfo) of
        [] ->
          {error, not_found};
        [NextNode|_] ->
          lookup_node_recursive(Node, NextNode)
      end
  end.