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