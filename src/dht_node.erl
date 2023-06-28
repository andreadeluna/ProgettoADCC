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

find_node(Node, TargetId) ->
  Nodes = dht_routing_table:find_closest_nodes(Node#node.routing_table, TargetId),
  Reply = {node_reply, Nodes},
  send_message(Node, Reply, TargetId).

find_value(Node, Key) ->
  case dht_datastore:lookup(Node, Key) of
    {ok, Value} ->
      Reply = {value_reply, Key, Value},
      send_message(Node, Key, Reply);
    error ->
      Nodes = dht_routing_table:find_closest_nodes(Node, Key),
      Reply = {node_reply, Nodes},
      send_message(Node, Key, Reply)
  end.

store(Node, Key, Value) ->
  NewDatastore = dht_datastore:store(Node#node.datastore, Key, Value),
  NewNode = Node#node{
    node_id = Node#node.node_id,
    routing_table = Node#node.routing_table,
    datastore = NewDatastore,
    socket = Node#node.socket,
    active_nodes = Node#node.active_nodes
  },
  NewNode.

handle_active_node(Node, NodeInfo) ->
  ActiveNodes = [{NodeInfo, erlang:system_time(millisecond)}],
  Node#node{active_nodes = ActiveNodes}.

handle_inactive_node(Node, _) ->
  Node#node{active_nodes = []}.

update_active_node(Node, NodeId) ->
  ActiveNodes = [{NodeId, erlang:system_time(millisecond)}],
  Node#node{active_nodes = ActiveNodes}.

send_message(Node, Message, TargetId) ->
  {_, _, NodePort} = dht_routing_table:get_node_info(Node#node.routing_table),
  TargetNodeInfo = dht_routing_table:get_node_info_by_id(Node#node.routing_table, TargetId#node.node_id),
  {_, Socket} = Node#node.socket,
  case TargetNodeInfo of
    {TargetIp, _} ->
      gen_udp:send(Socket, TargetIp, NodePort, term_to_binary(Message)),
      ok;
    _ ->
      io:format("Nodo non trovato") % Gestisci l'errore di nodo di destinazione non trovato come desiderato
  end.

listen(Node) ->
  receive
    {udp, _, _, _, BinMsg} ->
      Message = binary_to_term(BinMsg),
      handle_message(Node, Message)
  end,
  listen(Node).

handle_message(Node, {ping, SenderId}) ->
  Reply = {pong, Node},
  send_message(Node, SenderId, Reply);

handle_message(Node, {store, Key, Value}) ->
  dht_datastore:store(Node, Key, Value);

handle_message(Node, {find_node, TargetId}) ->
  Nodes = dht_routing_table:find_closest_nodes(Node, TargetId),
  Reply = {node_reply, Nodes},
  send_message(Node, TargetId, Reply);

handle_message(Node, {find_value, Key}) ->
  case dht_datastore:lookup(Node, Key) of
    {ok, Value} ->
      Reply = {value_reply, Key, Value},
      send_message(Node, Key, Reply);
    error ->
      Nodes = dht_routing_table:find_closest_nodes(Node, Key),
      Reply = {node_reply, Nodes},
      send_message(Node, Key, Reply)
  end;

handle_message(Node, {pong, TargetId}) ->
  update_active_node(Node, TargetId),
  ok.